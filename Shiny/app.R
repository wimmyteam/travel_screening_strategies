library(shiny)
library(tidyverse)
library(scales)

source("functions/analysis.R")
n_sims = 10000

prevalence <- read_rds("data/prevalence.rds")

baseline <- read_rds("data/baseline_results.rds") %>%
  mutate(days_released_inf = if_else(is.na(days_released_inf), 0, days_released_inf)) %>%
  group_by(sim) %>%
  summarise(sum_days_released_inf = sum(days_released_inf),
            trav_vol = first(trav_vol)) %>%
  mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>%
  ungroup() %>%
  full_join(tibble(sim = 1:n_sims)) %>%
  mutate(days_released_inf_per_traveller = if_else(is.na(days_released_inf_per_traveller),
                                                   0, days_released_inf_per_traveller),
         percent_compliant = "baseline")  


# Define UI for application t
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Peru model",
             br(),
             fluidRow(column(12,
                             p("This interactive web application is used to visualize summary metrics from the Peru simulation model 
                             investigating strategies aimed at reduce the risk of SARS-Cov2 re-introduction from infected employees.")
                      )
             ),
             h3("Model input"),
             br(),
             fluidRow(column(4,
                             wellPanel(
                               sliderInput(inputId = "quarantine_days",
                                           label = "number of days in quarantine",
                                           min = 0, max = 10, value = 3, step = 1),
                               sliderInput(inputId = "percent_compliant",
                                           label = "compliance",
                                           min = 0, max = 80, value = 80, step = 20)
                               )),
                      column(8,
                             tags$li("This application shows summary metrics from 10 000 pre-run
                             model simulations for different strategies."), 
                             tags$li("Employees quarantine either at home or in a hotel and test at the end of the quarantine period."),
                             tags$li("Compliance in this context is the percentage of employees who
                             adhere to the quarantine protocol. When compliance is 100%, 
                                     employees adhere to all the quarantine protocols and this is
                                     equivalent to a managed isolation or quarantine."),
                             tags$li("Number of days in quarantine is the mandatory isolation period and ranges from 0
                                     to 10 days."),
                             tags$li("We also simulated a baseline scenario which assumes no-quarantine and no-testing.")
                             
                             )
             ),
             h3("Model output"),
             br(),
             fluidRow(
               column(4, tags$h4("Number of days of infectiousness remaining"),
                      plotOutput(outputId = "stat1"),
                      p("Number of days of infectiousness remaining per 1000 employees.
                        The vertical lines show the mean for the different scenarios.")),
               column(3, 
                      tags$h4 ("Mean days of infectiousness remaining"), 
                      tableOutput(outputId = "tab1"),
                      br(),
                      tags$h4("Relative reduction"),
                      textOutput(outputId = "stat2", inline = TRUE),
                      textOutput(outputId = "stat3"),
                      textOutput(outputId = "stat4")),
               column(4, tags$h4("Difference in days of infectiousness remaining"),
                      plotOutput(outputId = "stat6"))
             ),
             br(),br(),br()
    ),
    tabPanel("Underlying assumptions",
             fluidRow(column(12,
               br(),
               p("This model was adapted from", tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.07.24.20161281v2", "Clifford et al. (2020)."),
                  "The model code is available on",tags$a(href = "https://github.com/wimmyteam/travel_screening_strategies", "GitHub.")),
               br(),
               tags$li("PCR Sensitivity"),
               p("The PCR Sensitivity function is defined as the the probability of an infected employee testing positive 
                 by nasopharyngeal or throat swab (NTS) PCR on a given day and is modelled as a function of time."), 
               p("For this modelling study, we used PCR Sensitivity that was used in the Clifford et al. (2020) study."),
               div(img(src = "kucirka_plot.png", 
                       
                       width="800", 
                       height="450"), style="text-align: left;"),
               p("Figure 1: Time varying PCR sensitivity curve, obtained by fitting a Binomial GAM to the data collated in", 
                 tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7240870/","Kucirka et al. (2020)."),
                 "The mean fit is used as the time-varying sensitivity function, P(t), and hence no uncertainty is shown in the figure."),
               
               tags$li("Syndromic sensitivity"),
               p("This is the proportion of symptomatic persons who don't pass the thermal scan and questionnaire-based screening 
                 that is assumed to take place prior to the employee going into quarantine."),
               p("This proportion is assumed to be 0.7 for all the simulations in line with the Clifford et al. (2020) study."),
               
               tags$li("Prevalence"),
               p("We obtain the mean and the limits from our Covid model, then generate a shape and scale parameter for a gamma
                 distribition from which we sample the prevalence for each simulation"),
               plotOutput(outputId = "prevalence",
                          width="800", 
                          height="450"),
               p("Figure 2: The distribution of the sampled prevalence"),
               
               tags$li("Proportion of assymptomatic individuals")
               
               
             )
             ),
             br(),br(),br()
    )
  )
)

# Define server logic required 
server <- function(input, output) {
  
  # reactive expression
  percent_compliant <- reactive({
    input$percent_compliant
  })
  
  sims <- reactive({
    tibble(percent_compliant = rep(c(input$percent_compliant, 100), n_sims)) %>% 
      group_by(percent_compliant) %>% 
      mutate(sim = row_number()) %>% ungroup()
  })
  
  dat <- reactive({
    res <- read_rds("data/simulation_results.rds") %>% 
      filter(quarantine_days == input$quarantine_days,
             percent_compliant %in% c(input$percent_compliant, 100))
    inf_days_summary(res, sims())})
  
  results <- reactive({bind_rows(dat(),baseline) %>% 
      mutate(percent_compliant = factor(percent_compliant, levels=unique(percent_compliant)))
      })
  
  dat2 <- reactive({
    results() %>%
    select(sim, percent_compliant, days_released_inf_per_traveller) %>% 
    pivot_wider(names_from = percent_compliant,
                values_from = days_released_inf_per_traveller) %>%
      mutate(!!paste(percent_compliant(),"baseline", sep = "-") := .[[2]] - .[[4]],
             "100-baseline" = .[[3]] - .[[4]],
             !!paste(percent_compliant(),"100", sep = "-") := .[[2]] - .[[3]]) %>% 
      select(-c(2:4)) %>% 
    pivot_longer(!sim, names_to = "Scenario") %>% 
      mutate(Scenario = factor(Scenario, levels = unique(Scenario)))
  })
  
  scenario_means <- reactive({results() %>%
    group_by(percent_compliant) %>%
    summarise(xvalue=mean(days_released_inf_per_traveller))
  })

  output$stat1 <- renderPlot({
    plot_hist1(results(), scenario_means())
    })
  
  output$tab1 <- renderTable({
    scenario_means() %>% rename("Scenario" = percent_compliant,
                                "Mean" = xvalue)
  })
  
  output$stat2 <- renderText({
    rr1 <- round(as.numeric(scenario_means()[1,2])/as.numeric(scenario_means()[3,2]),2)
    paste(paste(percent_compliant(),"%", sep = ""), "compliant vs baseline scenario:", rr1)
  })

  output$stat3 <- renderText({
    rr2 <- round(as.numeric(scenario_means()[2,2])/as.numeric(scenario_means()[3,2]),2)
    paste("100% compliant vs baseline scenario:", rr2)
  })

  output$stat4 <- renderText({
    rr3 <- round(as.numeric(scenario_means()[1,2])/as.numeric(scenario_means()[2,2]),2)
    paste(paste(percent_compliant(),"%", sep = ""), "vs 100% compliant scenario:", rr3)
  })
  
  output$stat6 <- renderPlot({
    dat2() %>%
      ggplot(aes(x = value, fill = Scenario)) +
      geom_histogram(alpha = 0.4, position = 'identity') +
      scale_y_log10(oob = scales::squish_infinite) +
      theme_bw(base_size = 12) +
      labs(x = "Diffence in days of infectiousness remaining",
           y = "Simulations")
  })
  
  # distribution of the prevalence
  output$prevalence <- renderPlot({
    prevalence %>%
      ggplot(aes(x = prev_vector)) +
      geom_density(color = "#00BF6F", 
                   size = 1)+
      labs(x = "Prevalence")+
      theme_bw(base_size = 20)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)