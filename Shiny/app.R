library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)

source("functions/analysis.R")
n_sims = 10000

prevalence <- read_rds("data/prevalence.rds")

baseline <- read_rds("data/baseline_results.rds") %>%
  mutate(days_released_inf_mod = if_else(is.na(days_released_inf_mod), 0, days_released_inf_mod)) %>%
  group_by(sim) %>%
  summarise(sum_days_released_inf = sum(days_released_inf_mod),
            trav_vol = 1000) %>%
  mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>%
  ungroup() %>%
  full_join(tibble(sim = 1:n_sims)) %>%
  mutate(days_released_inf_per_traveller = if_else(is.na(days_released_inf_per_traveller),
                                                   0, days_released_inf_per_traveller),
         percent_compliant = "baseline")  


# Define UI for application 
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Peru Simulation Model"),
                fluidRow(column(12,
                                p("This web application is used to visualize the summary metrics from the Peru simulation model 
                             investigating strategies aimed at reduce the risk of SARS-Cov2 re-introduction from infected employees."),
                br())
                ),
  tabsetPanel(
    tabPanel("Summary metrics",
             h3("Model input"),
             br(),
             fluidRow(column(4,
                             wellPanel(
                               sliderInput(inputId = "quarantine_days",
                                           label = "Number of days in quarantine",
                                           min = 1, max = 10, value = 3, step = 1),
                               sliderInput(inputId = "percent_compliant",
                                           label = "Compliance (in %)",
                                           min = 0, max = 80, value = 80, step = 20)
                               )),
                      column(8,
                             tags$p("This application shows summary metrics from 10 000 run
                             model simulations for different strategies. We ran simulations with 1000 employees.
                             The employees quarantine either at home or in a hotel and get tested at the end of the quarantine period."),
                             
                             tags$p("The number of days in quarantine is the mandatory isolation period and ranges from 1
                                     to 10 days."),
                             
                             tags$p("Compliance in this context is the percentage of employees who adhere to the 
                             quarantine protocol. When compliance is 100%, 
                                     employees will adhere to all the quarantine protocols and this is
                                     equivalent to a managed isolation or quarantine in a hotel or another facility."),
                             
                             tags$p("We also simulated a baseline scenario which assumes no pre- or post-arrival screening 
                                     and no-quarantine. Under this scenario, employees get tested immediately on arrival.")
                             
                             )
             ),
             h3("Model output"),
             br(),
             fluidRow(
               column(4, tags$h4("Number of remaining infectious days"),
                      plotOutput(outputId = "stat1"),
                      p("Number of remaining infectious days per 1000 employees in different
                      scenarios.
                        The vertical lines show the mean number of remaining infectious days.")),
               column(3, 
                      tags$h4 ("Mean days of infectiousness remaining"), 
                      tableOutput(outputId = "tab1"),
                      br(),
                      tags$h4("Relative reduction"),
                      textOutput(outputId = "stat2", inline = TRUE),
                      textOutput(outputId = "stat3"),
                      textOutput(outputId = "stat4")),
               column(4, tags$h4("Difference in the number of remaining infectious days"),
                      plotOutput(outputId = "stat6"))
             ),
             br(),br(),br()
    ),
    tabPanel("Underlying assumptions",
             fluidRow(column(12,
               br(),
               p("This model was adapted from", tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.07.24.20161281v2", "Clifford et al. (2020).")),
               p("The code is available on",tags$a(href = "https://github.com/wimmyteam/travel_screening_strategies", "GitHub.")),
               br(),
               tags$b("PCR sensitivity"),
               p("The PCR sensitivity function is defined as the the probability of an infected employee testing positive 
                 by nasopharyngeal or throat swab (NTS) PCR on a given day and is modelled as a function of time. 
                 For this simulation exercise, we used test sensitivity similar to what Clifford et al. (2020) used."),
               
               div(img(src = "kucirka_plot.png", 
                       
                       width="800", 
                       height="450"), style="text-align: left;"),
               p("Figure 1: Time varying PCR sensitivity curve, obtained by fitting a Binomial GAM to the data collated in", 
                 tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7240870/","Kucirka et al. (2020)."),
                 "The mean fit is used as the time-varying sensitivity function, P(t), and hence no uncertainty is shown in the figure."),
               
               tags$b("Syndromic sensitivity"),
               p("This is the proportion of symptomatic persons who don't pass the thermal scan and questionnaire-based screening 
                 that is assumed to take place prior to the employee going into quarantine. We assumed
                 this proportion to be 0.7 for all the simulations similar to what Clifford et al. (2020) used."),
               
               tags$b("Prevalence"),
               p("We obtain the mean and the range of the prevalence of active infections from the", 
                 tags$a(href = "https://github.com/wdelva/AAcovid", "COVID-19 Surveillance Modelling study"), 
               "and then estimated corresponding shape and rate parameters for a gamma
                 distribition from which we sampled prevalence values for each simulation."),
               plotOutput(outputId = "prevalence",
                          width="800", 
                          height="450"),
               p("Figure 2: The distribution of the sampled prevalence.")
              
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
      mutate(percent_compliant = factor(percent_compliant, levels = c("baseline", input$percent_compliant, "100")))
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
    rr1 <- round(as.numeric(scenario_means()[2,2])/as.numeric(scenario_means()[1,2]),2)
    paste(paste(percent_compliant(),"%", sep = ""), "compliant vs baseline scenario:", rr1)
  })

  output$stat3 <- renderText({
    rr2 <- round(as.numeric(scenario_means()[3,2])/as.numeric(scenario_means()[1,2]),2)
    paste("100% compliant vs baseline scenario:", rr2)
  })

  output$stat4 <- renderText({
    rr3 <- round(as.numeric(scenario_means()[2,2])/as.numeric(scenario_means()[3,2]),2)
    paste(paste(percent_compliant(),"%", sep = ""), "vs 100% compliant scenario:", rr3)
  })
  
  output$stat6 <- renderPlot({
    dat2() %>%
      ggplot(aes(x = value)) +
      stat_bin(aes(color = Scenario), geom="step", position = 'identity', size = 1)+
      #geom_histogram(alpha = 0.4, position = 'identity') +
      scale_y_log10(oob = scales::squish_infinite) +
      theme_bw(base_size = 12) +
      labs(x = "Diffence in remaining infectious days",
           y = "Simulations")
  })
  
  # distribution of the prevalence
  output$prevalence <- renderPlot({
    prevalence %>%
      ggplot(aes(x = prev_vector)) +
      geom_density(color = "#00BF6F", 
                   size = 1)+
      labs(x = "Prevalence")+
      theme_bw(base_size = 19)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)