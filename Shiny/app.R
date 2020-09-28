library(shiny)
library(tidyverse)
library(scales)

source("functions/analysis.R")
n_sims = 10000

#baseline <- read_rds("data/baseline_results.RDS") 

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
                                           min = 0, max = 100, value = 80, step = 20),
                               actionButton(inputId = "go",
                                            label = "Run simulation"))),
                      column(8,
                             p("This application shows summary metrics from 10 000 pre-run
                             model simulations for different strategies."), 
                             p("Employees quarantine either at home or in a hotel and test at the end of the quarantine period."),
                             tags$li("Hotel quarantine: Employees quarantine in a hotel and test
                                     after the quarantine period. Under this scenario, we assume that the employees are 100% compliant."),
                             tags$li("Home quarantine: Employees quarantine at
                                     home with imperfect compliance and test after the quarantine period."),
                             tags$li("Number of days in quarantine is the mandatory isolation period and ranges from 0
                                     to 10 days."),
                             tags$li("Compliance in this context is the probability of employees who
                             will adhere to the quarantine protocol.
                             How is it simulated?")
                             )
             ),
             h3("Model output"),
             br(),
             fluidRow(
               column(4, tags$h4("Number of infectious days"),
                      plotOutput(outputId = "stat1")),
               column(3, tags$h4("Relative reduction"),
                      verbatimTextOutput(outputId = "stat2"),
                      verbatimTextOutput(outputId = "stat3")),
               column(4, tags$h4("Averted infectious days"),
                      plotOutput(outputId = "stat4"))
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
  
  # managed1 <- eventReactive(input$go, {
  #   managed_quarentine_results <- read_rds("data/managed.RDS")
  #   inf_days_summary(managed_quarentine_results, n_sims = n_sims) %>%
  #   mutate(Scenario = "managed")})
  # 
  #   home1 <- eventReactive(input$go, {
  #     home_quarentine_results <- readRDS("data/home.RDS") 
  #     inf_days_summary(home_quarentine_results, n_sims = n_sims) %>% 
  #     mutate(Scenario = "home")
  #   })

  # dat <- eventReactive(input$go, {bind_rows(home(),managed())})
  
  sims <- eventReactive(input$go, {
    tibble(percent_compliant = rep(c(input$percent_compliant,100), n_sims)) %>% 
      group_by(percent_compliant) %>% 
      mutate(sim = row_number()) %>% 
      ungroup()
  })
  
  results <- eventReactive(input$go, {
    res <- read_rds("data/simulation_results.RDS") %>% 
      filter(quarantine_days == input$quarantine_days,
             percent_compliant %in% c(input$percent_compliant, 100))
    inf_days_summary(res, sims())})
  
  # dat <- eventReactive(input$go, {
  #   results() %>% 
  #     pivot_wider(names_from = percent_compliant, 
  #                 values_from = 3:5) %>% 
  #     mutate(diff = as.vector(.[6] - .[7]))
  #   })
  # 
  scenario_means <- eventReactive(input$go, {results() %>%
    group_by(percent_compliant) %>%
    summarise(xvalue=mean(days_released_inf_per_traveller)) %>% 
      mutate(percent_compliant = as.factor(percent_compliant))
  })

    
  output$stat1 <- renderPlot({
    plot_hist1(results(), scenario_means())
    })
  
  output$stat2 <- renderPrint({
    as.numeric(scenario_means()[1,2])/as.numeric(scenario_means()[2,2])
  })
  
  output$stat3 <- renderPrint({
    scenario_means()
  })
  
  # 
  # output$stat3 <- renderPlot({
  #   dat() %>% 
  #     ggplot(aes(x = diff_home_managed))+
  #     geom_histogram(alpha=0.4, fill = "#f8766d") +
  #     scale_y_log10()+
  #     theme_bw()+
  #     labs(y = "Simulations")
  #     
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)