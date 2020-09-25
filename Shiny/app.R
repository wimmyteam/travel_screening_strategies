library(shiny)
library(tidyverse)
library(scales)

source("functions/analysis.R")
n_sims = 10000

# Define UI for application t
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Peru model",br(),
  fluidRow(column(12,
                  p("This is an interactive web application to visualize summary metrics for the Peru model on strategies to reduce
                    the risk of SARS-Cov2 re-introduction from infected travellers."))),
  h3("Model input"),br(),
  fluidRow(column(4,
                  wellPanel(
                    sliderInput(inputId = "quarentine_days",
                                label = "number of days in quarantine",
                                min = 0, max = 10, value = 3, step = 1),
                    sliderInput(inputId = "percent_compliant",
                                label = "compliance (home quarantine)",
                                min = 0, max = 100, value = 80, step = 20),
                    actionButton(inputId = "go",
                                 label = "Run simulation"))),
           column(8,
                  p("Text explaining key words of the model and the simulation process"))
           ),
  h3("Model output"),
  br(),
  fluidRow(
    column(4, tags$h4("Number of infectious days"),
           plotOutput(outputId = "stat1")),
    column(3, tags$h4("Relative reduction"),
           verbatimTextOutput(outputId = "stat2")),
    column(4, tags$h4("Averted infectious days"),
           plotOutput(outputId = "stat3"))
  )),
  tabPanel("Underlying assumptions",
           fluidRow(br(),
             p("This model was adapted from", tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.07.24.20161281v2", "Clifford et al. (2020)."),
               "The model code lives in",tags$a(href = "https://github.com/wimmyteam/travel_screening_strategies", "GitHub."))),
           br(),
           div(img(src = "kucirka_plot.png", 
                   
                   width="800", 
                   height="450"), style="text-align: left;"),
           p("Figure 1: Traveller PCR sensitivity curve, obtained by fitting a Binomial GAM to the data collated in", 
             tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7240870/","Kucirka et al. (2020)."),
           "The mean fit is used as the time-varying sensitivity function, P(t), and hence no uncertainty is shown in the figure.")
           )
))

# Define server logic required 
server <- function(input, output) {
  
  managed1 <- eventReactive(input$go, {
    managed_quarentine_results <- read_rds("data/managed.RDS") 
    inf_days_summary(managed_quarentine_results, n_sims = n_sims) %>% 
    mutate(Scenario = "managed")})
  
    home1 <- eventReactive(input$go, {
      home_quarentine_results <- readRDS("data/home.RDS") 
      inf_days_summary(home_quarentine_results, n_sims = n_sims) %>% 
      mutate(Scenario = "home")
    })
  
  dat <- eventReactive(input$go, {bind_rows(managed1(),home1())})
  
  dat2 <- eventReactive(input$go, {
    dat() %>% 
      pivot_wider(names_from = Scenario, values_from = 2:4) %>% 
      mutate(diff_home_managed = days_released_inf_per_traveller_home - days_released_inf_per_traveller_managed)
    })
  
  scenario_means <- eventReactive(input$go, {dat() %>% 
    group_by(Scenario) %>%
    summarise(xvalue=mean(days_released_inf_per_traveller))
  })
  
  output$stat1 <- renderPlot({
    plot_hist1(dat(), scenario_means())
    })
  
  output$stat2 <- renderPrint({
    as.numeric(scenario_means()[1,2])/as.numeric(scenario_means()[2,2])
  })
  
  output$stat3 <- renderPlot({
    dat2() %>% 
      ggplot(aes(x = diff_home_managed))+
      geom_histogram(alpha=0.2) +
      scale_y_log10()+
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)