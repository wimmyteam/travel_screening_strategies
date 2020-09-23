library(shiny)
library(tidyverse)

source("functions/analysis.R")
n_sims = 10000

# Define UI for application t
ui <- fluidPage(
  titlePanel("Peru model"),
  fluidRow(column(12,
                  p("This is an interactive web application to visualize the number of days of infectiousness remaining per traveller.", 
                    "This model was adpted from Clifford et al."),
                  p("The model code lives in", tags$a(href = "https://github.com/wimmyteam/travel_screening_strategies", "GitHub.")))),
  tags$hr(), 
  fluidRow(column(5,
                  wellPanel(
                    sliderInput(inputId = "syndromic_sensitivity",
                                label = "syndromic sensitivity",
                                min = 0.3, max = 0.7, value = 0.7, step = 0.2),
                    sliderInput(inputId = "quarentine_days",
                                label = "number of days in quarantine",
                                min = 0, max = 12, value = 3, step = 3),
                    sliderInput(inputId = "percent_compliant",
                                label = "compliance (Home quarantine)",
                                min = 0, max = 100, value = 80, step = 20),
                    actionButton(inputId = "go",
                                 label = "run simulation"))),column(1),
           column(6, div(img(src = "kucirka_plot.png", 
                             width="600", 
                             height="350"), style="text-align: center;"),
                  p("Traveller PCR sensitivity curve, obtained by fitting a Binomial GAM to the data collated in Kucirka et al. (2020).
           The mean fit is used as the time-varying sensitivity function, P(t), and hence no uncertainty is shown in the figure."))),
  fluidRow(
    column(6, tags$h4("Managed Quarantine"),
           h6("Number of infectious travellers released per 1000 travellers"),
           verbatimTextOutput(outputId = "stats1"),
           h6("Number of infectious days per 1000 travellers"),
           verbatimTextOutput(outputId = "stats2")),
    column(6, tags$h4("Home Quarantine"),
           h6("Number of infectious travellers released per 1000 travellers"),
           verbatimTextOutput(outputId = "stats3"),
           h6("Number of infectious days per 1000 travellers"),
           verbatimTextOutput(outputId = "stats4"))
  )
)

# Define server logic required 
server <- function(input, output) {
  
  managed_quarentine_results <- eventReactive(input$go, {read_rds("data/managed.RDS") #%>% 
      #filter(syndromic_sensitivity == input$syndromic_sensitivity,
      #       quarentine_days == input$quarentine_days)
    })
  
  home_quarentine_results <- eventReactive(input$go, {readRDS("data/home.RDS") #%>% 
     #filter(syndromic_sensitivity == input$syndromic_sensitivity,
     #        quarentine_days == input$quarentine_days,
     #        percent_compliant == input$percent_compliant)
    })
  
  output$stats1 <- renderPrint({
    released_inf_trav_summary(managed_quarentine_results(), n_sims = n_sims)
    })
  
  output$stats2 <- renderPrint({
    inf_days_summary(managed_quarentine_results(), n_sims = n_sims)
    })
  
  output$stats3 <- renderPrint({
    released_inf_trav_summary(home_quarentine_results(), n_sims = n_sims)
    })
  
  output$stats4 <- renderPrint({
    inf_days_summary(home_quarentine_results(), n_sims = n_sims)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)