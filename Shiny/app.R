library(shiny)

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
                    sliderInput(inputId = "num",
                                label = "choose a number",
                                min = 1, max = 1000, value = 50),
                    sliderInput(inputId = "prevalence",
                                label = "prevalence",
                                min = 0, max = 10, value = 1),
                    sliderInput(inputId = "syndromic_sensitivity",
                                label = "syndromic sensitivity",
                                min = 0, max = 1, value = 0.7),
                    sliderInput(inputId = "quarentine_days",
                                label = "number of days in quarantine",
                                min = 0, max = 14, value = 3),
                    sliderInput(inputId = "quarentine_compliance",
                                label = "compliance (percentage)",
                                min = 0, max = 100, value = 80))),column(1),
           column(6, div(img(src = "kucirka_plot.png", 
                             width="600", 
                             height="350"), style="text-align: center;"),
                  p("Traveller PCR sensitivity curve, obtained by fitting a Binomial GAM to the data collated in Kucirka et al. (2020).
           The mean fit is used as the time-varying sensitivity function, P(t), and hence no uncertainty is shown in the figure."))),
  fluidRow(
    column(6, tags$h4("Scenario 1"),
           plotOutput(outputId = "hist")),
    column(6, tags$h4("Scenario 2"),
           verbatimTextOutput(outputId = "stats"))
  )
)

# Define server logic required 
server <- function(input, output) {
  data <- reactive({rnorm(input$num)})
  output$hist <- renderPlot({hist(data())})
  output$stats <- renderPrint({summary(data())})
}

# Run the application 
shinyApp(ui = ui, server = server)