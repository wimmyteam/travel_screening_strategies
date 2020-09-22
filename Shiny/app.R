library(shiny)

# Define UI for application t
ui <- fluidPage(
  titlePanel("Peru model outuput"),
  fluidRow(column(12,
p("This is an interactive web application to visualize the number of days of infectiousness remaining after release.", 
"This model was adpted from Clifford et al and implemented by the team from", tags$a(href = "https://www.wimmy.co.za", "Wimmy.")),
p("The model code lives in", tags$a(href = "https://github.com/wimmyteam/travel_screening_strategies", "GitHub.")))),
  tags$hr(), 
  fluidRow(column(4),column(4,
  wellPanel(
  sliderInput(inputId = "num",
              label = "choose a number",
              min = 1, max = 1000, value = 50),
  sliderInput(inputId = "num2",
              label = "Prevalence",
              min = 0, max = 10, value = 1),
  sliderInput(inputId = "num3",
              label = "Lenght of isolation (days)",
              min = 0, max = 14, value = 3),
  sliderInput(inputId = "num4",
              label = "Compliance (percentage)",
              min = 0, max = 100, value = 80),
  textInput(inputId = "title",
            label = "write the histogram title",
            value = "A histogram of random numbers")
  )),column(4)),
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
  output$hist <- renderPlot({hist(data(),main = input$title)})
  output$stats <- renderPrint({summary(data())})
}

# Run the application 
shinyApp(ui = ui, server = server)
