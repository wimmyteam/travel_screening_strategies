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
                    sliderInput(inputId = "syndromic_sensitivity",
                                label = "syndromic sensitivity",
                                min = 0, max = 1, value = 0.7),
                    sliderInput(inputId = "quarentine_days",
                                label = "number of days in quarantine",
                                min = 0, max = 14, value = 3),
                    sliderInput(inputId = "quarentine_compliance1",
                                label = "compliance (Managed quarantine)",
                                min = 0, max = 100, value = 100),
                    sliderInput(inputId = "quarentine_compliance2",
                                label = "compliance (Home quarantine)",
                                min = 0, max = 100, value = 80),
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
  
  managed_quarentine_results <- reactive({readRDS("data/managed.rds")})
  
  home_quarentine_results <- reactive({readRDS("data/home.rds")})
  
  output$stats1 <- renderPrint({managed_quarentine_results() %>%
    filter(stage_released == "Infectious") %>%
    group_by(sim) %>%
    summarise(released_infectious_travellers = n()) %>%
    full_join(y = sims) %>%
    mutate(released_infectious_travellers = ifelse(is.na(released_infectious_travellers), 0, released_infectious_travellers)) %>%
    summarise(mean = mean(released_infectious_travellers),
              median = median(released_infectious_travellers),
              min = min(released_infectious_travellers),
              max = max(released_infectious_travellers)) 
    })
  output$stats2 <- renderPrint({managed_quarentine_results() %>% 
      mutate(days_released_inf = if_else(is.na(days_released_inf), 0, days_released_inf)) %>% 
      group_by(sim) %>% 
      summarise(sum_days_released_inf = sum(days_released_inf),
                trav_vol = first(trav_vol)) %>% 
      mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>% 
      ungroup() %>% 
      full_join(y = sims) %>% 
      mutate(days_released_inf_per_traveller = if_else(is.na(days_released_inf_per_traveller), 0, days_released_inf_per_traveller)) %>% 
      summarise(mean = mean(days_released_inf_per_traveller),
                median = median(days_released_inf_per_traveller),
                min = min(days_released_inf_per_traveller),
                max = max(days_released_inf_per_traveller)) 
    })
  output$stats3 <- renderPrint({home_quarentine_results() %>% 
      filter(stage_released == "Infectious") %>%
      group_by(sim) %>%
      summarise(released_infectious_travellers = n()) %>%
      full_join(y = sims) %>%
      mutate(released_infectious_travellers = ifelse(is.na(released_infectious_travellers), 0, released_infectious_travellers)) %>%
      summarise(mean = mean(released_infectious_travellers),
                median = median(released_infectious_travellers),
                min = min(released_infectious_travellers),
                max = max(released_infectious_travellers)) 
    })
  output$stats4 <- renderPrint({home_quarentine_results() %>% 
      mutate(days_released_inf = if_else(is.na(days_released_inf), 0, days_released_inf)) %>% 
      group_by(sim) %>% 
      summarise(sum_days_released_inf = sum(days_released_inf),
                trav_vol = first(trav_vol)) %>% 
      mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>% 
      ungroup() %>% 
      full_join(y = tib1) %>% 
      mutate(days_released_inf_per_traveller = if_else(is.na(days_released_inf_per_traveller), 0, days_released_inf_per_traveller)) %>% 
      summarise(mean = mean(days_released_inf_per_traveller),
                median = median(days_released_inf_per_traveller),
                min = min(days_released_inf_per_traveller),
                max = max(days_released_inf_per_traveller)) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)