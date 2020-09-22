# Additional functions that facilitate more flexible 

# function to generate relevant clinical times for travellers
# The argument called pathogen is a list showing pathogen specific details for symptomatic and 
# assymptomatic cases. To generate the list you need gamma.parms.from.quantiles, a very long 
# function that we might not need to change since we are not interested in this specific 
# details for our scenario.

source("run_analysis_func.R")

# This function runs a single simulation
run_scenario <- function(
  prevalence               = 0.05,
  quarentine_days          = 3,
  quarentine_compliance    = 1.0,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 10,
  flight_time              = 2/24
)
{
  set.seed(145)
  incubation_times <- make_incubation_times(
    n_travellers = n_travellers,
    pathogen = pathogen, # pathogen created in Utils.R
    syndromic_sensitivity = syndromic_sensitivity
  )

  prev_vector <- rnorm(n_travellers, prevalence, 0.01) # Ideally we would get distribution from model prediction
  
  inf_arrivals <- make_inf_arrivals(
    prev_vector     = prev_vector,
    n_arrival_sims  = n_sims,
    asymp_fraction  = asymp_fraction, #Utils.R
    trav_vol        = n_travellers,
    flight_time     = flight_time,
    incubation_times = incubation_times,
    syndromic_sensitivity = syndromic_sensitivity
  )
  
  input <- 
    tibble(pathogen = "SARS-CoV-2") %>%
    mutate(syndromic_sensitivity = syndromic_sensitivity)  %>%
    bind_cols(., list(
      `only` = 
        crossing(pre_board_screening = c(NA),
                 post_flight_screening = c(TRUE),
                 first_test_delay = 3,
                 second_test_delay = 0)) %>%
        bind_rows(.id = "stringency")) %>% 
    crossing(max_mqp             = 14,
             post_symptom_window =  7,
             results_delay       =  1) %>%
    mutate(scenario=row_number())
  
  arrival_scenarios <- make_arrival_scenarios(
    input,
    inf_arrivals,
    incubation_times
  )
  
  arrival_released <- when_released(arrival_scenarios)
  
  # Calculate stage of infectiousness when released
  arrival_released_times <- stage_when_released(arrival_released)
  
  return(arrival_released_times)
}
