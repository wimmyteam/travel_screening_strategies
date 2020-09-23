# Additional functions that facilitate more flexible 

# function to generate relevant clinical times for travellers
# The argument called pathogen is a list showing pathogen specific details for symptomatic and 
# assymptomatic cases. To generate the list you need gamma.parms.from.quantiles, a very long 
# function that we might not need to change since we are not interested in this specific 
# details for our scenario.

source("run_analysis_func.R")

# This function runs a single simulation
run_scenario <- function(
  strategy,
  prev_vector,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 10,
  flight_time              = 2/24
)
{
  set.seed(145)
  
  input <- strategy
  
  incubation_times <- make_incubation_times(
    n_travellers = n_travellers,
    pathogen = pathogen, # pathogen created in Utils.R
    syndromic_sensitivity = syndromic_sensitivity
  )
  # Ideally we would get distribution from model prediction
  
  inf_arrivals <- make_inf_arrivals(
    prev_vector     = prev_vector,
    n_arrival_sims  = n_sims,
    asymp_fraction  = asymp_fraction, #Utils.R
    trav_vol        = n_travellers,
    flight_time     = flight_time,
    incubation_times = incubation_times,
    syndromic_sensitivity = syndromic_sensitivity
  )

  arrival_scenarios <- make_arrival_scenarios(
    input,
    inf_arrivals,
    incubation_times
  )
  
  arrival_released <- when_released(arrival_scenarios)
  
  # Calculate stage of infectiousness when released
  arrival_released_times <- stage_when_released(arrival_released) %>%
    select(
      -one_of(c('travellers', 'data'))
    )
  
  return(arrival_released_times)
}

run_partial_compliance_scenario <- function(
  prev_vector,
  quarentine_days          = 3,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 10,
  flight_time              = 2/24,
  percent_compliant        = 80 # percentage
)
{
  n_compliant     = round(n_travellers * percent_compliant / 100)
  n_non_compliant = n_travellers - n_compliant
  
  compliant_strategy <- 
    tibble(
      pathogen = "SARS-CoV-2",
      syndromic_sensitivity  = syndromic_sensitivity,
      pre_board_screening    = NA,
      post_flight_screening  = TRUE,
      first_test_delay       = 0,
      second_test_delay      = quarentine_days,
      max_mqp                = 14,
      post_symptom_window    = 7,
      results_delay          = 1,
      scenario               = 1
    )
  
  compliant_result <- run_scenario(
    strategy                 = compliant_strategy,
    prev_vector               = prev_vector,
    syndromic_sensitivity    = syndromic_sensitivity,
    n_travellers             = n_compliant,
    n_sims                   = n_sims,
    flight_time              = flight_time
  ) %>%
    mutate(compliant = TRUE)
  
  if (percent_compliant == 100) {
    return(compliant_result)
  }
  
  non_compliant_strategy <- 
    tibble(
      pathogen = "SARS-CoV-2",
      syndromic_sensitivity  = syndromic_sensitivity,
      pre_board_screening    = quarentine_days,
      post_flight_screening  = TRUE,
      first_test_delay       = 0,
      second_test_delay      = NA,
      max_mqp                = 14,
      post_symptom_window    = 7,
      results_delay          = 1,
      scenario               = 1
    )
  
  non_compliant_result <-  run_scenario(
    strategy                 = non_compliant_strategy,
    prev_vector               = prev_vector,
    syndromic_sensitivity    = syndromic_sensitivity,
    n_travellers             = n_non_compliant,
    n_sims                   = n_sims,
    flight_time              = flight_time
  ) %>%
    mutate(
      compliant = FALSE,
      idx = idx + n_compliant
    )
  
  full_result = bind_rows(compliant_result, non_compliant_result)
  
  return(full_result)
}