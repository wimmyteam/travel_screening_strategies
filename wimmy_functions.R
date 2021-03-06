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
  quarantine_days          = 3,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 10,
  flight_time              = 2/24,
  percent_compliant        = 80 # percentage
)
{
  n_compliant     = round(n_travellers * percent_compliant / 100)
  n_non_compliant = n_travellers - n_compliant
  
  if(percent_compliant != 0)
  {
    compliant_strategy <- 
      tibble(
        pathogen = "SARS-CoV-2",
        syndromic_sensitivity  = syndromic_sensitivity,
        pre_board_screening    = NA,
        post_flight_screening  = TRUE,
        first_test_delay       = quarantine_days,
        second_test_delay      = NA,
        max_mqp                = 14,
        post_symptom_window    = 7,
        results_delay          = 1,
        scenario               = 1
      )
  
    compliant_result <- run_scenario(
      strategy                 = compliant_strategy,
      prev_vector              = prev_vector,
      syndromic_sensitivity    = syndromic_sensitivity,
      n_travellers             = n_compliant,
      n_sims                   = n_sims,
      flight_time              = flight_time
    ) %>%
    mutate(compliant = TRUE)

    if(percent_compliant == 100)
    {
      return(compliant_result)
    }  
  }

  
  if (percent_compliant != 100)
  {
    non_compliant_strategy <- 
      tibble(
        pathogen = "SARS-CoV-2",
        syndromic_sensitivity  = syndromic_sensitivity,
        pre_board_screening    = NA,
        post_flight_screening  = TRUE,
        first_test_delay       = 0,
        second_test_delay      = NA,
        max_mqp                = 14,
        post_symptom_window    = 7,
        results_delay          = 1,
        scenario               = 1
      )
    
    non_compliant_result <- run_scenario(
      strategy                 = non_compliant_strategy,
      prev_vector              = prev_vector,
      syndromic_sensitivity    = syndromic_sensitivity,
      n_travellers             = n_non_compliant,
      n_sims                   = n_sims,
      flight_time              = flight_time
    ) %>%
    mutate(
      compliant = FALSE,
      idx = idx + n_compliant
    )
  }
  
  if(percent_compliant == 0)
  {
    return(non_compliant_result)
  }
  
  full_result = bind_rows(compliant_result, non_compliant_result)
  
  return(full_result)
}


inf_days_summary <- function(results, n_sims = 1000) {
  sims = tibble(sim = (1:n_sims))
  summary_stats <- results %>% 
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
  return(summary_stats)
}


released_inf_trav_summary <- function(results, n_sims = 1000) {
  # Should we also show this "per thousand travellers" ?
  sims = tibble(sim = (1:n_sims))
  summary_stats <- results %>%
    filter(stage_released == "Infectious") %>%
    group_by(sim) %>%
    summarise(released_infectious_travellers = n()) %>%
    full_join(y = sims) %>%
    mutate(released_infectious_travellers = ifelse(is.na(released_infectious_travellers), 0, released_infectious_travellers)) %>%
    summarise(mean = mean(released_infectious_travellers),
              median = median(released_infectious_travellers),
              min = min(released_infectious_travellers),
              max = max(released_infectious_travellers))
  return(summary_stats)
}


