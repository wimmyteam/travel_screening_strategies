# This file is used for scratching around in the code and exploring the data structures

# This script is not meant to be run from start to finish

source("wimmy_functions.R")

# incubation times

incubation_times <- make_incubation_times(
  n_travellers = 1000,
  pathogen = pathogen, # pathogen created in Utils.R
  syndromic_sensitivity = 0.7
)
# Result is 2000 objects of 19 variables. 1000 Symptomatic and 1000 Asymptomatic.
# Most columns are durations ie exposure_to_onset, onset_to_recovery etc.
# This function seems to build "random" symptomatic and asymptomatic "cases"


inf_arrivals <- make_inf_arrivals(
  countries       = c("Peru"),
  prev_est_region = prev_est_region,
  n_arrival_sims  = 1000,
  asymp_fraction  = asymp_fraction,
  flight_vols     = 1000,
  flight_times    = flight_times,
  trav_vol_manual = 2000,
  incubation_times = incubation_times,
  fixed            = TRUE)

input <- 
  tibble(pathogen = "SARS-CoV-2") %>%
  mutate(syndromic_sensitivity = 0.7)  %>%
  bind_cols(., list(
    `only` = 
      crossing(pre_board_screening = c(NA),
               post_flight_screening = c(TRUE),
               first_test_delay = 1,
               second_test_delay = 3)) %>%
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


# Call new function that runs a single scenario
# This one doesn't work yet :P
new_infections <- estimate_infectious_days_per_traveller(
  prevalence = 0.05,
  quarentine_days = 3,
  quarentine_compliance = 0.8,
  syndromatic_sensitivity = 0.7
)