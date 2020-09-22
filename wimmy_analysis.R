
source('wimmy_functions.R')

td1_results <- run_scenario(
  prevalence               = 0.05,
  quarentine_days          = 1,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 1000,
  flight_time              = 2/24
)

td3_results <- run_scenario(
  prevalence               = 0.05,
  quarentine_days          = 3,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 1000,
  flight_time              = 2/24
)

td3_results %>% 
  group_by(sim) %>% 
  summarise(sum(days_released_inf, na.rm = T))
