
source('wimmy_functions.R')

prevalence <- 0.05
syndromic_sensitivity <- 0.7

managed_quarentine_results <- run_partial_compliance_scenario(
  prevalence               = prevalence,
  quarentine_days          = 3,
  syndromic_sensitivity    = syndromic_sensitivity,
  n_travellers             = 1000,
  flight_time              = 2/24,
  percent_compliant        = 100 # percentage
)

  
home_quarentine_results <- run_partial_compliance_scenario(
  prevalence               = prevalence,
  quarentine_days          = 3,
  syndromic_sensitivity    = syndromic_sensitivity,
  n_travellers             = 1000,
  n_sims                   = 1000,
  flight_time              = 2/24,
  percent_compliant        = 80 # percentage
)

home_quarentine_results %>% 
  group_by(sim) %>% 
  summarise(sum(days_released_inf, na.rm = T))
