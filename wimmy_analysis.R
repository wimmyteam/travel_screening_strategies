source('wimmy_functions.R')

library(fitdistrplus)
library(lubridate)

n_sims <- 10000
n_travellers <- 1000
flight_time <- 2/24

# This fits gamma distribution very inexactly to quantiles taken from dashboard graphs
#prev_gamma_pars <- gamma.parms.from.quantiles(q = c(0.005, 0.008),
#                           p = c(0.5, 0.975))

peru_model_prevalence <- read_rds("data/peru_prevalence.rds") %>%
  filter(date == ymd("2020-10-26"))

fit.gamma <- fitdist(peru_model_prevalence$Prevalence, distr = "gamma", method = "mle")

set.seed(145)

prev_vector <- rgamma(n_sims, fit.gamma[["estimate"]][["shape"]], fit.gamma[["estimate"]][["rate"]])

saveRDS(tibble(prev_vector), 'Shiny/data/prevalence.rds')


slider_options <- list(
  syndromic_sensitivity = c(0.7),
  quarantine_days = c(0:10),
  percent_compliant = c(0, 20, 40, 60 ,80, 100)
)

option_combinations <- cross_df(slider_options)

for (i in 1:nrow(option_combinations)) {

  print(paste("progress: ", i, " out of ", nrow(option_combinations)))

  row <- option_combinations[i,]

  result <- run_partial_compliance_scenario(
      prev_vector              = prev_vector,
      quarantine_days          = row$quarantine_days,
      syndromic_sensitivity    = row$syndromic_sensitivity,
      n_travellers             = n_travellers,
      n_sims                   = n_sims,
      flight_time              = flight_time,
      percent_compliant        = row$percent_compliant
    )
  result <- result %>%
    select(sim, idx, type, released_test, released_t, days_released_inf, trav_vol, stage_released, pre_board_screening, first_test_delay, syndromic_sensitivity) %>%
    mutate(
      quarantine_days = row$quarantine_days,
      percent_compliant = row$percent_compliant
    )
  
  if (i == 1){
    combined_results <- result
  }
  else
    combined_results <- rbind(combined_results, result)
}

combined_results <- combined_results %>% 
  mutate(days_released_inf_mod = days_released_inf - 1,
         days_released_inf_mod = if_else(days_released_inf_mod < 0, 0, days_released_inf_mod))

saveRDS(combined_results, 'Shiny/data/simulation_results.rds')

baseline_strategy <- 
  tibble(
    pathogen = "SARS-CoV-2",
    syndromic_sensitivity  = 0.7,
    pre_board_screening    = NA,
    post_flight_screening  = FALSE,
    first_test_delay       = 0,
    second_test_delay      = NA,
    max_mqp                = 14,
    post_symptom_window    = 7,
    results_delay          = 1,
    scenario               = 1
  )

baseline_results <- run_scenario(
  strategy = baseline_strategy,
  prev_vector,
  syndromic_sensitivity    = 0.7,
  n_travellers             = n_travellers,
  n_sims                   = n_sims,
  flight_time              = flight_time
)

baseline_results <- baseline_results %>% 
  mutate(days_released_inf_mod = days_released_inf - 1,
         days_released_inf_mod = if_else(days_released_inf_mod < 0, 0, days_released_inf_mod))

saveRDS(baseline_results, 'Shiny/data/baseline_results.rds')

#managed_quarantine_results <- run_partial_compliance_scenario(
#  prev_vector               = prev_vector,
#  quarantine_days          = 9,
#  syndromic_sensitivity    = syndromic_sensitivity,
#  n_travellers             = 1000,
#  n_sims                   = n_sims,
#  flight_time              = 2/24,
#  percent_compliant        = 100 # percentage
#) %>% mutate(syndromic_sensitivity = syndromic_sensitivity,
#             quarantine_days = 9,
#             percent_compliant = 100)
