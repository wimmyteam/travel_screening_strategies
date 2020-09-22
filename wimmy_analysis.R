
source('wimmy_functions.R')

td1_results <- run_scenario(
  prevalence               = 0.05,
  quarentine_days          = 1,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 1000,
  flight_time              = 2/24
)

scenario2 <- run_scenario(
  prevalence               = 0.05,
  quarentine_days          = 3,
  syndromic_sensitivity    = 0.7,
  n_travellers             = 1000,
  n_sims                   = 10000,
  flight_time              = 2/24
)

dat1 <- scenario2 %>% 
  group_by(sim) %>% 
  summarise(sum_days_released_inf = sum(days_released_inf, na.rm = T),
            trav_vol = first(trav_vol),
            prevented_boarding = sum(is.na(released_t)),
            released_travellers = trav_vol - prevented_boarding) %>% 
  mutate(days_released_inf_per_traveller = sum_days_released_inf/released_travellers)
