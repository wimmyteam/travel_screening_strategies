source('wimmy_functions.R')

n_sims <- 1000
prev_gamma_pars <- gamma.parms.from.quantiles(q = c(0.005, 0.008),
                           p = c(0.5, 0.975))

prev_vector <- rgamma(n_sims, prev_gamma_pars[["shape"]], rate=prev_gamma_pars[["rate"]])

syndromic_sensitivity <- 0.7

managed_quarentine_results <- run_partial_compliance_scenario(
  prev_vector               = prev_vector,
  quarentine_days          = 3,
  syndromic_sensitivity    = syndromic_sensitivity,
  n_travellers             = 1000,
  n_sims                   = n_sims,
  flight_time              = 2/24,
  percent_compliant        = 100 # percentage
)


home_quarentine_results <- run_partial_compliance_scenario(
  prev_vector               = prev_vector,
  quarentine_days          = 3,
  syndromic_sensitivity    = syndromic_sensitivity,
  n_travellers             = 1000,
  n_sims                   = n_sims,
  flight_time              = 2/24,
  percent_compliant        = 80 # percentage
)

# number of infectious travellers released per week

dat1 <- managed_quarentine_results %>% 
  #filter(stage_released == "Infectious") %>% 
  group_by(sim, stage_released) %>% 
  summarise(released_travellers = n()) %>% 
  ungroup() %>%
  filter(stage_released == "Infectious") %>%
  right_join(managed_quarentine_results, by='sim') %>%
  summarise(mean = mean(released_travellers),
            median = median(released_travellers),
            min = min(released_travellers),
            max = max(released_travellers))

# number of days of infectiousness per released traveller
dat2 <- managed_quarentine_results %>% 
  filter(stage_released == "Infectious") %>% 
  group_by(sim) %>% 
  summarise(sum_days_released_inf = sum(days_released_inf, na.rm = T),
            trav_vol = first(trav_vol)) %>% 
  mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>% 
  ungroup() %>% 
  summarise(mean = mean(days_released_inf_per_traveller),
            median = median(days_released_inf_per_traveller),
            min = min(days_released_inf_per_traveller),
            max = max(days_released_inf_per_traveller))

# number of infectious travellers released per week

dat3 <- home_quarentine_results %>% 
  filter(stage_released == "Infectious") %>% 
  group_by(sim) %>% 
  summarise(released_travellers = n()) %>% 
  ungroup() %>% 
  summarise(mean = mean(released_travellers),
            median = median(released_travellers),
            min = min(released_travellers),
            max = max(released_travellers))

# number of days of infectiousness per released traveller
dat4 <- home_quarentine_results %>% 
  filter(stage_released == "Infectious") %>% 
  group_by(sim) %>% 
  summarise(sum_days_released_inf = sum(days_released_inf, na.rm = T),
            trav_vol = first(trav_vol)) %>% 
  mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>% 
  ungroup() %>% 
  summarise(mean = mean(days_released_inf_per_traveller),
            median = median(days_released_inf_per_traveller),
            min = min(days_released_inf_per_traveller),
            max = max(days_released_inf_per_traveller))
