
inf_days_summary <- function(results, n_sims = 10000) {
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

released_inf_trav_summary <- function(results, n_sims = 10000) {
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
