
inf_days_summary <- function(results, sims) {
  summary_stats <- results %>% 
    mutate(days_released_inf = if_else(is.na(days_released_inf), 0, days_released_inf)) %>% 
    group_by(sim, percent_compliant) %>% 
    summarise(sum_days_released_inf = sum(days_released_inf),
              trav_vol = first(trav_vol)) %>% 
    mutate(days_released_inf_per_traveller = (sum_days_released_inf/trav_vol)*1000) %>% 
    ungroup() %>% 
    full_join(y = sims) %>% 
    mutate(days_released_inf_per_traveller = if_else(is.na(days_released_inf_per_traveller), 
                                                     0, days_released_inf_per_traveller)) %>% 
    mutate(percent_compliant = as.factor(percent_compliant))
  
  return(summary_stats)
}

plot_hist1 <- function(dat, scenario_means){
  dat %>% 
    ggplot(aes(x = days_released_inf_per_traveller, fill = percent_compliant))+
    geom_histogram(alpha=0.4, position = 'identity') +
    geom_vline(data = scenario_means, aes(xintercept = xvalue, color = percent_compliant), size =1)+
    scale_y_log10(oob = scales::squish_infinite)+
    theme_bw()+
    labs(x = "Number of days of infectiousness remaining",
         y = "Simulations")
}

released_inf_trav_summary <- function(results, n_sims = 10000) {
  sims = tibble(sim = (1:n_sims))
  summary_stats <- results %>%
    filter(stage_released == "Infectious") %>%
    group_by(sim) %>%
    summarise(released_infectious_travellers = n()) %>%
    full_join(y = sims) %>%
    mutate(released_infectious_travellers = ifelse(is.na(released_infectious_travellers), 
                                                   0, released_infectious_travellers)) %>% 
    ggplot(aes(released_infectious_travellers))+
    geom_histogram()+
    labs(x = "Number of infectious travellers released")+
    geom_vline(aes(xintercept = mean(released_infectious_travellers)), 
               color = "red",
               size=1)+
    theme_bw()+
    scale_y_log10()
    
  return(summary_stats)
}

