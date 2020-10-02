
inf_days_summary <- function(results, sims) {
  summary_stats <- results %>% 
    mutate(days_released_inf_mod = if_else(is.na(days_released_inf_mod), 0, days_released_inf_mod)) %>% 
    group_by(sim, percent_compliant) %>% 
    summarise(sum_days_released_inf = sum(days_released_inf_mod),
              trav_vol = 1000) %>% 
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
    ggplot(aes(x = days_released_inf_per_traveller))+
    stat_bin(aes(color = percent_compliant), 
             geom="step",
             position = 'identity',
             size = 1,
             binwidth = 1) +
    geom_vline(data = scenario_means, aes(xintercept = xvalue, color = percent_compliant), size =1)+
    scale_y_log10(oob = scales::squish_infinite)+
    theme_bw(base_size = 12)+
    labs(x = "Number of remaining infectious days",
         y = "Simulations")+
    guides(fill=guide_legend(title="Scenario"),
           color=guide_legend(title="Scenario"))
}

# plot_hist1 <- function(dat, scenario_means){
#   dat %>% 
#     ggplot(aes(x = days_released_inf_per_traveller))+
#     geom_histogram(aes(color = percent_compliant), alpha=0.4, position = 'identity', fill=NA) +
#     geom_vline(data = scenario_means, aes(xintercept = xvalue, color = percent_compliant), size =1)+
#     scale_y_log10(oob = scales::squish_infinite)+
#     theme_bw(base_size = 12)+
#     labs(x = "Number of remaining infectious days",
#          y = "Simulations")+
#     guides(fill=guide_legend(title="Scenario"),
#            color=guide_legend(title="Scenario"))
# }

# plot_hist1 <- function(dat, scenario_means){
#   dat %>% 
#     ggplot(aes(x = days_released_inf_per_traveller))+
#     geom_histogram(aes(fill = percent_compliant), alpha=0.4, position = 'identity') +
#     geom_vline(data = scenario_means, aes(xintercept = xvalue, color = percent_compliant), size =1)+
#     scale_y_log10(oob = scales::squish_infinite)+
#     theme_bw(base_size = 12)+
#     labs(x = "Number of remaining infectious days",
#          y = "Simulations")+
#     guides(fill=guide_legend(title="Scenario"),
#            color=guide_legend(title="Scenario"))
# }

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

