source("packages.R")

if (Sys.info()["nodename"] == "Sams-MacBook-Pro.local"){
  future::plan(multicore)
} else {
  future::plan(multiprocess)
  options(future.globals.maxSize = 2000*1024^2 )
}

source("utils.R")

run_analysis <- 
  function(n_arrival_sims  = 1000,
           countries       = c("EU", "USA"),
           trav_vol_manual = NULL,
           n_travellers    = 1e4,
           trav_vol_p      = 1,
           fixed           = TRUE,
           seed            = 145){
    
    set.seed(seed)
    #Parameters
    
    incubation_times <- make_incubation_times(
      n_travellers = n_travellers,
      pathogen     = pathogen,
      syndromic_sensitivity = unique(input$syndromic_sensitivity))
    
    # Infected arrivals
    
    inf_arrivals <- make_inf_arrivals(
      countries       = countries,
      prev_est_region = prev_est_region,
      n_arrival_sims  = n_arrival_sims,
      asymp_fraction  = asymp_fraction,
      trav_vol_p      = trav_vol_p,
      flight_vols     = flight_vols,
      flight_times    = flight_times,
      trav_vol_manual = trav_vol_manual,
      incubation_times = incubation_times,
      fixed            = fixed)
    
    
    # Cross arrivals with scenarios
    arrival_scenarios <- make_arrival_scenarios(input, inf_arrivals, incubation_times)
    
    # Calculate when released
    arrival_released <- when_released(arrival_scenarios)
    
    # Calculate stage of infectiousness when released
    arrival_released_times <- stage_when_released(arrival_released)
    
    
    
    return(arrival_released_times)
    
  }

# baseline for comparison, least stringent
baseline_low <- data.frame(
  pre_board_screening   = NA,
  post_flight_screening = FALSE,
  first_test_delay      = 0,
  second_test_delay     = NA,
  stringency            = "low"
)

# baseline for comparison, most stringent but no test
baseline_high <- data.frame(
  pre_board_screening   = NA,
  post_flight_screening = TRUE,
  first_test_delay      = 14,
  second_test_delay     = NA,
  stringency            = "maximum"
)

run_rr_analysis <- 
  function(arrival_released_times,
           main_scenarios,
           baseline_scenario,
           text_size = 2.5,
           faceting = country ~ stringency){
    
    set.seed(145)
    
    #Parameters
    
    baseline <- inner_join(baseline_scenario, input)
    
    # arrival_released_times_summaries <- 
    #   make_arrival_released_quantiles(arrival_released_times, "country")
    
    stringencies <- distinct(arrival_released_times, stringency, scenario)
    
    if(!("pre_board_screening" %in% all.vars(faceting))){
      arrival_released_times <- filter(arrival_released_times,
                                       is.na(pre_board_screening))
    }
    
    arrival_released_times_summaries <- 
      mutate(arrival_released_times, 
             time_in_iso = released_t - flight_arrival) %>% 
      count(   stage_released, released_test, sim, scenario, country) %>%
      complete(stage_released, released_test, sim, scenario, country) %>% 
      mutate(n = replace_na(n, 0))
    
    
    baseline_summaries <- 
      inner_join(arrival_released_times_summaries,
                 baseline) %>% 
      filter(stage_released=="Infectious",
             grepl(x = released_test,
                   pattern ="Released after"),
             !grepl(x = released_test,
                    pattern = "\\+"))  %>%
      rename("baseline_n"             = n,
             "baseline_scenario"      = scenario,
             "baseline_released_test" = released_test,
             "baseline_stringency"    = stringency) %>%
      select(-contains("delay"), -pre_board_screening, -post_flight_screening,
             -pathogen, -syndromic_sensitivity)
    
    
    n_risk_ratios <- arrival_released_times_summaries %>% 
      filter(stage_released=="Infectious",
             grepl(x = released_test,
                   pattern ="Released after"),
             !grepl(x = released_test,
                    pattern = "\\+")) %>%
      inner_join(baseline_summaries) %>% 
      group_by_at(.vars = vars(sim, scenario, country,
                               #stringency,
                               one_of(names(baseline)) )) %>%
      summarise_at(.vars = vars(n, baseline_n),
                   .funs = sum) %>%
      mutate(ratio=(n)/(baseline_n)) %>% 
      replace_na(list(ratio=1)) %>% 
      nest(data = c(ratio, n, baseline_n, sim)) %>%
      mutate(Q = map(.x = data, ~quantile(.x$ratio, probs = probs)),
             M = map_dbl(.x = data, ~mean(.x$ratio))) %>%
      unnest_wider(Q) %>%
      select(-data) %>%
      inner_join(stringencies) %>%
      inner_join(input)
    
    ylabA <- sprintf("Ratio of infectious persons released in comparison to\n%s stringency, %i day quarantine, %s scenario",
                     baseline_scenario$stringency,
                     with(baseline_scenario,
                          first_test_delay + post_flight_screening + 
                            ifelse(is.na(second_test_delay), 0, second_test_delay),
                     ),
                     ifelse(is.na(baseline_scenario$pre_board_screening),
                            "no testing",
                            paste(baseline_scenario$pre_board_screening,
                                  "days pre-flight test")))
    
    rr_figs <-
      plot_data(input, n_risk_ratios, main_scenarios = NULL) %>%
      
      make_release_figure(
        x         = .,
        input     = input,
        text_size = text_size,
        xlab      = xlab,
        ylab      = ylabA, 
        faceting  = faceting)  
    
    
    file <- paste(names(baseline_scenario),
                  baseline_scenario, sep = "-", 
                  collapse = " ")
    
    list("png", "pdf") %>%
      map(~ggsave(filename = paste0("results/rr_figs_baseline_",
                                    file,".",.x),
                  plot=rr_figs,
                  width = 210, 
                  height = 70*nrow(distinct(ungroup(n_risk_ratios),
                                            !!lhs(faceting))), units="mm",
                  dpi = 320))
    
    #write.csv(n_fig_data,paste0("results/baseline_",baseline_scenario,"_n_RR_results.csv"))
    #write.csv(pd_fig_data,paste0("results/baseline_",baseline_scenario,"_pd_RR_results.csv"))
    
    return(list(arrival_released_times = n_risk_ratios#,
                #n_fig_data             = n_fig_data,
                #pd_fig_data            = pd_fig_data
    ))
  }

