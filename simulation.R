# A script that contains a single simulation function
# It brings together several functions from utils.R

# function to generate relevant clinical times for travellers
# The argument called pathogen is a list showing pathogen specific details for symptomatic and 
# assymptomatic cases. To generate the list you need gamma.parms.from.quantiles, a very long 
# function that we might not need to change since we are not interested in this specific 
# details for our scenario.

make_incubation_times <- function(
  n_travellers,
  pathogen,
  syndromic_sensitivity = 0.7){
  
  incubation_times <- crossing(idx  = 1:n_travellers,
                               type = c("symptomatic",
                                        "asymptomatic") %>%
                                 factor(x = .,
                                        levels = .,
                                        ordered = T)) %>%
    split(.$type) %>%
    map2_df(.x = .,
            .y = pathogen,
            ~mutate(.x,
                    exp_to_onset   = time_to_event(n = n(),
                                                   mean = .y$mu_inc, 
                                                   var  = .y$sigma_inc),
                    onset_to_recov = time_to_event(n = n(),
                                                   mean = .y$mu_inf, 
                                                   var  = .y$sigma_inf))) 
  
  source("wolfel.R")
  source("he.R")
  # infectious period from time of onset to no longer infectious
  incubation_times %<>% 
    mutate(u = runif(n = nrow(.), 0.01, 0.99)) %>%
    mutate(inf_from_onset = 
             approx(x    = wolfel_pred$y, 
                    y    = wolfel_pred$day, 
                    xout = u)$y,
           pre_symp_lead  = 
             approx(x    = HE$p,
                    y    = HE$delay,
                    xout = pmin(1 - 1e-5,
                                pmax(1e-5,
                                     pgamma(q = exp_to_onset,
                                            shape = inc_parms$shape,
                                            scale = inc_parms$scale))))$y
    )
  
  incubation_times %<>% 
    mutate(onset     = exp_to_onset,
           inf_start = onset - pre_symp_lead,
           inf_end   = ifelse(type == "asymptomatic",
                              exp_to_onset + onset_to_recov,
                              exp_to_onset + inf_from_onset),
           symp_end  = ifelse(type == "asymptomatic",
                              onset, # but really never matters because asymptomatics are never symptomatic!
                              exp_to_onset + onset_to_recov),
           inf_dur   = inf_end - inf_start,
           symp_dur  = symp_end - onset)
  
  # add flight
  incubation_times %<>% 
    mutate(flight_departure = runif(n = nrow(.),
                                    min = 0,
                                    max = onset + onset_to_recov)) %>%
    mutate(symp_screen_label      = 
             flight_departure > onset &
             flight_departure < symp_end &
             runif(n = nrow(.)) < syndromic_sensitivity)
  
  incubation_times %<>% gen_screening_draws
  
  return(incubation_times)
  
}


# function to make the infectious arrivals given traveller volumes
# calls make_prevalence() and make_travellers()

make_inf_arrivals <- function(countries,
                              prev_est_region,
                              n_arrival_sims,
                              asymp_fraction,
                              flight_vols = NULL,
                              trav_vol_manual = NULL,
                              trav_vol_p = 1,
                              flight_times,
                              incubation_times,
                              fixed = FALSE){
  
  
  inf_arrivals <- as.list(countries) %>%
    purrr::set_names(., .) %>%
    purrr::map(~make_prevalence(prev_est_region = prev_est_region,
                                origin_country = .x, 
                                n = n_arrival_sims)) %>%
    purrr::map_dfr(.id = "country", ~data.frame(pi = .x)) %>%
    dplyr::mutate(alpha = rbeta(n = nrow(.),
                                shape1 = asymp_fraction$shape1,
                                shape2 = asymp_fraction$shape2)) %>%
    tidyr::nest(data = -c(country)) 
  
  if (!fixed){
    inf_arrivals <- dplyr::inner_join(inf_arrivals,
                                      dplyr::filter(flight_vols, year == 2020) %>%
                                        tidyr::gather(country, trav_vol),
                                      by = "country") %>%
      dplyr::mutate(trav_vol = ceiling(trav_vol/2))
  } else {
    inf_arrivals <- dplyr::mutate(inf_arrivals, trav_vol = trav_vol_manual)
  }
  
  inf_arrivals <-  
    mutate(inf_arrivals,
           travellers = purrr::map2(.x = data,
                                    .y = trav_vol,
                                    .f = 
                                      ~make_travellers(
                                        x = .x,
                                        incubation_times = incubation_times,
                                        trav_vol = .y,
                                        trav_vol_p = trav_vol_p,
                                        fixed = fixed))) 
  
  inf_arrivals  <- 
    mutate(inf_arrivals,
           individuals = furrr::future_map(.x = travellers,
                                           .f = travellers_to_individuals,
                                           incubation_times = incubation_times))
  
  inf_arrivals <- tidyr::unnest(inf_arrivals, individuals)
  
  # add flight duration
  inf_arrivals <- dplyr::inner_join(inf_arrivals, flight_times)
  
  return(inf_arrivals)
  
}

