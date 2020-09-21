source('run_analysis_func.R')

# TEST WITH FIXED 10000 TRAVELLERS

test_10k <- run_analysis(
  n_arrival_sims  = 1000,
  countries       = c("EU", "USA"),
  trav_vol_manual = 10000,
  fixed           = TRUE,
  n_travellers    = 1e4,
  trav_vol_p      = 1,
  seed            = 145)

test_10k_fig <-
  make_plots(arrival_released_times = test_10k, 
             input = input,
             text_size = 2.5,
             log_scale = FALSE,
             main_scenarios = main_scenarios,
             faceting = country ~ stringency,
             pre_board_screening = FALSE)

save_plot(test_10k_fig,
          prefix = "10k",
          base = "main",
          width = 210, height = 210, units = "mm", device = NULL)

test_10k_fig_type <-
  make_plots(arrival_released_times = test_10k, 
             input = input,
             text_size = 2.5,
             log_scale = FALSE,
             main_scenarios = main_scenarios,
             faceting = country + type ~ stringency,
             pre_board_screening = FALSE)

save_plot(test_10k_fig_type,
          base = "type",
          prefix ="10k",
          height = 420, width = 210,
          device = c("pdf", "png"))

rr <- run_rr_analysis(test_10k, main_scenarios, 
                      baseline_scenario = baseline_low)
  #This is for the risk-ratio analysis


# TEST WITH REAL TRAVEL VOLUMES

test_real <- run_analysis(
  n_arrival_sims  = 1000,
  countries       = c("EU", "USA"),
  n_travellers    = 1e4,
  trav_vol_p      = 7/30,
  fixed           = FALSE,
  seed            = 145)

test_real_fig <-
  make_plots(arrival_released_times = test_real, 
             input = input,
             text_size = 2.5,
             log_scale = FALSE,
             main_scenarios = main_scenarios,
             faceting = country  ~ stringency,
             pre_board_screening = FALSE)

save_plot(test_real_fig, 
          base = "main",
          prefix ="real",
          height = 210, width = 210,
          device = c("pdf", "png"))
