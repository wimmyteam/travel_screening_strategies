# Additional functions that facilitate more flexible 

# function to generate relevant clinical times for travellers
# The argument called pathogen is a list showing pathogen specific details for symptomatic and 
# assymptomatic cases. To generate the list you need gamma.parms.from.quantiles, a very long 
# function that we might not need to change since we are not interested in this specific 
# details for our scenario.

source("run_analysis_func.R")

# This function runs a single simulation
estimate_infectious_days_per_traveller <- function(
  prevalence = 0.05,
  quarentine_days = 3,
  quarentine_compliance = 0.8,
  syndromatic_sensitivity = 0.7
)
{
  
  sim_results <- run_analysis(
  )
  return(sim_results)
}
