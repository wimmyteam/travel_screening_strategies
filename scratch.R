# This file is used for scratching around in the code and exploring the data structures

source("wimmy_additions.R")

new_infections <- estimate_infectious_days_per_traveller(
  prevalence = 0.05,
  quarentine_days = 3,
  quarentine_compliance = 0.8,
  syndromatic_sensitivity = 0.7
)

