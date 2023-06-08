library(TACforecasting)

# Read data
group_costs <- read_tac_data(
  claims_file = here::here("data-raw/T086_claim_header.csv"),
  costs_file = here::here("data-raw/T086_attendant_care_hours.csv")
)
# Replace non-zero elements with random data
nonzero <- abs(group_costs$adjusted_hours) > 0
group_costs$adjusted_hours[nonzero] <- exp(stats::rnorm(sum(nonzero), 7, 1))
usethis::use_data(group_costs, overwrite = TRUE)
