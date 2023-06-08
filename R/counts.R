# Time series of costs by age and injury groups
calc_group_costs <- function(costs, claims) {
  group_costs <- costs |>
  	dplyr::as_tibble() |>
  	dplyr::group_by(billing_period, age_group, injury_group) |>
  	dplyr::summarise(
      adjusted_hours = sum(adjusted_hours, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tsibble::as_tsibble(index = billing_period, key = c(age_group, injury_group), regular = TRUE) |>
  	dplyr::mutate(nclaims = 0)

  # Find number of active claims in each billing period
  for (i in seq(NROW(group_costs))) {
    group_costs$nclaims[i] <- claims |>
    	dplyr::filter(
        lubridate::as_date(acc_month) <= group_costs$billing_period[i],
        last_bill >= group_costs$billing_period[i],
        age_group == group_costs$age_group[i],
        injury_group == group_costs$injury_group[i]
      ) |>
      NROW()
  }
  return(group_costs)
}


add_lifetime_claims <- function(claims, last_billing_period) {
  max_bill <- max(claims$last_bill, na.rm = TRUE)
  claims |>
  	dplyr::mutate(
      censored = !is_deceased & last_bill >= last_billing_period - 365,
      length = dplyr::if_else(censored,
        max_bill - lubridate::as_date(acc_month),
        last_bill - lubridate::as_date(acc_month)
      ),
      # Compute lengths in days
      length = as.numeric(length)
    )
}

