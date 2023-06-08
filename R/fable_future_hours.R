make_fable_future_hours <- function(future_hours) {
  future_hours |>
    # Aggregate over claims
    dplyr::summarise(hours = sum(hours), .by = c("billing_period", "age_group", "injury_group", "id")) |>
    tsibble::as_tsibble(index = billing_period, key = c(age_group, injury_group, id)) |>
    # Add aggregates over age group and injury group for each id
    fabletools::aggregate_key(age_group * injury_group * id, hours = sum(hours)) |>
    dplyr::filter(!fabletools::is_aggregated(id)) |>
    # Convert to empirical distributions by combining ids
    tibble::as_tibble() |>
		dplyr::group_by(billing_period, age_group, injury_group) |>
    dplyr::select(-id) |>
    tidyr::nest() |>
		dplyr::mutate(
      data = list(unlist(data)),
      hours = distributional::dist_sample(data),
      .mean = mean(hours)
    ) |>
		dplyr::ungroup() |>
		dplyr::select(-data) |>
    # Return as a fable object
    fabletools::as_fable(
      index = billing_period,
      key = c(age_group, injury_group),
      response = "hours",
      distribution = hours
    )
}
