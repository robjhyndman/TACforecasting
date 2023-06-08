#' Compute accuracy statistics
#'
#' @param forecasts A fable object with forecasts, usually the output from \code{\link{get_forecasts}}
#' @param actuals A tsibble with actual values.  For example, the output from \code{\link{read_tac_data}}
#' @return A tibble with accuracy statistics
#' @export


tac_accuracy <- function(forecasts, actuals) {
  # Change variable name to hours
  if ("adjusted_hours" %in% colnames(actuals)) {
    actuals <- actuals |>
      dplyr::rename(hours = adjusted_hours)
  }
  # Add aggregations
  actuals <- actuals |>
    fabletools::aggregate_key(age_group * injury_group,
      hours = sum(hours), nclaims = sum(nclaims)
    ) |>
    dplyr::mutate(
      age_group = as.character(age_group),
      injury_group = as.character(injury_group)
    )
  forecasts <- forecasts |>
    dplyr::mutate(
      age_group = as.character(age_group),
      injury_group = as.character(injury_group)
    )

  coverage <- function(.dist, .actual, level = 95, na.rm = TRUE, ...) {
    interval <- fabletools::hilo(.dist, level)
    mean(.actual >= interval$lower & .actual <= interval$upper, na.rm = na.rm)
  }

  forecasts |>
    dplyr::filter(age_group == "<aggregated>", injury_group == "<aggregated>") |>
    fabletools::accuracy(
      actuals |> dplyr::filter(age_group == "<aggregated>", injury_group == "<aggregated>"),
      list(RMSE = RMSE, MAE = MAE, MAPE = MAPE, coverage = coverage, CRPS = CRPS),
      by = "h"
    ) |>
    dplyr::select(-.type)
}

#' Compute forecasts with a rolling origin and return accuracy statistics
#'
#' @param group_costs A tsibble with actual values. For example, the output from \code{\link{read_tac_data}}
#' @param h The forecast horizon
#' @param nsim The number of simulations used in each forecast for each model.
#' @param init The number of initial observations to use for the first fold.
#' @param step The number of observations to skip between each fold.
#' @return A tibble with accuracy statistics.
#' @export
tscv_accuracy <- function(group_costs, h, nsim, init, step) {
  all_dates <- sort(unique(group_costs$billing_period))
  end_dates <- all_dates[init + seq(0, length(all_dates) - init, by = step)]
  purrr::map_dfr(end_dates, tscv_accuracy_one_fold,
    group_costs = group_costs,
    h = h, nsim = nsim
  )
}

tscv_accuracy_one_fold <- function(end, group_costs, h, nsim) {
  fc <- get_forecasts(group_costs |> dplyr::filter(billing_period <= end), h, nsim)
  tac_accuracy(fc, group_costs)
}

utils::globalVariables(c("RMSE","MAE","MAPE","CRPS",".type"))
