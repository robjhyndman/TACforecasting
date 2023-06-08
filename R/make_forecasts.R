#' Generate forecasts of attendant care hours
#' 
#' Generate forecasts from ETS and ARIMA models, reconcile them, and combine them.
#' Return a fable object containing the forecasts.
#' @param data Data set computed from \code{\link{read_tac_data}}
#' @param h Forecast horizon.
#' @param nsim Number of simulated future sample paths per model.
#' @export
get_forecasts <- function(data, h, nsim) {
   # Check inputs
  if(h < 2)
  	stop("h must be at least 2")
	if(nsim < 2)
		stop("nsim must be at least 2")
   
  # Set up data as gts object
  label_maps <- make_label_maps(data)
  data_gts <- make_hours_gts(data, label_maps)
  # Fit models
  models_ets <- fit_models(data_gts, model_function = forecast::ets, model = "ZZN")
  models_arima <- fit_models(data_gts, model_function = forecast::auto.arima)
  #models_glm <- fit_models(claims_gts, model_function = tscount)
  #models_naive <- fit_models(claims_gts, model_function = naiveecdf)
  # Generate sample paths from models
  future_ets <- future_sample_paths(models_ets, h, nsim)
  future_arima <- future_sample_paths(models_arima, h, nsim)
  # Reconcile sample paths
  M_ets <- make_mapping_matrix(data_gts, models_ets)
  M_arima <- make_mapping_matrix(data_gts, models_arima)
  reconciled_future_ets <- reconcile_sample_paths(future_ets, M_ets)
  reconciled_future_arima <- reconcile_sample_paths(future_arima, M_arima)
  # Combine reconciled sample paths
  reconciled_future_comb <- combine_sample_paths(
      reconciled_future_ets, reconciled_future_arima)
  # Add back proper group labels
  future_gts <- fix_sim_labels(reconciled_future_comb, data, label_maps)
  # Turn into fable and return result
  fable_object <- make_fable_from_sim(future_gts, tsibble::index(data), "hours")
  fable_object |> 
  	dplyr::mutate(
      age_group = fabletools::agg_vec(age_group),
      injury_group = fabletools::agg_vec(injury_group)
    )
}
