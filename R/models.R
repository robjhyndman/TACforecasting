fit_models <- function(object, model_function = forecast::ets, ...) {
  # Generate all time series at all levels
  ally <- hts::aggts(object)
  # Loop over all time series and store models
  fit_model <- function(y, model_function) {
    ntime <- length(y)
    if (forecast::is.constant(y)) {
      # Series is constant. So just fit a mean zero model with small variance
      fit <- forecast::Arima(y + stats::rnorm(ntime, sd = 1e-3), order = c(0, 0, 0), include.mean = FALSE)
    } else {
      fit <- model_function(y, ...)
    }
  }
  models <- furrr::future_map(as.list(ally), fit_model,
    model_function = model_function,
    .options = furrr::furrr_options(seed = NULL)
  )
  return(models)
}
