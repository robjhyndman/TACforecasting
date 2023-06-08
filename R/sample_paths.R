future_sample_paths <- function(models, h = 52, nsim = 1000) {
  ntime <- length(models[[1]]$x)
  nseries <- length(models)
  if (!("tsglm" %in% class(models[[1]]))) {
    innov_res <- calculate_residuals(models, type = "innovation")
  } else {
    innov_res <- matrix(0, nrow = ntime, ncol = nseries)
  }

  # Compute reconciled future sample paths using cross-sectional bootstrap
  sim <- array(0, c(nseries, h, nsim))
  dimnames(sim)[[1]] <- names(models)
  dimnames(sim)[[2]] <- paste0("h=", seq(h))
  dimnames(sim)[[3]] <- paste0("sim", seq(nsim))
  for (j in seq(nsim)) {
    bootres <- innov_res[sample(ntime, size = h, replace = TRUE), ]
    for (i in seq(nseries)) {
      sim[i, , j] <- stats::simulate(models[[i]], innov = bootres[, i], nsim = h)
    }
  }
  # Set negative to zero
  sim[sim < 0] <- 0
  # Round result
  return(sim)
}

calculate_residuals <- function(models, type = c("innovation", "response")) {
  # Type of residual to store
  type <- match.arg(type)
  # Length of each series
  ntime <- length(models[[1]]$x)
  # Number of models
  nseries <- length(models)

  # Compute the residuals from each model
  res <- matrix(0, nrow = ntime, ncol = nseries)
  for (i in seq(nseries)) {
    res[, i] <- stats::residuals(models[[i]], type = type)
  }

  return(res)
}

combine_sample_paths <- function(...) {
  paths <- list(...)
  check_dim <- matrix(lapply(paths, dim) |> unlist(), ncol = 3, byrow = TRUE)
  if (!forecast::is.constant(check_dim[, 1]) | !forecast::is.constant(check_dim[, 2])) {
    stop("Not all sample paths of equal dimension")
  }
  abind::abind(paths)
}
