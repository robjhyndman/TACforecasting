make_mapping_matrix <- function(data, models) {
  ntime <- NROW(data$bts)

  # Grab the residuals from each model
  response_res <- calculate_residuals(models, type = "response")
  nseries <- NCOL(response_res)

  # Create S matrix
  S <- hts::smatrix(data)

  # Find W matrices
  W2 <- hts:::lowerD(response_res)

  # Find G matrix
  nbottom <- NCOL(data$bts)
  Winv <- MASS::ginv(W2)
  G2 <- MASS::ginv(t(S) %*% Winv %*% S) %*% t(S) %*% Winv

  # Computing mapping matrix M
  M2 <- S %*% G2

  return(M2)
}

reconcile_sample_paths <- function(sample_paths, mapping_matrix) {
  nsim <- dim(sample_paths)[3]
  newsim <- sample_paths
  for (j in seq(nsim)) {
    newsim[, , j] <- mapping_matrix %*% sample_paths[, , j]
  }
  # Set negative to zero
  newsim[newsim < 0] <- 0
  return(newsim)
}
