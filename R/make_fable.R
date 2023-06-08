# Fix simulation labels
fix_sim_labels <- function(sim, data, label_maps) {
  # Turn array into a tibble
  nsim <- dim(sim)[3]
  nh <- dim(sim)[2]
  index <- as.character(tsibble::index(data))
  if(index == "billing_period") {
    last_month_of_data <- max(data$billing_period)
    interval <- 28
  } else {
    last_month_of_data <- max(data$acc_month)
    interval <- 1
  }
  output <- NULL
  for (i in seq(nh)) {
    z <- suppressMessages(tibble::as_tibble(sim[, i, ], rownames = "Series", .name_repair = "unique"))
    colnames(z)[-1] <- seq(nsim)
    z <- z |>
      tidyr::pivot_longer(-Series, names_to = "id", values_to = "count") |>
      dplyr::mutate(h = i)
    output <- dplyr::bind_rows(output, z)
  }
  # Now add factor variables using Series names
  output <- output |>
  	dplyr::mutate(
      acc_month = last_month_of_data + h * interval,
      age_group2 = dplyr::case_when(
      	stringr::str_detect(Series, "^G1/A[0-9][0-9]") ~ stringr::str_sub(Series, 4, 6),
        stringr::str_detect(Series, "^A[0-9][0-9]") ~ stringr::str_sub(Series, 1, 3),
        TRUE ~ "<aggregated>"
      ),
      injury_group2 = dplyr::case_when(
        stringr::str_detect(Series, "^G2/") ~ stringr::str_sub(Series, 4, 9),
        stringr::str_detect(Series, "^A[0-9][0-9]") ~ stringr::str_sub(Series, 4, 9),
        TRUE ~ "<aggregated>"
      ),
    )
  # Convert back to original names
  output <- output |>
  	dplyr::left_join(label_maps$labels_age, by = "age_group2") |>
  	dplyr::left_join(label_maps$labels_injury_group, by = "injury_group2") |>
  	dplyr::select(-dplyr::ends_with("2")) |>
    tidyr::replace_na(list(
      age_group = "<aggregated>",
      injury_group = "<aggregated>",
      # injclass = "<aggregated>",
      injury = "<aggregated>"
    ))

  # Convert to factors
  output <- output |>
  	dplyr::mutate(
      injury_group = factor(injury_group, levels = sort(unique(as.character(injury_group)))),
      age_group = factor(age_group, levels = c("0-9","10-19","20-34","35-49","50-74", "75+", "<aggregated>"))
    )

  # Check whether this is for accident counts or hours
  if(index == "billing_period") {
    output <- output |>
    	dplyr::rename(hours = count) |>
    	dplyr::rename(billing_period = acc_month)
  }

  return(output)
}

# Turn simulated future sample paths into a fable object
make_fable_from_sim <- function(sim, time_index, response) {
  # Combine count to form distributions
  sim <- sim |>
  	dplyr::group_by(dplyr::across(dplyr::all_of(c("Series", as.character(time_index), "age_group", "injury_group", "h")))) |>
  	dplyr::select(-id) |>
    tidyr::nest() |>
  	dplyr::mutate(
      data = list(unlist(data)),
      count = distributional::dist_sample(data),
      .mean = mean(count)
    ) |>
  	dplyr::ungroup() |>
  	dplyr::select(-data, -Series) |>
    tsibble::as_tsibble(
      index = {{ time_index }},
      key = c(age_group, injury_group)
    ) 
  colnames(sim)[which(colnames(sim) == "count")] <- response
  sim |> 
    fabletools::as_fable(
      response = response,
      distribution = response
    ) |> 
    suppressWarnings() 
}

