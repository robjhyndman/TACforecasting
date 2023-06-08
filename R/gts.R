make_label_maps <- function(data) {
  data <- data |> 
    tibble::as_tibble() |> 
    dplyr::select(age_group, injury_group) |> 
    dplyr::distinct()
  
  # Set up mapping of labels from tibble to fixed length
  labels_age <- data |>
  	dplyr::select(age_group) |>
  	dplyr::distinct() |>
  	dplyr::mutate(
      age_group2 = dplyr::recode(age_group,
        "0-9" = "A00",
        "10-19" = "A10",
        "20-34" = "A20",
        "35-49" = "A35",
        "50-74" = "A50",
        "75+" = "A75"
      ),
      age_group = as.character(age_group),
      age_group2 = as.character(age_group2)
    )
  labels_injury_group <- data |>
  	dplyr::select(injury_group) |>
  	dplyr::distinct() |>
  	dplyr::mutate(
      injury_group2 = stringr::str_replace_all(injury_group, " ", ""),
      injury_group2 = stringr::str_pad(stringr::str_sub(injury_group2, end = 6), 6, side = "right", pad = "."),
      injury_group = as.character(injury_group)
    )
  return(list(
    labels_age = labels_age,
    labels_injury_group = labels_injury_group
  ))
}

# Make gts object from claims
make_claims_gts <- function(claims_tsibble, label_maps) {
  # Need class labels of fixed length to allow gts with a characters argument
  claims_ts <- claims_tsibble |>
    dplyr::left_join(label_maps$labels_age, by = "age_group") |>
  	dplyr::left_join(label_maps$labels_injury_group, by = "injury_group") |>
  	dplyr::mutate(
      age_group = age_group2,
      injury_group = injury_group2,
    ) |>
  	dplyr::select(-dplyr::ends_with("2")) |>
    # Convert to ts object
    tsbox::ts_ts()
  # Remove underscores from column names
  colnames(claims_ts) <- stringr::str_replace_all(colnames(claims_ts), "_", "")
  # Finally convert to gts object using column names to create labels
  claims_ts |>
    hts::gts(characters = list(3, 6))
}

# Make gts object from hours
make_hours_gts <- function(group_costs, label_maps) {
  # Need class labels of fixed length to allow gts with a characters argument
  hours_ts <- group_costs |>
    dplyr::left_join(label_maps$labels_age, by = "age_group") |>
  	dplyr::left_join(label_maps$labels_injury_group, by = "injury_group") |>
  	dplyr::mutate(
      age_group = age_group2,
      injury_group = injury_group2,
    ) |>
  	dplyr::select(-dplyr::ends_with("2")) |>
    tibble::as_tibble() |> 
    tidyr::complete(billing_period, age_group, injury_group, fill = list(adjusted_hours = 0)) |>
  	dplyr::mutate(Series = paste0(age_group, injury_group)) |>
  	dplyr::select(billing_period, Series, hours=adjusted_hours) |>
    tidyr::pivot_wider(names_from = Series, values_from = hours)

  # Convert to ts object
  hours_ts <- hours_ts |>
  	dplyr::arrange(billing_period) |>
  	dplyr::select(-billing_period) |>
    as.matrix() |>
    stats::ts(start = 1995 + 0.5 / 28, frequency = 365 / 28)

  # Finally convert to gts object using column names to create labels
  hours_ts |>
    hts::gts(characters = list(3, 6))
}


