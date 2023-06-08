#' Plot attendant care hours disaggregated by age or injury group.
#' 
#' Produce a time plot of attendant care hours per billing period disaggregated by the given `variable`
#' 
#' @param group_costs A tsibble containing costs optionally split by a variable
#' @param variable Name of disaggregation variable. If NULL, aggregated costs are shown
#' @param include_average Should the average cost per billing period be shown?
#' @author Rob J Hyndman
#' @examples
#'   group_costs |> 
#'     plot_total_hours(age_group)
#' @export
plot_total_hours <- function(group_costs, variable = NULL, include_average = TRUE) {
	group_costs <- group_costs |>
		dplyr::group_by({{ variable }}) |> 
		dplyr::summarise(
			adjusted_hours = sum(adjusted_hours),
			nclaims = sum(nclaims)
		) |>
		tibble::as_tibble() |>
		dplyr::mutate(ave_hours = adjusted_hours / nclaims) |> 
		dplyr::select(billing_period, {{ variable }}, dplyr::contains("hours")) |>
		tidyr::pivot_longer(adjusted_hours:ave_hours, names_to = "panel", values_to = "hours") |>
		dplyr::mutate(panel = factor(panel,
													levels = c("adjusted_hours", "ave_hours"),
													labels = c("Total hours", "Average hours")
		)) 
	if(!include_average) {
		group_costs <- group_costs |> 
			dplyr::filter(panel != "Average hours")
	}
	group_costs |> 
		ggplot2::ggplot(ggplot2::aes(x = billing_period, y = hours / 1e3, group = {{ variable }}, col = {{ variable }})) +
		ggplot2::geom_line() +
		ggplot2::facet_grid(panel ~ ., scales = "free_y") +
		ggplot2::theme(legend.position = "bottom") +
		ggplot2::guides(col = ggplot2::guide_legend(ncol = 3, title = "")) +
		ggplot2::labs(
			x = "Billing period",
			y = "Thousands of hours",
			title = "Hours in each billing period"
		)
}

#' Plot forecasts of attendant care hours disaggregated by age or injury group.
#' 
#' Produce a time plot of attendant care hours per billing period for specific disaggregations.
#' 
#' @param forecasts A fable object created by \code{\link{get_forecasts}}
#' @param data The data used to construct the forecasts. This should be a tsibble object of the same form as \code{\link{group_costs}}.
#' @param show_age_group A character string specifying either a specific age group or "<aggregated>" meaning the total across all age groups.
#' @param show_injury_group A character string specifying either a specific injury group or "<aggregated>" meaning the total across all injury groups.
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#'   group_costs |> 
#'     get_forecasts(h=13, nsim=100) |> 
#'     plot_forecasts(group_costs)
#' }
#' @export

plot_forecasts <- function(forecasts, data,
													show_age_group = "<aggregated>", show_injury_group = "<aggregated>") {
	response <- attributes(forecasts)$response
	index <- as.character(index(data))
	if("adjusted_hours" %in% colnames(data)) {
		data <- data |> 
			dplyr::rename(hours = adjusted_hours)
	}
	groups <- c(
		dplyr::if_else(show_age_group == "<aggregated>", NA, "age_group"),
		dplyr::if_else(show_injury_group == "<aggregated>", NA, "injury_group")
	) |>
		stats::na.omit() |>
		c()
	data_tibble <- tibble::as_tibble(data)
	if (show_age_group != "<aggregated>") {
		data_tibble <- data_tibble |>
			dplyr::filter(age_group == show_age_group)
	}
	if (show_injury_group != "<aggregated>") {
		data_tibble <- data_tibble |>
			dplyr::filter(injury_group == show_injury_group)
	}
	if(response == "count") {
		data_tibble <- data_tibble |>
			dplyr::summarise(
				response = sum(count), .by = dplyr::all_of(c(index, groups)))
	} else {
		data_tibble <- data_tibble |>
			dplyr::summarise(response = sum(hours), .by = dplyr::all_of(c(index, groups)))
	}
	data_tibble$index <- data_tibble[[index]]
	p <- forecasts |>
		dplyr::mutate(age_group = factor(age_group), injury_group = factor(injury_group)) |> 
		dplyr::filter(
			age_group == show_age_group,
			injury_group == show_injury_group,
		) |>
		fabletools::autoplot() +
		ggplot2::geom_line(ggplot2::aes(x = index, y = response), data = data_tibble)
	if(index == "acc_month") {
		p <- p + ggplot2::labs(x = "Accident month")
	} else {
		p <- p + ggplot2::labs(x = "Billing period")
	}
	if(response == "hours") {
		p <- p + ggplot2::labs(
			y = "Total hours",
			title = "Forecasts of billing hours"
		)
	} else {
		p <- p + ggplot2::labs(
			y = "Number of accidents",
			title = "Forecasts of accident numbers"
		)
	}
	return(p)
}

