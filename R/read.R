# Read in data and wrangle into a form suitable for analysis

#' Read in TAC data
#'
#' This function takes two csv files as inputs: one containing the claims header
#' and the other containing the attendant hours. It returns total hours per
#' age group and injury group by billing period.
#'
#' @param claims_file CSV file containing claims header
#' @param costs_file CSV file containing attendant hours
#' @return A tsibble object containing total attendant care adjusted hours
#' for each billing period, disaggregated by age group and injury group.
#' The column `nclaims` shows the number of "active" claims in each
#' billing period.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' group_costs <- read_tac_data(
#'   claims_file = "T086_claim_header.csv",
#'   costs_file = "T086_attendant_care_hours.csv"
#' )
#' }
#' @export

read_tac_data <- function(claims_file, costs_file) {
	claims_prelim <- read_claims(claims_file)
	costs_prelim <- read_costs(costs_file)
	# Add costs information to claims
	claims <- add_costs_to_claims(costs_prelim, claims_prelim)
	# Add claims information to costs
	costs <- add_claims_to_costs(costs_prelim, claims)
	# Compute total costs by age group and injury group
	calc_group_costs(costs, claims)
}

read_claims <- function(file) {
  # Read claims meta data
    readr::read_csv(file) |>
    janitor::clean_names() |>
		dplyr::rename(is_deceased = "isdeceased") |>
		dplyr::mutate(
      acc_month = tsibble::yearmonth(lubridate::dmy(acc_month)),
      acc_year = forcats::fct_reorder(acc_year, acc_year, .fun = "unique", .desc = TRUE),
      injury = stringr::str_to_title(injury),
      injury = dplyr::na_if(injury, "."),
      injury = factor(injury, levels = sort(unique(as.character(injury)))),
      injury = dplyr::if_else(is.na(injury), "Missing", injury),
      injclass = factor(injclass, levels = sort(unique(as.character(injclass)))),
      injury_group = factor(injury_group, levels = sort(unique(as.character(injury_group)))),
      is_deceased = dplyr::if_else(is_deceased == "Yes", TRUE, FALSE),
      deceased_date = readr::parse_date(deceased_month, format = "%d%b%Y"),
      acc_age = pmax(round(lubridate::year(acc_month) - birth_year), 0),
      current_age = acc_age + (tsibble::yearmonth("2023-04-01") - acc_month) / 12,
      current_age = dplyr::if_else(is_deceased, NA, current_age),
      age_group = dplyr::case_when(
        acc_age < 10 ~ "0-9",
        acc_age < 20 ~ "10-19",
        acc_age < 35 ~ "20-34",
        acc_age < 50 ~ "35-49",
        acc_age < 75 ~ "50-74",
        TRUE ~ "75+"
      ),
      age_group = factor(age_group, levels = c("0-9","10-19","20-34","35-49","50-74", "75+"))
    ) |>
		dplyr::select(claim, acc_month, acc_age, age_group, current_age, dplyr::starts_with("inj"), is_deceased, deceased_date) |>
    dplyr::arrange(acc_month, claim)
}

read_costs <- function(file) {
  # Read individual claims data
   readr::read_csv(file) |>
    janitor::clean_names() |>
		dplyr::mutate(
      billing_period = lubridate::dmy(billing_period_end),
      amount_paid = readr::parse_number(amount_paid),
      # Set missing adjusted hours to 0
      adjusted_hours = tidyr::replace_na(adjusted_hours, 0)
    ) |>
		dplyr::select(-billing_period_start, -billing_period_end) |>
    # Trim data sets from 1995 to 2022
		dplyr::filter(billing_period > "1994-12-31", billing_period < "2023-01-01") |>
		dplyr::select(billing_period, claim, dplyr::everything())
}

# Add costs info to claims

add_costs_to_claims <- function(costs, claims) {
  # Find last billing period for each claim
  last_claim <- costs |>
    tibble::as_tibble() |>
  	dplyr::filter(claim %in% unique(claims$claim)) |>
  	dplyr::group_by(claim) |>
  	dplyr::mutate(last_bill = max(billing_period)) |>
  	dplyr::filter(billing_period == last_bill) |>
  	dplyr::ungroup() |>
  	dplyr::select(claim, last_bill)

  # Find total costs for each claim
  total_costs <- costs |>
    tibble::as_tibble() |>
    dplyr::summarise(
      #accom = sum(adjusted_accom_amount, na.rm = TRUE),
      #paid = sum(adjusted_amount_paid, na.rm = TRUE),
      hours = sum(adjusted_hours, na.rm = TRUE),
      .by = "claim"
    )

  # Add that info to claims and return
  last_billing_period <- max(costs$billing_period)
  claims |>
  	dplyr::filter(claim %in% unique(costs$claim)) |>
  	dplyr::left_join(last_claim, by = "claim") |>
  	dplyr::left_join(total_costs, by = "claim") |> 
    add_lifetime_claims(last_billing_period)
}

# Add claims info to costs

add_claims_to_costs <- function(costs, claims) {
  # Add meta data to costs and compute age by billing period
  costs <- costs |>
  	dplyr::filter(claim %in% unique(claims$claim)) |>
  	dplyr::left_join(claims, by = "claim") |>
  	dplyr::mutate(age = acc_age + 1 / 365 * as.numeric(lubridate::as_date(billing_period) - lubridate::as_date(acc_month))) |>
  	dplyr::select(billing_period, claim, dplyr::starts_with("inj"), dplyr::contains("age"), dplyr::everything())
}
