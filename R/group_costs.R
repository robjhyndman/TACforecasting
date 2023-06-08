#' Synthetic data for attendant hours by age group and injury group
#'
#' Artificial Transport Accident Commission attendant care data
#'
#' \code{group_costs} is a daily `tsibble` with index `billing_period` and 
#' two values:
#' \tabular{ll}{
#'     adjusted_hours:      \tab Total attendant care hours\cr
#'     nclaims:     \tab Number of active claims \cr
#' }
#'
#' The data is disaggregated using two keys:
#' \tabular{ll}{
#'     age_group:      \tab Age group of client at the time of accident \cr
#'     injury_group:      \tab Injury sustained by client due to accident \cr
#' }
#'
#' @source
#' Synthetic data
#'
#' @name group_costs
#' @format Time series of class `tsibble`
#' @keywords datasets
#' @examples
#' group_costs
#'
NULL