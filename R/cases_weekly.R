#' Produce Weekly Summary of Cases
#'
#' @description Replicates the calculations in the 'Weekly Summary' tab of the
#' ESFT. There are elements of these calculations, such as the assumption that
#' hospital incidence = number of severe cases, that the ICL team does not make
#' in its calculations. Therefore, keep in mind that this is just a replication.
#'
#' Note: since proportion of mild and moderate are the same, the estimates of
#' these categories will be the same.
#'
#' @param params Includes both estimates of beds and case severity proportions.
#' @param capacity From get_country_capacity
#' @param test_strategy_params From set_testing_strategy
#' @param data Specific country fit data, from the imperial model fits
#' @param starting_date User specified string of starting date of weekly summary
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{hospital_demand}{Summed hospital demand: number of people who would
#'   be using a hospital bed given enough healthcare capacity but won't
#'   necessarily have it}
#'   \item{ICU_demand}{Summed ICU demand: number of people who would be using a
#'   hospital bed given enough healthcare capacity but won't necessarily have
#'   it}
#'   \item{hospital_incidence}{Summed hospital incidence: the total number of
#'   new people who need a new hospital bed at the current time. This does not
#'   include those recovering from ICU in a hospital bed or who already have
#'   access to a bed.}
#'   \item{ICU_incidence}{Summed ICU incidence: the total number of
#'   new people who need a new ICU bed at the current time. This does not
#'   include those who already have access to a bed.}
#'   \item{infections}{Estimated number of new infections, from model fits}
#'   \item{cumulative_infections}{Cumulative number of infections}
#'   \item{new_critical_cases}{New critical cases per week as defined by
#'   the ESFT: new ICU incidence}
#'   \item{new_severe_cases}{New severe cases per week as defined by the ESFT:
#'   hospital incidence that week}
#'   \item{new_mod_cases}{New moderate cases every week, found by doing a
#'   transformation of the critical and severe cases and multiplying by the
#'   moderate case proportion}
#'   \item{new_mild_cases}{New mild cases every week, found by doing a
#'   transformation of the critical and severe cases and multiplying by the mild
#'   case proportion}
#'   \item{cum_critical_cases}{Cumulative critical cases per week as defined by
#'   the ESFT: cumulative ICU incidence}
#'   \item{cum_severe_cases}{Cumulative severe cases per week as defined by the
#'   ESFT: cumulative hospital incidence}
#'   \item{cum_mod_cases}{Cumulative moderate cases, using the first moderate
#'   case calculation}
#'   \item{cum_mild_cases}{Cumulative mild cases, using the first mild case
#'   calculation}
#'   \item{sus_cases_but_negative}{Sum of all new cases multiplied by the
#'   number of negative tests per positive case}
#' }
#'
#' @import dplyr
#' @import data.table
#' @import countrycode
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
cases_weekly <- function(params, # from get_parameters
                         capacity, # country capacity, from get_country_capacity
                         test_strategy_params, # from set_testing_strategy
                         data, # imperial fit data
                         starting_date = "2019-11-29") {
  params <- merge(params, capacity)
  params <- merge(params, test_strategy_params)

  # add exists part here
  data$date <- as.Date(data$date)

  if (!is.null(starting_date)) {
    starting_date <- as.Date(starting_date)
    data <- subset(data, data$date >= starting_date)
  }

  if (is.null(params)) {
    stop("Parameters must be called using get_parameters before calculating
         the weekly summary.")
  }

  data <- data %>%
    dplyr::select(-c("death_calibrated"))

  data <- data %>%
    tidyr::pivot_wider(
      names_from = "compartment",
      values_from = "y_mean"
    )

  data <- data %>%
    dplyr::group_by(week_begins = cut(.data$date, breaks = "week",
                                      right = FALSE, include.lowest = T)) %>%
    dplyr::summarise(
      week_ends = data.table::last(.data$date),
      hospital_demand = max(.data$hospital_demand, na.rm = TRUE),
      ICU_demand = max(.data$ICU_demand, na.rm = TRUE),
      hospital_incidence = sum(.data$hospital_incidence, na.rm = TRUE),
      ICU_incidence = sum(.data$ICU_incidence, na.rm = TRUE),
      infections = sum(.data$infections, na.rm = TRUE),
      cumulative_infections = data.table::last(.data$cumulative_infections)
    )

  data$week_begins <- as.Date(as.character(data$week_begins))

  data <- data %>%
    dplyr::mutate(
      new_critical_cases = .data$ICU_incidence,
      new_severe_cases = .data$hospital_incidence
    )

  data <- data %>%
    dplyr::mutate(
      # moderate and mild cases, method in patient calcs:
      # why only severe and critical here, and not moderate?
      new_mod_cases = (.data$new_severe_cases + .data$new_critical_cases) *
        params$mod_i_proportion / (params$sev_i_proportion
          + params$crit_i_proportion),
      new_mild_cases = (.data$new_severe_cases + .data$new_critical_cases) *
        params$mild_i_proportion / (params$sev_i_proportion
          + params$crit_i_proportion)
    )

  data <- data %>%
    dplyr::mutate(
      cum_critical_cases = cumsum(.data$ICU_incidence),
      cum_severe_cases = cumsum(.data$hospital_incidence),
      cum_mod_cases = cumsum(.data$new_mod_cases),
      cum_mild_cases = cumsum(.data$new_mild_cases)
    )

  data <- data %>%
    dplyr::mutate(sus_cases_but_negative = (.data$new_mild_cases
      + .data$new_mod_cases
      + .data$new_severe_cases
      + .data$new_critical_cases) * params$num_neg_per_pos_test)
  data[is.na(data)] <- 0

  return(data)
}
