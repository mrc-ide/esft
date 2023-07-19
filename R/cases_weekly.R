#' Produce Weekly Summary of Cases
#'
#' @description Replicates the calculations in the 'Weekly Summary' tab of the
#' ESFT depending on the input data source (WHO or Imperial). There are elements
#' of these calculations, such as the assumption that hospital incidence =
#' number of severe cases in the Imperial calculations, that the ICL team does
#' not make in its calculations. Therefore, keep in mind that this is just a
#' duplicate of the calculations in the ESFT, and not a critical appraisal of
#' these calculations. This calculation does not yield an output where the first
#' row contains the start date, as rows previous to the start date are needed
#' for the patient calculations.
#'
#' Note: since proportion of mild and moderate are the same, the estimates of
#' these categories will be the same.
#'
#' @param params Includes both estimates of beds and case severity proportions.
#' @param capacity From get_country_capacity
#' @param test_strategy_params From set_testing_strategy
#' @param data Specific country fit data, from the imperial model fits or from
#' WHO data
#' @param data_source Either WHO or Imperial.
#' @param user From user_input function
#'
#' @return Dataframe of weekly summary, if data_source = "Imperial". If the
#' data_source = "WHO", the ICU and hospital columns will not be included.
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
#'   \item{new_critical_cases}{For Imperial input = new critical cases per week
#'   as defined by as the ICU incidence. For WHO input, this is equal to the
#'   number of new cases per week multiplied by the proportion of infections
#'   that are critical}
#'   \item{new_severe_cases}{For Imperial input = new severe cases per week
#'   as defined by as the hospital incidence. For WHO input, this is equal to
#'   the number of new cases per week multiplied by the proportion of infections
#'   that are severe}
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
#' @import countrycode
#' @importFrom data.table last
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#'
#' @export
cases_weekly <- function(params, # from get_parameters
                         capacity, # country capacity, from get_country_capacity
                         test_strategy_params, # from set_testing_strategy
                         data,
                         data_source = "Imperial",
                         user) {
  params <- merge(params, capacity)
  params <- merge(params, test_strategy_params)

  if (is.null(params)) {
    stop("Parameters must be called using get_parameters before calculating
         the weekly summary.")
  }

  if (data_source == "Imperial") {
    # add exists part here
    data$date <- as.Date(data$date)
    # data <- data %>% dplyr::select(-any_of(death_calibrated))
    data <- data[!(names(data) == "death_calibrated")]

    data <- data %>%
      tidyr::pivot_wider(
        names_from = compartment,
        values_from = y_mean
      )

    data <- data %>%
      dplyr::group_by(week_begins = cut(date,
                                        breaks = "week",
                                        right = FALSE, include.lowest = T
      )) %>%
      dplyr::summarise(
        week_ends = data.table::last(date),
        hospital_demand = max(hospital_demand, na.rm = TRUE),
        ICU_demand = max(ICU_demand, na.rm = TRUE),
        hospital_incidence = sum(hospital_incidence, na.rm = TRUE),
        ICU_incidence = sum(ICU_incidence, na.rm = TRUE),
        infections = sum(infections, na.rm = TRUE),
        cumulative_infections = data.table::last(cumulative_infections)
      )

    data$week_begins <- as.Date(as.character(data$week_begins))

    data <- data %>%
      dplyr::mutate(
        new_critical_cases = ICU_incidence,
        new_severe_cases = hospital_incidence
      )
  } else if (data_source == "WHO") {
    data$date <- as.Date(data$Date_reported)

    data <- data[!(names(data) == "Date_reported")]

    data <- data %>%
      dplyr::group_by(week_begins = cut(date,
                                        breaks = "week",
                                        right = FALSE, include.lowest = T
      )) %>%
      dplyr::summarise(
       week_ends = data.table::last(date),
       cases = sum(New_cases, na.rm = TRUE)
      )

    data$week_begins <- as.Date(as.character(data$week_begins))

    data <- data %>%
      dplyr::mutate(
        new_critical_cases = cases * params$crit_i_proportion,
        new_severe_cases = cases * params$sev_i_proportion
      )
  }
  data[is.na(data)] <- 0
  data <- data %>%
    dplyr::mutate(
      # moderate and mild cases, method in patient calcs:
      # why only severe and critical here, and not moderate?
      new_mod_cases = (new_severe_cases + new_critical_cases) *
        params$mod_i_proportion / (params$sev_i_proportion
          + params$crit_i_proportion),
      new_mild_cases = (new_severe_cases + new_critical_cases) *
        params$mild_i_proportion / (params$sev_i_proportion
          + params$crit_i_proportion)
    )

  # subset to the weeks needed before hand to be able to have values for the
  # first week
  week_behind <- max(params$stay_crit, params$stay_mild, params$stay_mod,
                     params$stay_sev, na.rm=T)
  week_behind = week_behind + 1
  data <- subset(data, data$week_ends > (as.Date(user$week1)-7*(week_behind)))

  data <- data %>%
    dplyr::mutate(
      cum_critical_cases = cumsum(new_critical_cases),
      cum_severe_cases = cumsum(new_severe_cases),
      cum_mod_cases = cumsum(new_mod_cases),
      cum_mild_cases = cumsum(new_mild_cases)
    )
  data$cum_cases <- data$cum_critical_cases + data$cum_severe_cases +
    data$cum_mod_cases + data$cum_mild_cases
  data <- data %>%
    dplyr::mutate(sus_cases_but_negative = (new_mild_cases
      + new_mod_cases
      + new_severe_cases
      + new_critical_cases) * params$num_neg_per_pos_test)

  return(data)
}
