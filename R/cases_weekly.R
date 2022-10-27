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
#' @param data Specific country fit data, from the imperial model fits
#' @param starting_date User specified, starting date of weekly summary
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
#'   \item{cum_severe_cases}{Cumulative severe cases per week as defined by the
#'   ESFT: cumulative hospital incidence}
#'   \item{new_severe_cases}{New severe cases per week as defined by the ESFT:
#'   hospital incidence that week}
#'   \item{cum_critical_cases}{Cumulative critical cases per week as defined by
#'   the ESFT: cumulative ICU incidence}
#'   \item{new_critical_cases}{New critical cases per week as defined by
#'   the ESFT: new ICU incidence}
#'   \item{adm_severe_cases_nocap}{Admitted severe cases per week = hospital
#'   demand}
#'   \item{adm_critical_cases_nocap}{Admitted critical cases per week - ICU
#'   demand}
#'   \item{adm_severe_cases_cap}{Hospital incidence, capped by severe bed
#'   availability}
#'   \item{adm_critical_cases_cap}{ICU incidence, capped by critical bed
#'   availability}
#'   \item{new_mild_cases}{New mild cases every week, found by doing a
#'   transformation of the critical and severe cases and multiplying by the mild
#'   case proportion}
#'   \item{new_mod_cases}{New moderate cases every week, found by doing a
#'   transformation of the critical and severe cases and multiplying by the
#'   moderate case proportion}
#'   \item{new_mild_cases_2}{New mild cases, alternative calculation. Multiplies
#'   the infections per week times the mild proportion.}
#'   \item{new_mod_cases_2}{New moderate cases, alternative calculation.
#'   Multiplies the infections per week times the mild proportion.}
#'   \item{new_severe_cases_2}{New severe cases, alternative calculation.
#'   Multiplies the infections per week times the mild proportion.}
#'   \item{new_critical_cases_2}{New critical cases, alternative calculation.
#'   Multiplies the infections per week times the mild proportion.}
#'   \item{cum_mild_cases}{Cumulative mild cases, using the first mild case
#'   calculation}
#'   \item{cum_mod_cases}{Cumulative moderate cases, using the first moderate
#'   case calculation}
#'   \item{rem_mild_cases}{Mild cases who have completed their isolation,
#'   using the average length of stay for mild cases}
#'   \item{rem_mod_cases}{Moderate cases who have completed their isolation,
#'   using the average length of stay for moderate cases}
#'   \item{rem_severe_cases}{Severe cases who have completed their hospital
#'   stay, using the average length of stay for severe cases}
#'   \item{rem_critical_cases}{Critical cases who have completed their hospital
#'   stay, using the average length of stay for critical cases}
#'   \item{cum_rem_mild_cases}{Cumulative mild cases who have completed their
#'   isolation using the average length of stay for mild cases}
#'   \item{cum_rem_mod_cases}{Cumulative moderate cases who have completed their
#'   isolation using the average length of stay for moderate cases}
#'   \item{cum_rem_severe_cases}{Cumulative severe cases - admitted severe cases
#'   per week with no cap}
#'   \item{cum_rem_critical_cases}{Cumulative critical cases - admitted critical
#'   cases per week with no cap}
#'   \item{sus_cases_but_negative}{Sum of all new cases multiplied by the
#'   number of negative tests per positive case}
#' }
#' @import dplyr
#' @import data.table
#' @import countrycode
#' @importFrom magrittr %>%
#'
#' @export
cases_weekly <- function(params, # includes both beds and case proportions
                         data, # imperial fit data
                         starting_date = "2019-11-29") {

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
    dplyr::group_by(week_begins = cut(date, breaks = "week")) %>%
    dplyr::summarise(
      week_ends = data.table::last(date),
      hospital_demand = max("hospital_demand", na.rm = TRUE),
      ICU_demand = max("ICU_demand", na.rm = TRUE),
      hospital_incidence = sum("hospital_incidence", na.rm = TRUE),
      ICU_incidence = sum("ICU_incidence", na.rm = TRUE),
      infections = data.table::last("infections"),
      cumulative_infections = data.table::last("cumulative_infections")
    )

  data$week_begins <- as.Date(as.character(data$week_begins))

  data <- data %>%
    dplyr::mutate(
      cum_severe_cases = cumsum("hospital_incidence"),
      new_severe_cases = "hospital_incidence",
      cum_critical_cases = cumsum("ICU_incidence"),
      new_critical_cases = "ICU_incidence",
      adm_severe_cases_nocap = "hospital_demand",
      adm_critical_cases_nocap = "ICU_demand"
    )

  data <- data %>%
    dplyr::mutate(
      adm_severe_cases_cap =
        ifelse(hospital_incidence < params$severe_beds_covid,
        hospital_incidence, params$severe_beds_covid
      ),
      adm_critical_cases_cap = ifelse(ICU_incidence < params$crit_beds_covid,
        ICU_incidence, params$crit_beds_covid
      ),

      # moderate and mild cases, method in patient calcs:
      # why only severe and critical here, and not moderate?
      new_mild_cases = (new_severe_cases + new_critical_cases) *
        params$mildI_proportion / (params$sevI_proportion
                                   + params$critI_proportion),
      new_mod_cases = (new_severe_cases + new_critical_cases) *
        params$modI_proportion / (params$sevI_proportion
                                  + params$critI_proportion),

      # second method also in patient calcs:
      # second possible method - needs review
      # this results in MUCH fewer cases estimated btw
      new_mild_cases_2 = infections * params$mildI_proportion,
      new_mod_cases_2 = infections * params$modI_proportion,
      new_severe_cases_2 = infections * params$sevI_proportion,
      new_critical_cases_2 = infections * params$critI_proportion
    )


  data <- data %>%
    dplyr::mutate(
      cum_mild_cases = cumsum(new_mild_cases),
      cum_mod_cases = cumsum(new_mod_cases),
      rem_mild_cases = data.table::shift(new_mild_cases,
        n = params$stay_mild
      ),
      rem_mod_cases = data.table::shift(new_mod_cases,
        n = params$stay_mod
      ),
      rem_severe_cases = data.table::shift(new_severe_cases,
        n = params$stay_sev
      ),
      rem_critical_cases = data.table::shift(new_critical_cases,
        n = params$stay_crit
      )
    )

  data <- data %>%
    dplyr::mutate(
      cum_rem_mild_cases = data.table::shift(cum_mild_cases,
        n = params$stay_mild
      ),
      cum_rem_mod_cases = data.table::shift(cum_mod_cases,
        n = params$stay_mod
      ),
      cum_rem_severe_cases = cum_severe_cases - adm_severe_cases_nocap,
      cum_rem_critical_cases = cum_critical_cases - adm_critical_cases_nocap
    )

  data <- data %>%
    dplyr::mutate(sus_cases_but_negative = (new_mild_cases
    + new_mod_cases
      + new_severe_cases
      + new_critical_cases) * params$num_neg_per_pos_test)
  data[is.na(data)] <- 0

  return(data)
}
