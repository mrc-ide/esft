#' Produce Weekly Summary of Diagnostics
#'
#' @description
#'
#' @param params
#' @param data Weekly summary dataframe - from cases_weekly
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{xyz}
#'   \item{week_ends}{xyz}
#'   \item{hospital_demand}{xyz}
#'   }
#' @import dplyr
#' @importFrom data.table shift
#'
#' @export
patients_weekly <- function(params,
                            data) {
  # add exists part here
  data <- data %>%
    dplyr::mutate(
      mild_patients_nocap = cum_mild_cases - cum_rem_mild_cases,
      mod_patients_nocap = cum_mod_cases - cum_rem_mod_cases,
      sev_patients_nocap = hospital_demand,
      crit_patients_nocap = ICU_demand
    )

  data <- data %>%
    dplyr::mutate(
      sev_beds_inuse = ifelse(sev_patients_nocap < params$severe_beds_covid,
                              sev_patients_nocap, params$severe_beds_covid
      ),
      crit_beds_inuse = ifelse(crit_patients_nocap < params$crit_beds_covid,
                               crit_patients_nocap, params$crit_beds_covid
      )
    )

  data <- data %>%
    dplyr::mutate(
      total_beds_inuse = sev_beds_inuse + crit_beds_inuse,
      hosp_facilities_inuse = ceiling((sev_beds_inuse + crit_beds_inuse) / params$n_hosp_beds_per_care_unit)
    )

  data <- data %>%
    dplyr::mutate(
      rem_mild_patients = data.table::shift(new_mild_cases, n=params$stay_mild),
      rem_mod_patients = data.table::shift(new_mod_cases, n=params$stay_mod),
      rem_sev_patients = cum_rem_severe_cases - data.table::shift(cum_rem_severe_cases, n=1),
      rem_crit_patients = cum_rem_critical_cases - data.table::shift(cum_rem_critical_cases, n=1)
    )

  data <- data %>%
    dplyr::mutate(
      discharged_sev_patients = data.table::shift(adm_severe_cases_cap, n=params$stay_sev),
      discharged_crit_patients = data.table::shift(adm_critical_cases_cap, n=params$stay_sev)
    )

  data[is.na(data)] <- 0

  return(data)

}
