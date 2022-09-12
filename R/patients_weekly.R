#' Produce Weekly Summary of Patients
#'
#' @description
#' Basically this function corresponds to the calculations in the Weekly
#' Summary tab under the headers 'Sick patients per week', 'Patients recovering
#' (or dying) from illness, per week'. It takes in the model output plus the
#' cumulative infections calculated by cases_weekly (which is the first step in
#' the weekly summary process).
#' The difference with this function is that it includes some of the bed capping.
#'
#'
#' I might be able to put this into cases weekly as well, but at first i'll just
#' do it separately.
#'
#' @param params
#' @param data Weekly summary dataframe - from cases_weekly
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{xyz}
#'   \item{week_ends}{xyz}
#'   \item{hospital_demand}{xyz}
#'   \item{ICU_demand}{xyz}
#'   \item{hospital_incidence}{xyz}
#'   \item{ICU_incidence}{xyz}
#'   \item{infections}{xyz}
#'   \item{cumulative_infections}{xyz}
#'   \item{cum_severe_cases}{xyz}
#'   \item{new_severe_cases}{xyz}
#'   \item{cum_critical_cases}{xyz}
#'   \item{new_critical_cases}{xyz}
#'   \item{adm_severe_cases_nocap}{xyz}
#'   \item{adm_critical_cases_nocap}{xyz}
#'   \item{adm_severe_cases_cap}{xyz}
#'   \item{adm_critical_cases_cap}{xyz}
#'   \item{new_mild_cases}{xyz}
#'   \item{new_mod_cases}{xyz}
#'   \item{new_mild_cases_2}{xyz}
#'   \item{new_mod_cases_2}{xyz}
#'   \item{new_severe_cases_2}{xyz}
#'   \item{new_critical_cases_2}{xyz}
#'   \item{cum_mild_cases}{xyz}
#'   \item{cum_mod_cases}{xyz}
#'   \item{rem_mild_cases}{xyz}
#'   \item{rem_mod_cases}{xyz}
#'   \item{rem_severe_cases}{xyz}
#'   \item{rem_critical_cases}{xyz}
#'   \item{cum_rem_mild_cases}{xyz}
#'   \item{cum_rem_mod_cases}{xyz}
#'   \item{cum_rem_severe_cases}{xyz}
#'   \item{cum_rem_critical_cases}{xyz}
#'   \item{sus_cases_but_negative}{xyz}
#'   \item{mild_patients_nocap}{xyz}
#'   \item{mod_patients_nocap}{xyz}
#'   \item{sev_patients_nocap}{xyz}
#'   \item{crit_patients_nocap}{xyz}
#'   \item{sev_beds_inuse}{xyz}
#'   \item{crit_beds_inuse}{xyz}
#'   \item{total_beds_inuse}{xyz}
#'   \item{hosp_facilities_inuse}{xyz}
#'   \item{rem_mild_patients}{xyz}
#'   \item{rem_mod_patients}{xyz}
#'   \item{rem_sev_patients}{xyz}
#'   \item{rem_crit_patients}{xyz}
#'   \item{discharged_sev_patients}{xyz}
#'   \item{discharged_crit_patients}{xyz}
#'   }
#' @import dplyr
#' @importFrom data.table shift
#'
#' @export
patients_weekly <- function(params,
                            data) {
  # add exists part here
  # the total bed capacity was calculated in the input excel sheet
  # basically here you take the min of the new cases by severity or the num beds available
  # and theoretically the control for time spent in bed (removal) should have already been done

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
