#' Produce Weekly Summary of Patients
#'
#' @description This function corresponds to the calculations in the Weekly
#' Summary tab under the headers 'Sick patients per week', 'Patients recovering
#' (or dying) from illness, per week'. It takes in the model output plus the
#' cumulative infections calculated by cases_weekly (which is the first step in
#' the weekly summary process). It then calculates hospital demands, including
#' bed capping.
#'
#' Note: I might be able to put this into cases weekly as well, but at first
#' i'll do it separately.
#'
#' Here is the description of the additional patient calculations:
#'
#' * mild_patients_nocap - cum_mild_cases - cum_rem_mild_cases
#' * mod_patients_nocap - cum_mod_cases - cum_rem_mod_cases
#' * sev_patients_nocap - hospital_demand
#' * crit_patients_nocap - ICU_demand
#' * sev_beds_inuse - sev_patients_nocap capped by beds allocated to severe
#' COVID
#' * crit_beds_inuse - crit_patients_nocap capped by beds allocated to critical
#' COVID
#' * total_beds_inuse - severe + critical beds in use
#' * hosp_facilities_inuse - total beds in use / avg. hosp beds per care centre
#' * rem_mild_patients - same as rem_mild_cases - new mild cases shifted by avg.
#' length of stay in isolation
#' * rem_mod_patients - same as rem_mod_cases - new moderate cases shifted by
#' avg. length of stay in isolation
#' * rem_sev_patients - cumulative removed severe patients - cumulative removed
#' severe patients shifted by avg. length of hospital stay
#' * rem_crit_patients - cumulative removed critical patients - cumulative
#' removed critical patients shifted by avg. length of hospital stay
#' * discharged_sev_patients - admitted severe patients shifted back by avg.
#' length of hospital stay
#' * discharged_crit_patients - admitted critical patients shifted back by avg.
#' length of hospital stay
#'
#' @param params Includes country_capacity params and get_parameters params
#' @param data Weekly summary dataframe - from cases_weekly
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
#'   \item{mild_patients_nocap}{cum_mild_cases - cum_rem_mild_cases}
#'   \item{mod_patients_nocap}{cum_mod_cases - cum_rem_mod_cases}
#'   \item{sev_patients_nocap}{Uncapped hospital demand}
#'   \item{crit_patients_nocap}{Uncapped ICU demand}
#'   \item{sev_beds_inuse}{Hospital demand capped by beds allocated to severe
#' COVID}
#'   \item{crit_beds_inuse}{ICU demand capped by beds allocated to critical
#' COVID}
#'   \item{total_beds_inuse}{Sum of severe and critical beds in use}
#'   \item{hosp_facilities_inuse}{Total beds in use divided by the avg. number
#'   of beds per hospital, as specified in parameters}
#'   \item{rem_mild_patients}{Mild cases who have completed avg length of
#'   isolation}
#'   \item{rem_mod_patients}{Moderate cases who have completed avg length of
#'   isolation}
#'   \item{rem_sev_patients}{Cumulative removed severe patients - the
#'   cumulative removed severe patients shifted back by avg. length of hospital
#'   stay}
#'   \item{rem_crit_patients}{Cumulative removed critical patients - the
#'   cumulative removed critical patients shifted back by avg. length of
#'   hospital stay}
#'   \item{discharged_sev_patients}{Severe patients admitted number of weeks
#'   ago that = avg. length of stay}
#'   \item{discharged_crit_patients}{Critical patients admitted number of weeks
#'   ago that = avg. length of stay}
#'   }
#'
#' @import dplyr
#' @importFrom data.table shift
#'
#' @export
patients_weekly <- function(params,
                            data) {
  # add exists part here
  # the total bed capacity was calculated in the input excel sheet
  # basically here you take the min of the new cases by severity/num beds avail
  # and theoretically the control for time spent in bed (removal) should have
  # already been done

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
      hosp_facilities_inuse = ceiling(
        (sev_beds_inuse + crit_beds_inuse) / params$n_hosp_beds_per_care_unit
      )
    )

  data <- data %>%
    dplyr::mutate(
      rem_mild_patients = data.table::shift(new_mild_cases,
        n = params$stay_mild
      ),
      rem_mod_patients = data.table::shift(new_mod_cases, n = params$stay_mod),
      rem_sev_patients = cum_rem_severe_cases -
        data.table::shift(cum_rem_severe_cases, n = 1),
      rem_crit_patients = cum_rem_critical_cases -
        data.table::shift(cum_rem_critical_cases, n = 1)
    )

  data <- data %>%
    dplyr::mutate(
      discharged_sev_patients = data.table::shift(adm_severe_cases_cap,
        n = params$stay_sev
      ),
      discharged_crit_patients = data.table::shift(adm_critical_cases_cap,
        n = params$stay_sev
      )
    )

  data[is.na(data)] <- 0

  return(data)
}
