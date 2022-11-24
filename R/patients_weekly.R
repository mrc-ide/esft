#' Produce Weekly Summary of Patients
#'
#' @description This function corresponds to the calculations in the Weekly
#' Summary tab under the headers 'Sick patients per week', 'Patients recovering
#' (or dying) from illness, per week'. It takes in the model output plus the
#' cumulative infections calculated by cases_weekly (which is the first step in
#' the weekly summary process). It then calculates hospital demands, including
#' bed capping.
#'
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
#' @param params From get_parameters
#' @param country_capacity From get_country_capacity
#' @param data Weekly summary dataframe - from cases_weekly
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
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
#' @importFrom rlang .data
#'
#' @export
patients_weekly <- function(params,
                            country_capacity, # from get country capacity
                            data) {
  # add exists part here
  # the total bed capacity was calculated in the input excel sheet
  # basically here you take the min of the new cases by severity/num beds avail
  # and theoretically the control for time spent in bed (removal) should have
  # already been done
  params <- merge(params, country_capacity)

  data <- data %>%
    dplyr::mutate(
      mild_patients_nocap = .data$cum_mild_cases - .data$cum_rem_mild_cases,
      mod_patients_nocap = .data$cum_mod_cases - .data$cum_rem_mod_cases,
      sev_patients_nocap = .data$hospital_demand,
      crit_patients_nocap = .data$ICU_demand
    )

  data <- data %>%
    dplyr::mutate(
      sev_beds_inuse = ifelse(
        .data$sev_patients_nocap < params$severe_beds_covid,
        .data$sev_patients_nocap, params$severe_beds_covid
      ),
      crit_beds_inuse = ifelse(
        .data$crit_patients_nocap < params$crit_beds_covid,
        .data$crit_patients_nocap, params$crit_beds_covid
      )
    )

  data <- data %>%
    dplyr::mutate(
      total_beds_inuse = .data$sev_beds_inuse + .data$crit_beds_inuse,
      hosp_facilities_inuse = ceiling(
        (.data$sev_beds_inuse + .data$crit_beds_inuse) /
          params$n_hosp_beds_per_care_unit
      )
    )

  data <- data %>%
    dplyr::mutate(
      rem_mild_patients = data.table::shift(.data$new_mild_cases,
        n = params$stay_mild
      ),
      rem_mod_patients = data.table::shift(.data$new_mod_cases,
                                           n = params$stay_mod),
      rem_sev_patients = .data$cum_rem_severe_cases -
        data.table::shift(.data$cum_rem_severe_cases, n = 1),
      rem_crit_patients = .data$cum_rem_critical_cases -
        data.table::shift(.data$cum_rem_critical_cases, n = 1)
    )
  # keep in mind: discharged means capping
  data <- data %>%
    dplyr::mutate(
      discharged_sev_patients = data.table::shift(.data$adm_severe_cases_cap,
        n = params$stay_sev
      ),
      discharged_crit_patients = data.table::shift(.data$adm_critical_cases_cap,
        n = params$stay_sev
      )
    )

  data <- data %>% dplyr::select(c(
    week_begins, week_ends, mild_patients_nocap,
    mod_patients_nocap, sev_patients_nocap,
    crit_patients_nocap, sev_beds_inuse,
    crit_beds_inuse, total_beds_inuse,
    hosp_facilities_inuse, rem_mild_patients,
    rem_mod_patients, rem_sev_patients,
    rem_crit_patients, discharged_sev_patients,
    discharged_crit_patients
  ))
  data[is.na(data)] <- 0

  return(data)
}
