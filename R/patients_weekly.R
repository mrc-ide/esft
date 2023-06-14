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
#' * crit_patients_nocap - ICU_demand
#' * sev_patients_nocap - hospital_demand
#' * mod_patients_nocap - cum_mod_cases - cum_rem_mod_cases
#' * mild_patients_nocap - cum_mild_cases - cum_rem_mild_cases
#' * crit_beds_inuse - crit_patients_nocap capped by beds allocated to critical
#' COVID
#' * sev_beds_inuse - sev_patients_nocap capped by beds allocated to severe
#' COVID
#' * total_beds_inuse - severe + critical beds in use
#' * hosp_facilities_inuse - total beds in use / avg. hosp beds per care centre
#' * rem_crit_patients - cumulative removed critical patients - cumulative
#' removed critical patients shifted by avg. length of hospital stay
#' * rem_sev_patients - cumulative removed severe patients - cumulative removed
#' severe patients shifted by avg. length of hospital stay
#' * rem_mod_patients - same as rem_mod_cases - new moderate cases shifted by
#' avg. length of stay in isolation
#' * rem_mild_patients - same as rem_mild_cases - new mild cases shifted by avg.
#' length of stay in isolation
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
#'   \item{crit_patients_nocap}{Uncapped ICU demand}
#'   \item{sev_patients_nocap}{Uncapped hospital demand}
#'   \item{mod_patients_nocap}{cum_mod_cases - cum_rem_mod_cases}
#'   \item{mild_patients_nocap}{cum_mild_cases - cum_rem_mild_cases}
#'   \item{crit_beds_inuse}{ICU demand capped by beds allocated to critical
#' COVID, same as crit_patients_cap}
#'   \item{sev_beds_inuse}{Hospital demand capped by beds allocated to severe
#' COVID, same as sev_patients_cap}
#'   \item{total_beds_inuse}{Sum of severe and critical beds in use}
#'   \item{hosp_facilities_inuse}{Total beds in use divided by the avg. number
#'   of beds per hospital, as specified in parameters}
#'   \item{rem_crit_patients}{Cumulative removed critical patients - the
#'   cumulative removed critical patients shifted back by avg. length of
#'   hospital stay}
#'   \item{rem_sev_patients}{Cumulative removed severe patients - the
#'   cumulative removed severe patients shifted back by avg. length of hospital
#'   stay}
#'   \item{rem_mod_patients}{Moderate cases who have completed avg length of
#'   isolation}
#'   \item{rem_mild_patients}{Mild cases who have completed avg length of
#'   isolation}
#'   \item{discharged_crit_patients}{Critical patients admitted number of weeks
#'   ago that = avg. length of stay}
#'   \item{discharged_sev_patients}{Severe patients admitted number of weeks
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
      sev_patients_nocap = .data$hospital_demand,
      crit_patients_nocap = .data$ICU_demand
    )
  # taken from cases weekly - maybe update here since admitted
  data <- data %>%
    dplyr::mutate(
      cum_rem_mild_cases = data.table::shift(.data$cum_mild_cases,
        n = params$stay_mild
      ),
      cum_rem_mod_cases = data.table::shift(.data$cum_mod_cases,
        n = params$stay_mod
      ),
      # isnt this going to give a negative value?
      cum_rem_severe_cases = .data$cum_severe_cases -
        .data$sev_patients_nocap,
      cum_rem_critical_cases = .data$cum_critical_cases -
        .data$crit_patients_nocap
    )

  data <- data %>%
    dplyr::mutate(
      mild_patients_nocap = .data$cum_mild_cases - .data$cum_rem_mild_cases,
      mod_patients_nocap = .data$cum_mod_cases - .data$cum_rem_mod_cases
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
        n = params$stay_mod
      ),
      rem_sev_patients = .data$cum_rem_severe_cases -
        data.table::shift(.data$cum_rem_severe_cases, n = 1),
      rem_crit_patients = .data$cum_rem_critical_cases -
        data.table::shift(.data$cum_rem_critical_cases, n = 1)
    )

  # initialize the columns
  data$discharged_sev_patients <- c(rep(0, nrow(data)))
  data$discharged_crit_patients <- c(rep(0, nrow(data)))
  data$sev_patients_admitted_cap <- c(rep(0, nrow(data)))
  data$crit_patients_admitted_cap <- c(rep(0, nrow(data)))

  data[is.na(data)] <- 0

  # this has to be a loop since they are dependent on each other
  # but the issue here is that it depends on keeping the stays in hospital constant
  # theoretically this would calculate backwards to get the x amount backwards of whatever - although that doesnt work
  # so how bout we calculate a -1
  # would that work in dealing with the condition?
  for (i in 1:nrow(data)) {
    # step 1: get current num discharged, which is the previous
    if (i == 1){
      discharged_sev_patients = 0
      discharged_crit_patients = 0
      sev_patients_admitted_cap = data$sev_beds_inuse[i]
      crit_patients_admitted_cap = data$crit_beds_inuse[i]
    } else if (i == 2){
      discharged_crit_patients = 0
      discharged_sev_patients = data$sev_patients_admitted_cap[i-params$stay_sev]
      sev_patients_admitted_cap = ifelse((data$sev_beds_inuse[i -1] -
                                            discharged_sev_patients + data$new_severe_cases[i]) >
                                           params$severe_beds_covid,
                                         params$severe_beds_covid - (data$sev_beds_inuse[i - 1] -
                                                                       discharged_sev_patients),
                                         data$new_severe_cases[i])
      crit_patients_admitted_cap = 0
    } else {
      # maybe should i add something here ??
      # it still doesnt work if i subset before or after calculation
      discharged_sev_patients = data$sev_patients_admitted_cap[i-params$stay_sev]
      discharged_crit_patients = data$crit_patients_admitted_cap[i-params$stay_crit]
      sev_patients_admitted_cap = ifelse((data$sev_beds_inuse[i -1] -
                                            discharged_sev_patients + data$new_severe_cases[i]) >
                                           params$severe_beds_covid,
                                         params$severe_beds_covid - (data$sev_beds_inuse[i - 1] -
                                                                       discharged_sev_patients),
                                         data$new_severe_cases[i])
      crit_patients_admitted_cap = ifelse((data$crit_beds_inuse[i -1] -
                                             discharged_crit_patients + data$new_critical_cases[i]) >
                                            params$crit_beds_covid,
                                          params$crit_beds_covid - (data$crit_beds_inuse[i - 1] -
                                                                      discharged_crit_patients),
                                          data$new_critical_cases[i])
    }


      data$discharged_sev_patients[i] <- discharged_sev_patients
      data$discharged_crit_patients[i] <- discharged_crit_patients
      data$sev_patients_admitted_cap[i] <- sev_patients_admitted_cap
      data$crit_patients_admitted_cap[i] <- crit_patients_admitted_cap
  }


  data <- data %>% dplyr::select(c(
    week_begins, week_ends, crit_patients_nocap,
    sev_patients_nocap, mod_patients_nocap,
    mild_patients_nocap, crit_beds_inuse,
    sev_beds_inuse, total_beds_inuse,
    hosp_facilities_inuse, rem_crit_patients,
    rem_sev_patients, rem_mod_patients,
    rem_mild_patients, discharged_crit_patients, discharged_sev_patients,
    sev_patients_admitted_cap, crit_patients_admitted_cap
  ))


  return(data)
}
