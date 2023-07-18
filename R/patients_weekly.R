#' Produce Weekly Summary of Patients
#'
#' **UPDATE DOCUMENTATION**
#' I think the first row of nocap isoff
#' @description This function corresponds to the calculations in the Weekly
#' Summary tab under the headers 'Sick patients per week', 'Patients recovering
#' (or dying) from illness, per week'. It takes in the model output plus the
#' cumulative infections calculated by cases_weekly (which is the first step in
#' the weekly summary process). It then calculates hospital demands, including
#' bed capping. Note: there will be a lag on the appearances of the first values
#' in these projections, and these are dependent on the length of stay of
#' various case types specified in the get_parameters() function.
#'
#' Here is the description of the additional patient calculations:
#' \itemize{
#'  \item{crit_patients_nocap}{ - ICU_demand}
#'  \item{sev_patients_nocap}{ - hospital_demand}
#'  \item{mod_patients_nocap}{ - cum_mod_cases - cum_rem_mod_cases}
#'  \item{mild_patients_nocap}{ - cum_mild_cases - cum_rem_mild_cases}
#'  \item{crit_beds_inuse}{ - crit_patients_nocap capped by beds allocated to
#'  critical COVID}
#'  \item{sev_beds_inuse}{ - sev_patients_nocap capped by beds allocated to
#'  severe COVID}
#'  \item{total_beds_inuse}{ - severe + critical beds in use}
#'  \item{hosp_facilities_inuse}{ - total beds in use / avg. hosp beds per care
#'  centre}
#'  \item{rem_crit_patients}{ - cumulative removed critical patients -
#'  cumulative removed critical patients shifted by avg. length of hospital
#'  stay}
#'  \item{rem_sev_patients}{ - cumulative removed severe patients - cumulative
#'  removed severe patients shifted by avg. length of hospital stay}
#'  \item{rem_mod_patients}{ - same as rem_mod_cases - new moderate cases
#'  shifted by avg. length of stay in isolation}
#'  \item{rem_mild_patients}{ - same as rem_mild_cases - new mild cases shifted
#'  by avg. length of stay in isolation}
#'  \item{discharged_sev_patients}{ - admitted severe patients shifted back by
#'  avg. length of hospital stay}
#'  \item{discharged_crit_patients}{ - admitted critical patients shifted back
#'  by avg. length of hospital stay}
#'  \item{sev_patients_admitted_cap}{ - admitted severe patients capped by
#'  severe beds, number of beds in use, and number of patients discharged}
#'  \item{crit_patients_admitted_cap}{ - admitted critical patients capped by
#'  critical beds, number of beds in use, and number of patients discharged}
#' }
#' @param params From get_parameters
#' @param country_capacity From get_country_capacity
#' @param data Weekly summary dataframe - from cases_weekly
#' @param data_source Either WHO or Imperial.
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
#'   \item{sev_patients_admitted_cap}{Severe patients admitted during the week
#'   based on bed availability and numbers of discharged patients}
#'   \item{crit_patients_admitted_cap}{Critical patients admitted during the
#'   week based on bed availability and numbers of discharged patients}
#'   }
#'
#' @import dplyr
#' @importFrom data.table shift
#' @importFrom rlang .data
#'
#' @export
patients_weekly <- function(params,
                            country_capacity, # from get country capacity
                            data,
                            data_source = "Imperial") {
  # add exists part here
  # the total bed capacity was calculated in the input excel sheet
  # basically here you take the min of the new cases by severity/num beds avail
  # and theoretically the control for time spent in bed (removal) should have
  # already been done
  params <- merge(params, country_capacity)

  if (data_source == "Imperial") {
    data <- data %>%
      dplyr::mutate(
        sev_patients_nocap = hospital_demand,
        crit_patients_nocap = ICU_demand
      )
    # taken from cases weekly - maybe update here since admitted
    data <- data %>%
      dplyr::mutate(
        cum_rem_mild_cases = data.table::shift(cum_mild_cases,
          n = params$stay_mild
        ),
        cum_rem_mod_cases = data.table::shift(cum_mod_cases,
          n = params$stay_mod
        ),
        # isnt this going to give a negative value?
        cum_rem_severe_cases = cum_severe_cases -
          sev_patients_nocap,
        cum_rem_critical_cases = cum_critical_cases -
          crit_patients_nocap
      )
  } else if (data_source == "WHO") {
    data <- data %>%
      dplyr::mutate(
        cum_rem_mild_cases = data.table::shift(cum_mild_cases,
                                               n = params$stay_mild
        ),
        cum_rem_mod_cases = data.table::shift(cum_mod_cases,
                                              n = params$stay_mod
        ),
        cum_rem_severe_cases = data.table::shift(cum_severe_cases,
                                                  n = params$stay_sev
        ),
        cum_rem_critical_cases = data.table::shift(cum_critical_cases,
                                                   n = params$stay_crit
        )
      )
    data <- data %>%
      dplyr::mutate(
        sev_patients_nocap = cum_severe_cases - cum_rem_severe_cases,
        crit_patients_nocap = cum_critical_cases -
          cum_rem_critical_cases
      )
  }

  data <- data %>%
    dplyr::mutate(
      mild_patients_nocap = cum_mild_cases - cum_rem_mild_cases,
      mod_patients_nocap = cum_mod_cases - cum_rem_mod_cases
    )

  data <- data %>%
    dplyr::mutate(
      sev_beds_inuse = ifelse(
        sev_patients_nocap < params$severe_beds_covid,
        sev_patients_nocap, params$severe_beds_covid
      ),
      crit_beds_inuse = ifelse(
        crit_patients_nocap < params$crit_beds_covid,
        crit_patients_nocap, params$crit_beds_covid
      )
    )

  data <- data %>%
    dplyr::mutate(
      total_beds_inuse = sev_beds_inuse + crit_beds_inuse,
      hosp_facilities_inuse = ceiling(
        (sev_beds_inuse + crit_beds_inuse) /
          params$n_hosp_beds_per_care_unit
      )
    )

  data <- data %>%
    dplyr::mutate(
      rem_mild_patients = data.table::shift(new_mild_cases,
        n = params$stay_mild
      ),
      rem_mod_patients = data.table::shift(new_mod_cases,
        n = params$stay_mod
      ),
      rem_sev_patients = cum_rem_severe_cases -
        data.table::shift(cum_rem_severe_cases, n = 1),
      rem_crit_patients = cum_rem_critical_cases -
        data.table::shift(cum_rem_critical_cases, n = 1)
    )

  # initialize the columns
  data$discharged_sev_patients <- c(rep(0, nrow(data)))
  data$discharged_crit_patients <- c(rep(0, nrow(data)))
  data$sev_patients_admitted_cap <- c(rep(0, nrow(data)))
  data$crit_patients_admitted_cap <- c(rep(0, nrow(data)))

  data[is.na(data)] <- 0

  # depends on stay - there will be a lag between the start of the projections
  # and the first occurrence of a discharged patient value based on how long
  # the length of stay is in weeks
  for (i in 1:nrow(data)) {
    if (params$stay_sev >= i && params$stay_crit >= i) {
      discharged_sev_patients <- 0
      discharged_crit_patients <- 0
      sev_patients_admitted_cap <- data$sev_beds_inuse[i]
      crit_patients_admitted_cap <- data$crit_beds_inuse[i]
    } else if (params$stay_sev < i && params$stay_crit >= i) {
      discharged_crit_patients <- 0
      discharged_sev_patients <-
        data$sev_patients_admitted_cap[i - params$stay_sev]
      sev_patients_admitted_cap <- ifelse((data$sev_beds_inuse[i - 1] -
        discharged_sev_patients + data$new_severe_cases[i]) >
        params$severe_beds_covid,
      params$severe_beds_covid - (data$sev_beds_inuse[i - 1] -
        discharged_sev_patients),
      data$new_severe_cases[i]
      )
      crit_patients_admitted_cap <- 0
    } else if (params$stay_sev >= i && params$stay_crit < i) {
      discharged_crit_patients <-
        data$crit_patients_admitted_cap[i - params$stay_crit]
      discharged_sev_patients <- 0
      sev_patients_admitted_cap <- 0
      crit_patients_admitted_cap <- ifelse((data$crit_beds_inuse[i - 1] -
        discharged_crit_patients + data$new_critical_cases[i]) >
        params$crit_beds_covid,
      params$crit_beds_covid - (data$crit_beds_inuse[i - 1] -
        discharged_crit_patients),
      data$new_critical_cases[i]
      )
    } else {
      # maybe should i add something here ??
      # it still doesnt work if i subset before or after calculation
      discharged_sev_patients <-
        data$sev_patients_admitted_cap[i - params$stay_sev]
      discharged_crit_patients <-
        data$crit_patients_admitted_cap[i - params$stay_crit]
      sev_patients_admitted_cap <- ifelse((data$sev_beds_inuse[i - 1] -
        discharged_sev_patients + data$new_severe_cases[i]) >
        params$severe_beds_covid,
      params$severe_beds_covid - (data$sev_beds_inuse[i - 1] -
        discharged_sev_patients),
      data$new_severe_cases[i]
      )
      crit_patients_admitted_cap <- ifelse((data$crit_beds_inuse[i - 1] -
        discharged_crit_patients + data$new_critical_cases[i]) >
        params$crit_beds_covid,
      params$crit_beds_covid - (data$crit_beds_inuse[i - 1] -
        discharged_crit_patients),
      data$new_critical_cases[i]
      )
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
# so the only
  data <- subset(data, data$week_begins > as.Date(user$week1))
# block it off for predicted period
  return(data)
}
