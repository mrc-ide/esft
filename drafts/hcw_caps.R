#' HCW Caps
#'
#' @description This function calculates the static HCW caps found in the
#' `Weekly Summary` in the ESFT.
#'
#' @param params From get_parameters
#' @param capacity Country capacity, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param hwfe From WHO ESFT sheet
#' @param patients From patients_weekly
#' @param ambulanciers_per_bed Assumes 1 ambulance per 100 bed hospital w 2
#' operators at all times (3x8 hour shifts) = (2/100)*3 = 0.06, taken from ESFT
#' @param bio_eng_per_bed Assumes 2 biomed engineers on 8 hr shifts per 100 bed
#' hospital, taken from ESFT
#'
#' From old statis caps - UPDATE
#' @return List of caps
#' \describe{
#'   \item{hcws_inpatients_cap}{Date the week summarized begins}
#'   \item{hcws_screening_cap}{Date the week summarized ends}
#'   \item{lab_staff}{Lab staff available}
#'   \item{hygienists_per_bed}{Date the week summarized begins}
#'   \item{hygienists_per_crit_bed}{Date the week summarized ends}
#'   \item{cleaners_inpatient_cap}{Number cleaners required based on hospital
#'   beds in use}
#'   \item{inf_caregiver_inpatient_cap}{Number caregivers required based on
#'   hospital beds in use}
#' }
#'
#' Maybe redefine????
#'#' * perc_hcws_treat_covid - assumption of percentage of HCWs performing mostly
#' inpatient tasks with severe and critical patients (e.g., management of
#' respiratory failure and critical care monitoring); default = 0.51
#' * perc_hcws_screen_covid - assumption of percentage of HCWs screening and
#' triaging suspected COVID cases at all points of access to the health system,
#' including primary health centres, clinics, hospital emergency units, and ad
#' hoc community settings; default = 0.09
#'
#' @export
hcw_caps <- function(params,
                     capacity,
                     throughput,
                     hwfe, # from who sheet
                     patients) {

  # cases screened per HCW per day - Inputs, I78
  cases_screened_per_hcw_per_day <- 8/sum(
    hwfe$patient_t24_screen[
      hwfe$esft_group == "HCW"
    ]
  )

  # D18 - capped num lab staff for labs - WHOLE PROCESS ----- WS
  # calculating average covid capacity WS
  covid_capacity_high_throughput <- mean(
    throughput$covid_capacity[throughput$type == "high_throughput"]
  )
  covid_capacity_near_patient <- mean(
    throughput$covid_capacity[throughput$type == "near_patient"]
  )
  covid_capacity_manual <- mean(
    throughput$covid_capacity[throughput$type == "manual"]
  )
  # % lab staff available for covid response - E160 Inputs (or I160)
  lab_cap <- mean(
    covid_capacity_high_throughput,
    covid_capacity_near_patient,
    covid_capacity_manual
  )
  # back calculations, c32 & also c27
  lab_staff <- capacity$n_labs
  # c19 bed cap = I67 in inputs tab
  bed_cap <- capacity$n_hosp_beds # this has already been calculated in country capacity

  # D18 WS
  lab_staff_cap <-ifelse(is.na(lab_cap), lab_staff,
                         lab_staff*lab_cap)

  # num hygienists per bed, I73 - DEPENDS ON OUTPUT - patients
  hygienists_per_sev_bed <- hwfe$patient_t24_sev[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (
    round(sum(patients$sev_beds_inuse) / sum(patients$total_beds_inuse))
  ) * 10

  hygienists_per_crit_bed <- hwfe$patient_t24_crit[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (
    round(sum(patients$crit_beds_inuse) / sum(patients$total_beds_inuse))
  ) * 10

  hygienists_per_bed <- hygienists_per_sev_bed + hygienists_per_crit_bed

  # INPUTS - num hcws per bed, I72 - DEPENDS ON OUTPUT - patients
  hcws_per_sev_bed <- sum(
    hwfe$patient_t24_sev[
      hwfe$esft_group == "HCW"
    ]
  ) / 8 * (
    round(sum(patients$sev_beds_inuse) / sum(patients$total_beds_inuse))
  )

  hcws_per_crit_bed <- sum(
    hwfe$patient_t24_crit[
      hwfe$esft_group == "HCW"
    ]
  ) / 8 * (
    round(sum(patients$crit_beds_inuse) / sum(patients$total_beds_inuse))
  )

  hcws_per_bed <- hcws_per_sev_bed + hcws_per_crit_bed

  # back calculations - capped number cleaners for inpatient - C33 - same as ws D19
  # need to probably rework this, since the parameters are actually calculated from stuff
  if (is.null(params$n_hosp_beds)) {
    cleaners_inpatient_cap <- params$beds_covid * hygienists_per_bed
  } else {
    cleaners_inpatient_cap <- params$n_hosp_beds * hygienists_per_bed
  }

  # back calculations - c36 - of critical/severe, % of cases that are critical
  perc_crit_cases <- round(sum(patients$crit_beds_inuse)/sum(patients$total_beds_inuse), 3)
  # back calculations - c37 - of critical/severe, % of cases that are severe
  perc_sev_cases <- round(1 - perc_crit_cases, 3)
  # add check here that they sum to 1 / correspond

  # back calculations - C40 - probability of a new case being outpatient
  prob_outpatient <- params$mild_i_proportion + params$mod_i_proportion
  # back calculations - C41 - probability of a new case being inpatient
  prob_inpatient <- params$sev_i_proportion + params$crit_i_proportion
  # back calculations - C42 - HCWS required per outpatient
  hcws_per_outpatient <- round(1/cases_screened_per_hcw_per_day, 3)
  # back calculations - C43 - HCWS required per inpatient
  hcws_per_inpatient <- hcws_per_bed
  # back calculations - C44 - per new case, ratio of HCWs for inpatient vs outpatient
  ratio_hcws_inpatient_outpatient <- round((prob_inpatient*hcws_per_inpatient)/(prob_inpatient*hcws_per_inpatient + prob_outpatient*hcws_per_outpatient), 3)

  # inputs - I66 - % HCW treating hospitalized covid inpatients
  perc_treating_covid <- round(ratio_hcws_inpatient_outpatient*(1-params$perc_hcws_not_covid), 3)
  # inputs - I67 - % HCW screening/triaging suspected covid-19 cases
  perc_screening_covid <- round(1 - params$perc_hcws_not_covid - perc_treating_covid, 3)

  # BACK CALCULATIONS - - - -
  # back calculations C30 - D16 ws
  hcws_inpatients_cap <- round(perc_treating_covid * capacity$n_hcws, 3)
  # n_hcws = num nurses + num doctors,C31 - D17 ws
  hcws_screening_cap <- round(perc_screening_covid * capacity$n_hcws, 3)

  # condition check for correct prc hcws
  perc_hcw <- c(
    params$perc_hcws_not_covid,
    perc_treating_covid,
    perc_screening_covid
  )

  if (!approx_sum(perc_hcw, 1)) {
    stop("HCW allocation percentages do not sum to 1")
  }

  hcw_caps_list <- list(
    bed_cap = bed_cap,
    cases_screened_per_hcw_per_day = cases_screened_per_hcw_per_day,
    cleaners_inpatient_cap = cleaners_inpatient_cap,
    hcws_inpatients_cap = hcws_inpatients_cap,
    hcws_per_bed = hcws_per_bed,
    hcws_per_inpatient = hcws_per_inpatient, # DOUBLE CHECK IF ACTUALLY USED
    hcws_per_outpatient = hcws_per_outpatient,
    hcws_screening_cap = hcws_screening_cap,
    hygienists_per_bed = hygienists_per_bed, # not sure if the by severity breakdown needed
    lab_staff_cap = lab_staff_cap, # this one is lab staff available for covid - one in WS
    perc_crit_cases = perc_crit_cases,
    perc_screening_covid = perc_screening_covid, # is there a better way to call this just back to parameters? or should we just add it?
    perc_sev_cases = perc_sev_cases,
    perc_treating_covid = perc_treating_covid,
    prob_inpatient = prob_inpatient,
    prob_outpatient = prob_outpatient,
    ratio_hcws_inpatient_outpatient = ratio_hcws_inpatient_outpatient
  )
  return(hcw_caps_list)
}
