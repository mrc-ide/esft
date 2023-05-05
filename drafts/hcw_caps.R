#' Static HCW Caps
#'
#' @description This function calculates the static HCW caps found in the
#' `Weekly Summary` in the ESFT.
#'
#' @param params From get_parameters
#' @param capacity Country capacity, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param cap_lab_staff TRUE/FALSE. Option to set a cap on lab staff based on
#' percent of diagnostic machinery allocated to COVID
#'
#' @return List of caps
#' \describe{
#'   \item{hcws_inpatients_cap}{Date the week summarized begins}
#'   \item{hcws_screening_cap}{Date the week summarized ends}
#'   \item{lab_staff}{Lab staff available}
#' }
#'
#'
#'#' * perc_hcws_treat_covid - assumption of percentage of HCWs performing mostly
#' inpatient tasks with severe and critical patients (e.g., management of
#' respiratory failure and critical care monitoring); default = 0.51
#' * perc_hcws_screen_covid - assumption of percentage of HCWs screening and
#' triaging suspected COVID cases at all points of access to the health system,
#' including primary health centres, clinics, hospital emergency units, and ad
#' hoc community settings; default = 0.09
#'
#' @export
hcw_caps_static <- function(params,
                            capacity,
                            throughput,
                            hwfe, # from who sheet
                            patients) {
  # add exists part here

  # BACK CALCULATIONS - - - -
  # back calculations C30
  hcws_inpatients_cap <- params$perc_hcws_treat_covid * capacity$n_hcws
  # n_hcws = num nurses + num doctors,C31
  hcws_screening_cap <- params$perc_hcws_screen_covid * capacity$n_hcws


  # BACK CALCULATIONS - DEPENDENT ON TOTAL FORECAST ------

  # INPUTS - - - - - - - - -
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


  # cases screened per HCW per day - Inputs, I78
  cases_screened_per_hcw_per_day <- 8/sum(
    hwfe$patient_t24_screen[
      hwfe$esft_group == "HCW"
    ]
  )


  # back calculations - capped number cleaners for inpatient - C33
  # need to probably rework this, since the parameters are actually calculated from stuff
  if (is.null(params$n_hosp_beds)) {
    cleaners_inpatient_cap <- params$beds_covid * hygienists_per_bed
  } else {
    cleaners_inpatient_cap <- params$n_hosp_beds * hygienists_per_bed
  }

  # back calculations - c36 - of critical/severe, % of cases that are critical
  perc_crit_cases <- sum(patients$crit_beds_inuse)/sum(patients$total_beds_inuse)
  # back calculations - c37 - of critical/severe, % of cases that are severe
  perc_sev_cases <- 1 - perc_crit_cases
  # add check here that they sum to 1 / correspond

  # back calculations - C40 - probability of a new case being outpatient
  prob_outpatient <- params$mild_i_proportion + params$mod_i_proportion
  # back calculations - C41 - probability of a new case being inpatient
  prob_inpatient <- params$sev_i_proportion + params$crit_i_proportion
  # back calculations - C42 - HCWS required per outpatient
  hcws_per_outpatient <- 1/cases_screened_per_hcw_per_day
  # back calculations - C43 - HCWS required per inpatient
  hcws_per_inpatient <- hcws_per_bed
  # back calculations - C44 - per new case, ratio of HCWs for inpatient vs outpatient
  ratio_hcws_inpatient_outpatient <- (prob_inpatient*hcws_per_inpatient)/(prob_inpatient*hcws_per_inpatient + prob_outpatient*hcws_per_outpatient)


  ambulanciers_per_bed = 0.06
  bio_eng_per_bed = 0.02

  # inputs - I66 - % HCW treating hospitalized covid inpatients
  perc_treating_covid <- ratio_hcws_inpatient_outpatient*(1-params$perc_hcws_not_covid)
  # inputs - I67 - % HCW screening/triaging suspected covid-19 cases
  perc_screening_covid <- 1 - params$perc_hcws_not_covid - perc_treating_covid


  # weekly summary caps - THESE DEPEND ON BEDS IN USE FROM PATIENTS - recursively ----------------------
  # D16 - capped num HCWs for inpatients
  cap_hcw_inpatient <- ifelse(is.na(capacity$n_hcws), hcws_inpatients_cap,
                              capacity$n_hcws*perc_treating_covid)
  # D17 - capped num hcws for screening/triage
  cap_hcw_screen <- ifelse(is.na(capacity$n_hcws),hcws_screening_cap,
                              capacity$n_hcws*perc_screening_covid)

  # D18 - capped num lab staff for labs - WHOLE PROCESS -----
  # calculating average covid capacity
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

  # THIS IS THE PREVIOUS CALCULATION
  lab_staff <-ifelse(cap_lab_staff == FALSE, lab_staff,
                     capacity$n_labs * lab_cap)
  # D18
  cap_lab_staff <- ifelse(is.na(capacity$n_hcws), hcws_inpatients_cap,
                              capacity$n_hcws*perc_treating_covid)
  # D19 - capped num cleaners for inpatients






  hcw_lab_caps <- list(
    hcws_inpatients_cap = hcws_inpatients_cap,
    hcws_screening_cap = hcws_screening_cap,
    lab_staff = lab_staff,
    cap_lab_staff = cap_lab_staff,
    hcws_per_bed = hcws_per_bed,
    cleaners_inpatient_cap = cleaners_inpatient_cap,
    hygienists_per_bed = hygienists_per_bed
  )
  return(hcw_lab_caps)
}
#' Dynamic HCW caps
#'
#' @param params From get_country_capacity
#' @param hwfe From WHO ESFT sheet
#' @param patients From patients_weekly
#' @param ambulanciers_per_bed Assumes 1 ambulance per 100 bed hospital w 2
#' operators at all times (3x8 hour shifts) = (2/100)*3 = 0.06, taken from ESFT
#' @param bio_eng_per_bed Assumes 2 biomed engineers on 8 hr shifts per 100 bed
#' hospital, taken from ESFT
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{hygienists_per_bed}{Date the week summarized begins}
#'   \item{hygienists_per_crit_bed}{Date the week summarized ends}
#'   \item{cleaners_inpatient_cap}{Number cleaners required based on hospital
#'   beds in use}
#'   \item{inf_caregiver_inpatient_cap}{Number caregivers required based on
#'   hospital beds in use}
#'   \item{amb_personnel_inpatient_cap}{Ambulanciers required based on hospital
#'   beds in use}
#'   \item{bio_eng_inpatient_cap}{Biomedical engineers required based on
#'   hospital beds in use}
#' }
#' @export
hcw_caps_dynamic <- function(params, # includes specific bed counts
                             patients,
                             ) {

# i dont think these are caps - these are numbers per week for inpatietnst- i feel ike they belogn w hcws_weekly
  inf_caregiver_inpatient_cap <- patients$total_beds_inuse *
    params$n_inf_caregivers_hosp
  amb_personnel_inpatient_cap <- patients$total_beds_inuse * ambulanciers_per_bed
  bio_eng_inpatient_cap <- patients$total_beds_inuse * bio_eng_per_bed

  hcw_caps <- data.frame(
    week_begins = patients$week_begins,
    week_ends = patients$week_ends,
    hcws_per_bed = hcws_per_bed,
    hcws_per_sev_bed = hcws_per_sev_bed,
    hcws_per_crit_bed = hcws_per_crit_bed,
    cleaners_inpatient_cap = cleaners_inpatient_cap,
    inf_caregiver_inpatient_cap = inf_caregiver_inpatient_cap,
    amb_personnel_inpatient_cap = amb_personnel_inpatient_cap,
    bio_eng_inpatient_cap = bio_eng_inpatient_cap
  )

  return(hcw_caps)
}
