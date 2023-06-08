#' HCW Caps
#'
#' @description This function calculates the static HCW caps found in the
#' `Weekly Summary` in the ESFT.
#'
#' Here is the description of the additional patient calculations:
#'
#' * crit_patients_nocap - ICU_demand
#' * sev_patients_nocap - hospital_demand
#' * mod_patients_nocap - cum_mod_cases - cum_rem_mod_cases
#' * mild_patients_nocap - cum_mild_cases - cum_rem_mild_cases
#' * crit_beds_inuse - crit_patients_nocap capped by beds allocated to critical
#' COVID
#' #'#' * cases_screened_per_hcw_per_day - screening/triage ratio is based on the
#' assumption that each screening/triage takes approximately 48 minutes, which
#' is 10 consultations per 8-hour shift ((8 x 60)/48); default = 10
#'
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
#'
#' @param params From get_parameters
#' @param capacity Country capacity, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param hwfe From WHO ESFT sheet
#' @param patients From patients_weekly
#'
#' From old statis caps - UPDATE
#' @return List of caps
#' \describe{
#'   \item{iso3c}{Iso3c code these HCW caps are associated with.}
#'   \item{bed_cap}{Hospital bed cap (equals number of beds per country, as
#'   found in the capacity function, which takes it from the World Bank)}
#'   \item{cases_screened_per_hcw_per_day}{}
#'   \item{cleaners_inpatient_cap}{}
#'   \item{hcws_inpatients_cap}{}
#'   \item{hcws_per_bed}{}
#'   \item{hcws_per_inpatient}{}
#'   \item{hcws_per_outpatient}{}
#'   \item{hcws_screening_cap}{}
#'   \item{hygienists_per_bed}{}
#'   \item{lab_staff_cap}{}
#'   \item{perc_crit_cases}{}
#'   \item{perc_screening_covid}{}
#'   \item{perc_sev_cases}{}
#'   \item{perc_treating_covid}{}
#'   \item{prob_inpatient}{}
#'   \item{prob_outpatient}{}
#'   \item{ratio_hcws_inpatient_outpatient}{}
#' }
#'
#' @export
hcw_caps <- function(params, # STATIC
                     capacity, # changes by country DYNAMIC
                     throughput, # also from WHO sheet
                     hwfe, # from who sheet
                     patients) { # dynamic
  # STATIC
  # cases screened per HCW per day - Inputs, I78
  cases_screened_per_hcw_per_day <- 8/sum(
    hwfe$patient_t24_screen[
      hwfe$esft_group == "HCW"
    ]
  )
  # STATIC
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
  # STATIC
  lab_cap <- mean(
    covid_capacity_high_throughput,
    covid_capacity_near_patient,
    covid_capacity_manual
  )
  # back calculations, c32 & also c27
  # DYNAMIC
  lab_staff <- capacity$n_labs
  # c19 bed cap = I67 in inputs tab
  # DYNAMIC
  bed_cap <- capacity$n_hosp_beds # this has already been calculated in country capacity

  # D18 WS
  # DYNAMIC
  lab_staff_cap <-ifelse(is.na(lab_cap), lab_staff,
                         lab_staff*lab_cap)

  # num hygienists per bed, I73 - DEPENDS ON OUTPUT - patients
  hygienists_per_sev_bed <- hwfe$patient_t24_sev[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (
    (sum(patients$sev_beds_inuse) / sum(patients$total_beds_inuse))
  ) * 10
  # there are many fewer crit beds than sev beds so, sometimes this stays
  hygienists_per_crit_bed <- hwfe$patient_t24_crit[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (
    (sum(patients$crit_beds_inuse) / sum(patients$total_beds_inuse))
  ) * 10

  hygienists_per_bed <- hygienists_per_sev_bed + hygienists_per_crit_bed

  # INPUTS - num hcws per bed, I72 - DEPENDS ON OUTPUT - patients -----------------------------------------------
  hcws_per_sev_bed <- sum(
    hwfe$patient_t24_sev[
      hwfe$esft_group == "HCW"
    ]
  ) / 8 * (
    (sum(patients$sev_beds_inuse) / sum(patients$total_beds_inuse))
  )

  hcws_per_crit_bed <- sum(
    hwfe$patient_t24_crit[
      hwfe$esft_group == "HCW"
    ]
  ) / 8 * (
    (sum(patients$crit_beds_inuse) / sum(patients$total_beds_inuse))
  )

  hcws_per_bed <- hcws_per_sev_bed + hcws_per_crit_bed # -----------------------------------------------

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
  # back calculations - C42 - HCWS required per outpatient -----------------------------------------------
  hcws_per_outpatient <- round(1/cases_screened_per_hcw_per_day, 3)
  # back calculations - C43 - HCWS required per inpatient -----------------------------------------------
  hcws_per_inpatient <- hcws_per_bed
  # back calculations - C44 - per new case, ratio of HCWs for inpatient vs outpatient -----------------------------------------------
  ratio_hcws_inpatient_outpatient <- round((prob_inpatient*hcws_per_inpatient)/(prob_inpatient*hcws_per_inpatient + prob_outpatient*hcws_per_outpatient), 3)

  # inputs - I66 - % HCW treating hospitalized covid inpatients --------------------------------------------------
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
    iso3c = capacity$iso3c,
    bed_cap = bed_cap,
    cases_screened_per_hcw_per_day = cases_screened_per_hcw_per_day,
    cleaners_inpatient_cap = cleaners_inpatient_cap,
    hcws_inpatients_cap = hcws_inpatients_cap,
    hcws_per_bed = hcws_per_bed,
    # hcws_per_crit_bed = hcws_per_crit_bed,
    # hcws_per_sev_bed = hcws_per_sev_bed,
    hcws_per_inpatient = hcws_per_inpatient, # DOUBLE CHECK IF ACTUALLY USED
    hcws_per_outpatient = hcws_per_outpatient,
    hcws_screening_cap = hcws_screening_cap,
    hygienists_per_bed = hygienists_per_bed, # not sure if the by severity breakdown needed
    # hygienists_per_crit_bed = hygienists_per_crit_bed,
    # hygienists_per_sev_bed = hygienists_per_sev_bed,
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
