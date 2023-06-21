#' HCW Caps
#'
#' @description This function calculates the HCW caps found in the
#' `Weekly Summary`, `Inputs`, and `Back Calculations` tabs in the ESFT.
#'
#' Here is the description of the bed cap calculations:
#' * bed_cap - Bed cap to calculate max bed occupancy, equal to total number of
#' beds available per country.
#' * cases_screened_per_hcw_per_day - Screening/triage ratio is based on the
#' assumption that each screening/triage takes approximately 48 minutes, which
#' is 10 consultations per 8-hour shift.
#' * cleaners_inpatient_cap - Max number of estimated cleaners needed, refers
#' to Cleaners and Helpers (ILO ISCO code 9112).
#' * hcws_inpatients_cap - Max number of estimated healthcare workers, refers
#' to all medical practitioners, including physicians, nursing professionals,
#' and paramedical practitioners (ILO ISCO codes 2240, 2211, 2212, 2221, 3221,
#' 5321, 3256)
#' * hcws_per_bed - Average number of health care workers per bed, derived from
#' bed occupancy and typical/recommended hours per bed per health care worker.
#' * hcws_per_inpatient - Same as hcws_per_bed.
#' * hcws_per_outpatient - Number of health care workers required per outpatient
#' (screening) case, or the inverse of cases screened per health care worker per
#' day.
#' * hcws_screening_cap - Max number of estimated health care workers
#' dedicated to screening COVID-19 cases.
#' * hygienists_per_bed - Average number of hygienists per bed, derived from bed
#' occupancy and typical/recommended hours per bed per hygienist.
#' * lab_staff_cap - Estimated max number of possible lab staff - either derived
#' from the different machines and their estimated COVID-19 capacity or derived
#' from the total number of available lab staff.
#' * perc_crit_cases - Percent of all patients occupying beds who are of
#' critical severity.
#' * perc_screening_covid - Percent of all HCWs dedicated to screening COVID-19
#' cases (usually the percent leftover after accounting for those who are and
#' are not dedicated to COVID-19 response)
#' * perc_sev_cases - Percent of all patients occupying beds who are of severe
#' case severity.
#' * perc_treating_covid - Percent of HCWs dedicated to responding to COVID-19.
#' * prob_inpatient - Probability that a COVID-19 case is inpatient, i.e. in a
#' care facility.
#' * prob_outpatient - Probability that a COVID-19 case is outpatient, i.e. that
#' it is not in a care facility.
#' * ratio_hcws_inpatient_outpatient - Per new case, ratio of HCWs for inpatient
#' vs. outpatient
#'
#' @param params From get_parameters
#' @param capacity Country capacity, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param hwfe From WHO ESFT sheet
#' @param patients From patients_weekly
#' @param overrides a named list of hcw_cap values to use instead of defaults
#'
#' @return List of caps
#' \describe{
#'   \item{iso3c}{Iso3c code these HCW caps are associated with.}
#'   \item{bed_cap}{Hospital bed cap (equals number of beds per country, as
#'   found in the capacity function, which takes it from the World Bank)}
#'   \item{cases_screened_per_hcw_per_day}{Calculates the number of cases that
#'   could theoretically be screened per HCW per day based on standardized
#'   recommendations of hours spent per day per patient per HCW grouping (which
#'   was developed using HWFE tool methodology and consultation with clinical
#'   leads)}
#'   \item{cleaners_inpatient_cap}{Number of beds times the hygienists per bed
#'   in a hospital setting}
#'   \item{hcws_inpatients_cap}{Percent of HCWs treating COVID-19 patients times
#'   the number of HCWs}
#'   \item{hcws_per_bed}{Number of HCWs per bed, weighted by the total number of
#'   beds in use by severity and the time recommended for HCWs to spend at beds
#'   by severity}
#'   \item{hcws_per_inpatient}{Same as hcws_per_bed}
#'   \item{hcws_per_outpatient}{1/Number of cases screened per HCW per day}
#'   \item{hcws_screening_cap}{Percent of HCWs screening COVID-19 times the
#'   number of HCWs}
#'   \item{hygienists_per_bed}{Number of hygienists per bed, weighted by the
#'   total number of beds in use by severity and the time recommended for
#'   hygienists to spend at beds by severity}
#'   \item{lab_staff_cap}{If there are machines dedicated to COVID-19 test
#'   processing, its the average of their percent capacity allocation to COVID
#'   multiplied by the number of lab staff. If there are no estimates for
#'   percent allocation to COVID, it is equal to the total number of lab staff}
#'   \item{perc_crit_cases}{Percent of total critical beds in use divided by
#'   the total beds in use over the forecasting period}
#'   \item{perc_screening_covid}{Percent of HCWs dedicated to COVID-19
#'   screening, which is equal to the leftover Percent after accounting for
#'   those HCWs not dedicated to COVID-19 and those allocated to COVID-19
#'   response}
#'   \item{perc_sev_cases}{Percent of total severe beds in use divided by
#'   the total beds in use over the forecasting period}
#'   \item{perc_treating_covid}{Percent of HCWs dedicated to COVID-19
#'   response - this depends on the total patients in beds and the number of
#'   HCWs needed to respond to them during the forecasting period}
#'   \item{prob_inpatient}{Probability the patient in inpatient (severe or
#'   critical)}
#'   \item{prob_outpatient}{Probability the patient in inpatient (mild or
#'   moderate)}
#'   \item{ratio_hcws_inpatient_outpatient}{Ratio of inpatient dedicated HCWs
#'   to inpatient and outpatient HCWs}
#' }
#'
#' @export
hcw_caps <- function(params, # STATIC
                     capacity, # changes by country DYNAMIC
                     throughput, # also from WHO sheet
                     hwfe, # from who sheet
                     patients,
                     overrides = list()) { # dynamic
  # STATIC
  # cases screened per HCW per day - Inputs, I78
  cases_screened_per_hcw_per_day <- 8 / sum(
    hwfe$patient_t24_screen[
      hwfe$esft_group == "HCW"
    ]
  )
  # DYNAMIC - depends on covid capacity inputs
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
  lab_cap <- mean(c(
    covid_capacity_high_throughput,
    covid_capacity_near_patient,
    covid_capacity_manual
  ))
  # back calculations, c32 & also c27
  # DYNAMIC
  lab_staff <- capacity$n_labs
  # c19 bed cap = I67 in inputs tab
  # DYNAMIC # this has already been calculated in country capacity
  bed_cap <- capacity$n_hosp_beds

  # D18 WS
  # DYNAMIC
  lab_staff_cap <- ifelse(is.na(lab_cap), lab_staff,
    lab_staff * lab_cap
  )

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

  # INPUTS - num hcws per bed, I72 - DEPENDS ON OUTPUT - patients
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

  hcws_per_bed <- hcws_per_sev_bed + hcws_per_crit_bed # ----------------------

  # back calculations - capped number cleaners for inpatient C33, same as ws D19
  if (is.null(capacity$n_hosp_beds)) {
    cleaners_inpatient_cap <- capacity$beds_covid * hygienists_per_bed
  } else {
    cleaners_inpatient_cap <- capacity$n_hosp_beds * hygienists_per_bed
  }

  # back calculations - c36 - of critical/severe, % of cases that are critical
  perc_crit_cases <- round(sum(patients$crit_beds_inuse) /
    sum(patients$total_beds_inuse), 5)
  # back calculations - c37 - of critical/severe, % of cases that are severe
  perc_sev_cases <- round(1 - perc_crit_cases, 5)
  # add check here that they sum to 1 / correspond

  # back calculations - C40 - probability of a new case being outpatient
  prob_outpatient <- params$mild_i_proportion + params$mod_i_proportion
  # back calculations - C41 - probability of a new case being inpatient
  prob_inpatient <- params$sev_i_proportion + params$crit_i_proportion
  # back calculations - C42 - HCWS required per outpatient
  hcws_per_outpatient <- round(1 / cases_screened_per_hcw_per_day, 5)
  # back calculations - C43 - HCWS required per inpatient
  hcws_per_inpatient <- hcws_per_bed
  # back calculations - C44 - per new case, ratio of HCWs for
  # inpatient : outpatient
  ratio_hcws_inpatient_outpatient <- round(
    (prob_inpatient * hcws_per_inpatient) /
      (prob_inpatient * hcws_per_inpatient +
        prob_outpatient * hcws_per_outpatient), 5
  )

  # inputs - I66 - % HCW treating hospitalized covid inpatients ---------------
  perc_treating_covid <- round(
    ratio_hcws_inpatient_outpatient * (1 - params$perc_hcws_not_covid), 5
  )
  # inputs - I67 - % HCW screening/triaging suspected covid-19 cases
  perc_screening_covid <- round(
    1 - params$perc_hcws_not_covid - perc_treating_covid, 5
  )

  # BACK CALCULATIONS - - - -
  # the caps are recalculated based on
  # back calculations C30 - D16 ws
  hcws_inpatients_cap <- round(perc_treating_covid * capacity$n_hcws, 5)
  # n_hcws = num nurses + num doctors,C31 - D17 ws
  hcws_screening_cap <- round(perc_screening_covid * capacity$n_hcws, 5)

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
    hcws_per_inpatient = hcws_per_inpatient, # DOUBLE CHECK IF ACTUALLY USED
    hcws_per_outpatient = hcws_per_outpatient,
    hcws_screening_cap = hcws_screening_cap,
    hygienists_per_bed = hygienists_per_bed,
    # this one is lab staff avail. for covid - one in WS
    lab_staff_cap = lab_staff_cap,
    perc_crit_cases = perc_crit_cases,
    perc_screening_covid = perc_screening_covid,
    perc_sev_cases = perc_sev_cases,
    perc_treating_covid = perc_treating_covid,
    prob_inpatient = prob_inpatient,
    prob_outpatient = prob_outpatient,
    ratio_hcws_inpatient_outpatient = ratio_hcws_inpatient_outpatient
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(hcw_caps_list))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    hcw_caps_list[[name]] <- overrides[[name]]
  }

  return(hcw_caps_list)
}
