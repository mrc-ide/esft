#' Produce Weekly Summary of HCWs
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params
#' @param hwfe
#' @param data
#' @par
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{rem_severe_cases}{xyz}
#'   \item{rem_critical_cases}{xyz}
#' }
#' @import dplyr
#' @importFrom data.table first
#' @import countrycode
#'
#' @export
hcws_weekly <- function(params, # maybe this should already by a subsetted country vector of params?
                        hwfe,
                        data,
                        country_capacity,
                        diagnostic_parameters,
                        lab_params,
                        cap_lab_staff = FALSE, # option to cap lab staff by diagnostic machine capacity
                        # available in early iterations of esft, not in late ones
                        hcws_in_region = NULL) # seems weird to have this at all
                        # - i think in calculation, it served as both a way to get a proxy value in
# and also allow for manual tinkering
{
  # add exists part here
  if (is.null(hcws_in_region)) {
    hcws_inpatients_capped <- params$perc_hcws_treat_covid * params$n_hcws
    # n_hcws = num nurses + num doctors
    hcws_screening_capped <- params$perc_hcws_screen_covid * params$n_hcws
  } else { # maybe if n_hcws is null?
    hcws_inpatients_capped <- params$perc_hcws_treat_covid * hcws_in_region
    # n_hcws = num nurses + num doctors
    hcws_screening_capped <- params$perc_hcws_screen_covid * hcws_in_region
  }

  if (cap_lab_staff == FALSE) {
    lab_staff <- params$n_labs
  } else {
    lab_cap <- mean(
      diagnostic_parameters$covid_capacity_high_throughput,
      diagnostic_parameters$covid_capacity_near_patient,
      diagnostic_parameters$covid_capacity_manual
    )
    lab_staff <- params$n_labs * lab_cap
  }

  # wrap this in error message?
  # also what bout mod - we have data for this?
  hygienists_per_sev_bed <- hwfe$patient_t24_sev[hwfe$esft_group == "Cleaner"] / 8 * (data$sev_beds_inuse / data$total_beds_inuse) * 10
  hygienists_per_crit_bed <- hwfe$patient_t24_crit[hwfe$esft_group == "Cleaner"] / 8 * (data$crit_beds_inuse / data$total_beds_inuse) * 10
  hygienists_per_bed <- hygienists_per_sev_bed + hygienists_per_crit_bed # here we need HWFE

  if (is.null(params$n_hosp_beds)) {
    cleaners_inpatient_capped <- params$beds_covid * hygienists_per_bed
  } else {
    cleaners_inpatient_capped <- params$n_hosp_beds * hygienists_per_bed
  }

  ambulanciers_per_bed <- (2 / 100) * 3 # 1 ambulance per 100 bed hospital w 2 operators at all times (3x8 hour shifts)
  bio_eng_per_bed <- 0.02 # assumes 2 biomed engineers on 8 hr shifts per 100 bed hospital

  inf_caregiver_inpatient_capped <- data$total_beds_inuse * params$n_inf_caregivers_hosp
  amb_personnel_inpatient_capped <- data$total_beds_inuse * ambulanciers_per_bed
  bio_eng_inpatient_capped <- data$total_beds_inuse * bio_eng_per_bed

  # screening_hcw_uncapped # this depends on testing
  # screening_hcw_capped # also depends on testing
  # inf_caregiver_isolation # again depends on testing
  # cleaners_lab_capped <- total_labs*hygienists_per_lab # also depends on diagnostics

  hcws <- list(
    hcws_inpatients_capped = hcws_inpatients_capped,
    hcws_screening_capped = hcws_screening_capped,
    lab_staff = lab_staff,
    cleaners_inpatient_capped = cleaners_inpatient_capped,
    inf_caregiver_inpatient_capped = inf_caregiver_inpatient_capped,
    amb_personnel_inpatient_capped = amb_personnel_inpatient_capped,
    bio_eng_inpatient_capped = bio_eng_inpatient_capped,
    screening_hcw_uncapped = screening_hcw_uncapped,
    screening_hcw_capped = screening_hcw_capped,
    inf_caregiver_isolation = inf_caregiver_isolation,
    cleaners_lab_capped = cleaners_lab_capped
  )
}
