#' Dynamic HCW caps
#'
#' @param params From get_country_capacity
#' @param hwfe From WHO ESFT sheet
#' @param data From patients_weekly
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
                             hwfe, # from who sheet
                             data,
                             ambulanciers_per_bed = 0.06,
                             bio_eng_per_bed = 0.02
) {

  hygienists_per_sev_bed <- hwfe$patient_t24_sev[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (
    data$sev_beds_inuse / data$total_beds_inuse
  ) * 10

  hygienists_per_crit_bed <- hwfe$patient_t24_crit[
    hwfe$esft_group == "Cleaner"
  ] / 8 * (data$crit_beds_inuse / data$total_beds_inuse) * 10

  hygienists_per_bed <- hygienists_per_sev_bed + hygienists_per_crit_bed

  if (is.null(params$n_hosp_beds)) {
    cleaners_inpatient_cap <- params$beds_covid * hygienists_per_bed
  } else {
    cleaners_inpatient_cap <- params$n_hosp_beds * hygienists_per_bed
  }

  inf_caregiver_inpatient_cap <- data$total_beds_inuse *
    params$n_inf_caregivers_hosp
  amb_personnel_inpatient_cap <- data$total_beds_inuse * ambulanciers_per_bed
  bio_eng_inpatient_cap <- data$total_beds_inuse * bio_eng_per_bed

  hcw_caps <- data.frame(
    hygienists_per_bed = hygienists_per_bed,
    hygienists_per_crit_bed = hygienists_per_crit_bed,
    hygienists_per_sev_bed = hygienists_per_sev_bed,
    cleaners_inpatient_cap = cleaners_inpatient_cap,
    inf_caregiver_inpatient_cap = inf_caregiver_inpatient_cap,
    amb_personnel_inpatient_cap = amb_personnel_inpatient_cap,
    bio_eng_inpatient_cap = bio_eng_inpatient_cap
  )
  return(hcw_caps)
}

#' HCWS Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params From get_country_capacity
#' @param hwfe From WHO ESFT sheet
#' @param data From patients_weekly
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
hcws_weekly <- function(params, # includes specific bed counts
                        hwfe, # from who sheet
                        data,
                        diagnostics_weekly,
                        hcw_caps # static and dynamic
) {
  # screening_hcw_uncapped # this depends on testing
  # screening_hcw_capped # also depends on testing
  # inf_caregiver_isolation # again depends on testing
  # cleaners_lab_capped <- total_labs*hygienists_per_lab # also depends on diagnostics


  # screening_hcw_uncap = screening_hcw_uncap,
  # screening_hcw_cap = screening_hcw_cap,
  # inf_caregiver_isolation = inf_caregiver_isolation,
  # cleaners_lab_cap = cleaners_lab_cap
}
