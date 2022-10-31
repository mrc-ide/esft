#' Static HCW Caps
#'
#' @description This function calculates the static HCW caps found in the
#' `Weekly Summary` in the ESFT.
#'
#' @param params Country capacity params, get_country_capacity
#' @param throughput Throughput dataframe, from r data file - can be altered
#' @param cap_lab_staff TRUE/FALSE. Option to set a cap on lab staff based on
#' percent of diagnostic machinery allocated to COVID
#'
#' @return List of caps
#' \describe{
#'   \item{hcws_inpatients_cap}{Date the week summarized begins}
#'   \item{hcws_screening_cap}{Date the week summarized ends}
#'   \item{lab_staff}{Lab staff available}
#'   \item{cap_lab_staff}{TRUE/FALSE, whether you want to cap lab staff by
#'   machines allocated to COVID or not}
#' }
#'
#' @export
hcw_caps_static <- function(params,
                            throughput,
                            cap_lab_staff = FALSE
                            # option to cap lab staff by percent allocated to covid
                            # percent allocated to covid is the average of the percent
                            # machine type allocation to covid testing
) {
  # add exists part here

  hcws_inpatients_cap <- params$perc_hcws_treat_covid * params$n_hcws
  # n_hcws = num nurses + num doctors
  hcws_screening_cap <- params$perc_hcws_screen_covid * params$n_hcws

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

  if (cap_lab_staff == FALSE) {
    lab_staff <- params$n_labs
  } else {
    lab_cap <- mean(
      covid_capacity_high_throughput,
      covid_capacity_near_patient,
      covid_capacity_manual
    )
    lab_staff <- params$n_labs * lab_cap
  }
  hcw_lab_caps <- list(
    hcws_inpatients_cap = hcws_inpatients_cap,
    hcws_screening_cap = hcws_screening_cap,
    lab_staff = lab_staff,
    cap_lab_staff = cap_lab_staff
  )
  return(hcw_lab_caps)
}

#' Dynamic HCW caps
#'
#' @description This function calculates some of the dynamic HCW caps found in
#' the `Weekly Summary` tab in the ESFT. These caps depend on weekly cases, and
#' so change with time.
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


  hcw_caps <- list(
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
