#' HCWS Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff (except for
#' the screening/triage section, which depends on diagnostics_weekly and is
#' calculated separately in screening_hcws_weekly).
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params from get_parameters
#' @param capacity from get_country_capacity
#' @param lab_params get_lab_parameters
#' @param tests diagnostics_weekly
#' @param patients patients_weekly
#' @param t_labs From total_labs
#' @param hcw_dyn_caps dataframe of dynamic caps, hcw_caps_dynamic
#' @param hcw_stat_caps single list of caps, hcw_caps_static
#'
#' @return Dataframe of summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{hcws_inpatient_capped}{HCWs for inpatients, capped by reported HCWs}
#'   \item{hcws_inpatient_uncapped}{HCWs for inpatients, uncapped - total beds
#'   in use times number of HCWs per bed}
#'   \item{cleaners_inpatient_capped}{Cleaners for inpatients, i.e. number of
#'   beds available for COVID-19 (or if unavaiable, just number of beds) times
#'   ideal number of cleaners ber bed}
#'   \item{amb_personnel_inpatient_capped}{Total ambulance personnel for
#'   inpatients - number of beds in use per week times ambulanciers per bed}
#'   \item{bio_eng_inpatient_capped}{Total biomedical engineers for inpatients -
#'   number of beds in use per week times biomedical engineers per bed}
#'   \item{inf_caregivers_isol_uncapped}{Total uncapped informal caregivers for
#'   isolating cases - total mild and moderate tested times number of
#'   informal caregivers per quarantining or isolating case}
#'   \item{lab_staff_capped}{Capped total lab staff - so total labs times staff
#'   per lab, capped by number of lab staff reported y country}
#'   \item{cleaners_lab}{Total cleaners for labs  - so total labs times cleaners
#'   per lab}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
hcws_weekly <- function(params, # from get_parameters
                        capacity, # from get_country_capacity
                        lab_params, # get_lab_parameters
                        tests, # from diagnostics_weekly
                        patients, # patients_weekly
                        t_labs, # total_labs
                        hcw_dyn_caps,
                        hcw_stat_caps # static and dynamic
) {
  data <- merge(patients, tests)
  data <- merge(data, hcw_dyn_caps)
  params <- merge(params, capacity)

  data <- data %>%
    dplyr::mutate(
      hcws_inpatient_uncapped = .data$total_beds_inuse *
        .data$hcws_per_bed,
      hcws_inpatient_capped = min(
        .data$total_beds_inuse * .data$hcws_per_bed,
        hcw_stat_caps$hcws_inpatients_cap
      ),
      cleaners_inpatient_capped = min(
        .data$total_beds_inuse * .data$hygienists_per_bed,
        .data$cleaners_inpatient_cap
      ),
      amb_personnel_inpatient_capped = .data$amb_personnel_inpatient_cap,
      bio_eng_inpatient_capped = .data$bio_eng_inpatient_cap,
      # it's basically calculated this way i think to accommodate for mild
      # + mod testing in outpatient setting
      inf_caregivers_isol_uncapped = (
        .data$tests_mild + .data$tests_mod) * params$n_inf_caregivers_isol,
      # for the all testing strategy, capped by testing capacity
      # (confusing - needs simplification)
      lab_staff_capped = min(
        t_labs * lab_params$lab_staff_per_lab, params$n_labs
      ),
      cleaners_lab = t_labs * lab_params$hygienists_per_lab
    ) %>%
    dplyr::select(c(
      week_ends, week_begins, hcws_inpatient_capped,
      hcws_inpatient_uncapped, cleaners_inpatient_capped,
      amb_personnel_inpatient_capped, bio_eng_inpatient_capped,
      inf_caregivers_isol_uncapped, lab_staff_capped,
      cleaners_lab
    ))

  return(data)
}
