#' HCWS Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params
#' @param lab_params get_lab_parameters
#' @param tests diagnostics_weekly
#' @param patients patients_weekly
#' @param t_labs From total_labs
#' @param hcw_dyn_caps dataframe of dynamic caps, hcw_caps_dynamic
#' @param hcw_stat_caps single list of caps, hcw_caps_static
#'
#' @return Dataframe of summary
#' \describe{
#'   \item{week_ends}{xyz}
#'   \item{week_begins}{xyz}
#'   \item{hcws_inpatient_capped}{xyz}
#'   \item{hcws_inpatient_uncapped}{xyz}
#'   \item{cleaners_inpatient_capped}{xyz}
#'   \item{amb_personnel_inpatient_capped}{xyz}
#'   \item{bio_eng_inpatient_capped}{xyz}
#'   \item{inf_caregivers_isol_uncapped}{xyz}
#'   \item{lab_staff_capped}{xyz}
#'   \item{cleaners_lab}{xyz}
#' }
#' @export
hcws_weekly <- function(params,
                        lab_params, # get_lab_parameters
                        tests,
                        patients, # diagnostics_weekly
                        t_labs, # total_labs
                        hcw_dyn_caps,
                        hcw_stat_caps # static and dynamic
) {
  df <- merge(patients, tests)
  df <- merge(df, hcw_dyn_caps)

  df <- df %>%
    mutate(
      hcws_inpatient_uncapped = total_beds_inuse * hcws_per_bed,
      hcws_inpatient_capped = min(
        total_beds_inuse * hcws_per_bed,
        hcw_stat_caps$hcws_inpatients_cap
      ),
      cleaners_inpatient_capped = min(
        total_beds_inuse * hygienists_per_bed,
        cleaners_inpatient_cap
      ),
      amb_personnel_inpatient_capped = amb_personnel_inpatient_cap,
      bio_eng_inpatient_capped = bio_eng_inpatient_cap,
      # it's basically calculated this way i think to accomodate for mild
      # + mod testing in outpatient setting
      inf_caregivers_isol_uncapped = (
        tests_mild + tests_mod) * params$n_inf_caregivers_isol,
      # for the all testing strategy, capped by testing capacity (confusing - needs simplificaiton)
      lab_staff_capped = min(
        t_labs * lab_params$lab_staff_per_lab, params$n_labs
      ),
      cleaners_lab = t_labs * lab_params$hygienists_per_lab
    ) %>%
    select(c(
      week_ends, week_begins, hcws_inpatient_capped,
      hcws_inpatient_uncapped, cleaners_inpatient_capped,
      amb_personnel_inpatient_capped, bio_eng_inpatient_capped,
      inf_caregivers_isol_uncapped, lab_staff_capped, cleaners_lab
    ))

  return(df)
}
