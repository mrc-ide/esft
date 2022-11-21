#' HCWS Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params From get_country_capacity
#' @param data From diagnostics_weekly
#' @param total_labs From total_labs
#' @param hcw_caps Merged from both dynamic and static hcw caps
#'
#' @return Dataframe of summary
#' \describe{
#'   \item{hygienists_per_bed}{Date the week summarized begins}
#'   \item{hygienists_per_crit_bed}{Date the week summarized ends}
#' }
#' @export
hcws_weekly <- function(params, # includes specific bed counts and lab parameters
                        data,# diagnostics_weekly
                        total_labs,
                        hcw_caps # static and dynamic
) {
  # screening_hcw_uncapped # this depends on testing
  # screening_hcw_capped # also depends on testing
  # inf_caregiver_isolation # again depends on testing
  # cleaners_lab_capped <- total_labs*hygienists_per_lab # also depends on diagnostics

  df <- cbind(data, hcw_caps, total_labs)

  df <- df %>%
    mutate(
      hcws_inpatient_uncapped = total_beds_inuse * hcws_per_bed,
      hcws_inpatient_capped = min(
        total_beds_inuse * hcws_per_bed,
        hcws_inpatients_cap
      ),
      cleaners_inpatient_capped = min(
        total_beds_inuse * hygienists_per_bed,
        cleaners_inpatient_cap
      ),
      amb_personnel_inpatient_capped = amb_personnel_inpatient_cap,
      bio_eng_inpatient_capped = bio_eng_inpatient_cap,
      # it's basically calculated this way i think to accomodate for mild
      # + mod testing in outpatient setting
      inf_caregivers_isol_uncapped = (tests_mild+tests_mod)*n_inf_caregivers_isol,
      # for the all testing strategy, capped by testing capacity (confusing - needs simplificaiton)
      lab_staff_capped = min(total_labs*lab_staff_per_lab, lab)
    )

  return(df)
  # screening_hcw_uncap = screening_hcw_uncap,
  # screening_hcw_cap = screening_hcw_cap,
  # inf_caregiver_isolation = inf_caregiver_isolation,
  # cleaners_lab_cap = cleaners_lab_cap
}
