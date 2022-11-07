#' Diagnostics Weekly
#'
#' i think i want to rewrite to have a factor vriable w targeted vs all strategy
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params
#' @param hwfe
#' @param diagnostic_parameters
#' @param cap_lab_staff
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{rem_severe_cases}{xyz}
#'   \item{rem_critical_cases}{xyz}
#' }
#'
#' @export
diagnostics_weekly <- function(params, # maybe this should already by a subsetted country vector of params?
                        hwfe,
                        data, # from patients weekly
                        diagnostic_capacity, # from calc_diagnostic_capacity
                        diagnostic_parameters, # get_diagnostic_parameters
                        cap_lab_staff = FALSE # option to cap lab staff by diagnostic machine capacity
                        # available in early iterations of esft, not in late ones
)
{
  data <- merge(data, diagnostic_parameters)
  data <- merge(data, params)
  data <- data %>%
    mutate(
      tests_diagnosis_uncapped_sev_crit =
        (new_severe_cases +
           new_critical_cases)*tests_diagnosis_sev_crit,
      tests_release_uncapped_sev_crit =
        rem_sev_patients*(
          1 - ifr_sev)*tests_release_sev_crit +
           rem_crit_patients*(
             1-ifr_crit)*tests_release_sev_crit,
      tests_diagnosis_capped_sev_crit =
        (adm_severe_cases_cap +
           adm_critical_cases_cap)*tests_diagnosis_sev_crit,
      tests_release_capped_sev_crit =
        discharged_sev_patients*(
          1 - ifr_sev)*tests_release_sev_crit +
           discharged_crit_patients*(
             1 - ifr_crit)*tests_release_sev_crit,
      tests_all_mild = new_mild_cases*tests_diagnosis_mild_mod,
      tests_all_mod = new_mod_cases*tests_diagnosis_mild_mod,
      tests_all_suspected = sus_cases_but_negative*tests_diagnosis_mild_mod,
      tests_targeted_mild =
        new_mild_cases*tests_diagnosis_mild_mod*perc_tested_mild_mod,
      tests_targeted_mod =
        new_mod_cases*tests_diagnosis_mild_mod*perc_tested_mild_mod,
      tests_targeted_suspected =
        sus_cases_but_negative*tests_diagnosis_mild_mod*perc_tested_mild_mod,

    )
  # DEPENDING ON TESTING STRATEGY AND MAX TESTS PER DAY< GET SUMMARY
}

#' Max tests per day
#'
#' @description Uses the diagnostic capacity data and throughput data to calculate
#' the max number of tests per week and per day.
#'
#' @export
max_tests_per_day <- function(diagnostic_capacity) {
  max_tests_per_week <- sum(diagnostic_capacity$covid_test_capacity)
  max_tests_per_day <- max_tests_per_week/7
  return(max_tests_per_day)
}

#' Summary diagnostics weekly
#'
#' @export
diagnostics_summary <- function(diagnostics_weekly,
                                testing_strategy,
                                max_tests_per_day) {

}
