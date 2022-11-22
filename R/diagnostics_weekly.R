#' Diagnostics Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params
#' @param patients
#' @param cases
#' @param diagnostic_parameters
#' @param testing_strategy
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{xyz}
#'   \item{week_ends}{xyz}
#'   \item{tests_diagnosis_uncapped_sev_crit}{xyz}
#'   \item{tests_release_uncapped_sev_crit}{xyz}
#'   \item{tests_diagnosis_capped_sev_crit}{xyz}
#'   \item{tests_release_capped_sev_crit}{xyz}
#'   \item{tests_mild}{xyz}
#'   \item{tests_mod}{xyz}
#'   \item{tests_suspected}{xyz}
#'   \item{testing_strategy}{xyz}
#' }
#'
#' @export
diagnostics_weekly <- function(params, # maybe this should already by a subsetted country vector of params?
                               patients, # from patients weekly
                               cases, # from cases weekly
                               diagnostic_parameters, # get_diagnostic_parameters
                               testing_strategy = "all"
                               # cap_lab_staff = FALSE # option to cap lab staff by diagnostic machine capacity
                               # available in early iterations of esft, not in late ones
) {
  data <- merge(patients, cases)
  params <- merge(params, diagnostic_parameters)

  data <- data %>%
    mutate(
      tests_diagnosis_uncapped_sev_crit =
        (new_severe_cases +
          new_critical_cases) * params$tests_diagnosis_sev_crit,
      tests_release_uncapped_sev_crit =
        rem_sev_patients * (
          1 - params$ifr_sev) * params$tests_release_sev_crit +
          rem_crit_patients * (
            1 - params$ifr_crit) * params$tests_release_sev_crit,
      tests_diagnosis_capped_sev_crit =
        (adm_severe_cases_cap +
          adm_critical_cases_cap) * params$tests_diagnosis_sev_crit,
      tests_release_capped_sev_crit =
        discharged_sev_patients * (
          1 - params$ifr_sev) * params$tests_release_sev_crit +
          discharged_crit_patients * (
            1 - params$ifr_crit) * params$tests_release_sev_crit
    )
  if (!(is.null(testing_strategy))) {
    if (testing_strategy == "all") {
      data <- data %>%
        mutate(
          tests_mild = new_mild_cases * params$tests_diagnosis_mild_mod,
          tests_mod = new_mod_cases * params$tests_diagnosis_mild_mod,
          tests_suspected = sus_cases_but_negative * params$tests_diagnosis_mild_mod,
          testing_strategy = testing_strategy
        )
    } else if (testing_strategy == "targeted") {
      data <- data %>%
        mutate(
          tests_mild =
            new_mild_cases * params$tests_diagnosis_mild_mod * params$perc_tested_mild_mod,
          tests_mod =
            new_mod_cases * params$tests_diagnosis_mild_mod * params$perc_tested_mild_mod,
          tests_suspected =
            sus_cases_but_negative * params$tests_diagnosis_mild_mod * params$perc_tested_mild_mod,
          testing_strategy = testing_strategy
        )
    } else {
      stop("Please specify either strategy: all or targeted.")
    }
  }

  data <- data %>%
    select(c(
      week_begins, week_ends, tests_diagnosis_uncapped_sev_crit,
      tests_release_uncapped_sev_crit, tests_diagnosis_capped_sev_crit,
      tests_release_capped_sev_crit, tests_mild, tests_mod,
      tests_suspected, testing_strategy
    ))

  return(data)
}

#' Max tests per day
#'
#' @description Uses the diagnostic capacity data and throughput data to calculate
#' the max number of tests per week and per day.
#'
#' @param diagnostic_capacity from calc_diagnostic_capacity
#'
#' @export
max_tests_per_day <- function(diagnostic_capacity) {
  max_tests_per_week <- sum(diagnostic_capacity$covid_test_capacity)
  max_tests_per_day <- max_tests_per_week / 7
  return(max_tests_per_day)
}
