#' Diagnostics Weekly
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params From get_parameters
#' @param patients From patients_weekly
#' @param cases From cases_weekly
#' @param diagnostic_parameters From get_diagnostic_parameters
#' @param testing_scenario List of 8 testing scenario parameters, from
#' set_testing_strategy()
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{tests_diagnosis_uncapped_sev_crit}{Tests required if all severe and
#'   critical patients tested for diagnosis, regardless of capacity limits.
#'   Number of tests required per case specified in diagnostic_parameters.}
#'   \item{tests_release_uncapped_sev_crit}{Tests performed if all severe and
#'   critical patients tested for release, regardless of capacity limits.
#'   Number of tests per case specified in diagnostic_parameters.}
#'   \item{tests_diagnosis_capped_sev_crit}{Tests for diagnosis for severe
#'   and critical patients capped by hospital capacity/number of severe and
#'   critical patients admitted to hospital}
#'   \item{tests_release_capped_sev_crit}{Tests for release for severe and
#'   critical patients capped by hospital capacity and IFR/number of severe and
#'   critical patients discharged from hospital}
#'   \item{tests_mod}{Number of tests for moderate patient diagnosis, given
#'   testing strategy}
#'   \item{tests_mild}{Number of tests for mild patient diagnosis, given testing
#'   strategy}
#'   \item{tests_suspected}{Number of tests for suspected but negative cases}
#'   \item{testing_strategy}{Testing strategy, string}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
diagnostics_weekly <- function(params,
                               patients, # from patients weekly
                               cases, # from cases weekly
                               diagnostic_parameters,
                               testing_scenario) { # from set_testing_strategy()
  data <- merge(patients, cases)
  params <- merge(params, diagnostic_parameters)
  params <- merge(params, testing_scenario)

  # this section produces results that are wildly different from the spreadsheet
  # why is that?
  data <- data %>%
    dplyr::mutate(
      tests_diagnosis_uncapped_sev_crit =
        (.data$new_severe_cases +
          .data$new_critical_cases) * params$tests_diagnosis_sev_crit,
      tests_release_uncapped_sev_crit =
        .data$rem_sev_patients * (
          1 - params$ifr_sev) * params$tests_release_sev_crit +
          .data$rem_crit_patients * (
            1 - params$ifr_crit) * params$tests_release_sev_crit,
      tests_diagnosis_capped_sev_crit =
        (.data$sev_patients_admitted_cap +
          .data$crit_patients_admitted_cap) * params$tests_diagnosis_sev_crit,
      tests_release_capped_sev_crit =
        .data$discharged_sev_patients * (
          1 - params$ifr_sev) * params$tests_release_sev_crit +
          .data$discharged_crit_patients * (
            1 - params$ifr_crit) * params$tests_release_sev_crit
    )

  if (params$strategy == "all") {
    data <- data %>%
      dplyr::mutate(
        tests_mild = .data$new_mild_cases * params$tests_diagnosis_mild_mod,
        tests_mod = .data$new_mod_cases * params$tests_diagnosis_mild_mod,
        tests_suspected = .data$sus_cases_but_negative *
          params$tests_diagnosis_mild_mod,
        testing_strategy = params$strategy
      )
  } else if (params$strategy == "targeted") {
    data <- data %>%
      dplyr::mutate(
        tests_mild =
          .data$new_mild_cases * params$tests_diagnosis_mild_mod *
            params$perc_tested_mild_mod,
        tests_mod =
          .data$new_mod_cases * params$tests_diagnosis_mild_mod *
            params$perc_tested_mild_mod,
        tests_suspected =
          .data$sus_cases_but_negative * params$tests_diagnosis_mild_mod *
            params$perc_tested_mild_mod,
        testing_strategy = params$strategy
      )
  }

  data <- data %>%
    dplyr::select(c(
      week_begins, week_ends,
      tests_diagnosis_uncapped_sev_crit,
      tests_release_uncapped_sev_crit,
      tests_diagnosis_capped_sev_crit,
      tests_release_capped_sev_crit, tests_mod, tests_mild,
      tests_suspected, testing_strategy
    ))

  return(data)
}

#' Max tests per day
#'
#' @description Uses the country specific diagnostic capacity estimates to
#' calculate the max number of tests per day.
#'
#' @param diagnostic_capacity from calc_diagnostic_capacity
#'
#' @export
max_tests_per_day <- function(diagnostic_capacity) {
  max_tests_per_week <- sum(diagnostic_capacity$covid_test_capacity)
  max_tests_per_day <- max_tests_per_week / 7
  return(max_tests_per_day)
}
