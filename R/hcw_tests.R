#' Screening HCWs per week, capped and uncapped
#'
#' @param diagnostics_weekly From diagnostics_weekly
#' @param hcw_caps From hcw_caps
#' @param capacity Country capacity, get_country_capacity
#'
#' @return Dataframe of summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{screening_hcw_uncapped}{Number of HCWs required for screening/triage,
#'   based on tests needed for outpatients (i.e. suspected negatives, mild and
#'   moderate cases) per day, and number of cases one HCW can screen a day}
#'   \item{screening_hcw_capped}{Number of HCWs allocated to screening/triage:
#'   minimum of uncapped HCWs as calculated above and number of HCWs available/
#'   reported per country and the percentage of HCWs allocated to screening,
#'   which is a parameter that can be modified in get_parameters}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
screening_hcws_weekly <- function(diagnostics_weekly,
                                  hcw_caps,
                                  capacity) {
  data <- diagnostics_weekly
  data <- data %>%
    dplyr::mutate(screening_hcw_uncapped = (
      .data$tests_suspected + .data$tests_mild + .data$tests_mod) / (
      7 * hcw_caps$cases_screened_per_hcw_per_day
    ))

  data <- data %>%
    dplyr::mutate(
      screening_hcw_capped = ifelse(
        .data$screening_hcw_uncapped > hcw_caps$hcws_screening_cap,
        hcw_caps$hcws_screening_cap,
        .data$screening_hcw_uncapped
      )
    ) %>%
    dplyr::select(c(
      week_begins, week_ends, screening_hcw_uncapped,
      screening_hcw_capped
    ))

  return(data)
}

#' Additional testing
#'
#' @param hcws From hcws_weekly
#' @param screening_hcws From screening_hcws_weekly
#' @param test_strat From set_testing_strategy
#' @param tests_weekly From diagnostics_weekly
#'
#' @return Dataframe of additional testing
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{tests_hcw_weekly}{Tests for all HCWs, weekly}
#'   \item{tests_contacts_weekly}{Tests for all contacts, weekly, if testing
#'   contacts of positive cases or contact tracing. Can be specified in
#'   set_testing_strategy}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
additional_testing <- function(hcws, # from hcws_weekly
                               screening_hcws, # from screening_hcws_weekly
                               test_strat, # from set_testing_strategy
                               tests) { # from diagnostics_weekly
  data <- merge(hcws, screening_hcws)
  data <- merge(data, tests)
# this is temporary fix
  if(typeof(test_strat)!='data.frame'){
    test_strat <- as.data.frame(test_strat)
  }

  data <- data %>%
    dplyr::mutate(
      tests_hcws_weekly = test_strat$tests_per_hcw_per_week * (
        .data$hcws_inpatient_capped + .data$cleaners_inpatient_capped +
          .data$amb_personnel_inpatient_capped +
          .data$bio_eng_inpatient_capped + .data$screening_hcw_capped +
          .data$lab_staff_capped + .data$cleaners_lab),
      tests_contacts_weekly = ifelse(
        test_strat$testing_contacts == TRUE,
        test_strat$perc_contacts_tested * test_strat$avg_contacts_pos_case * (
          .data$tests_diagnosis_uncapped_sev_crit +
            .data$tests_release_uncapped_sev_crit + .data$tests_mild +
            .data$tests_mod), 0
      )
    ) %>%
    dplyr::select(c(
      week_begins, week_ends, tests_hcws_weekly,
      tests_contacts_weekly
    ))
  return(data)
}

#' Total tests
#'
#' @param tests_weekly From diagnostics_weekly
#' @param additional_tests From additional_testing
#' @param max_tests From max_tests_per_day, which depends on diagnostic capacity
#' and percent allocated to COVID-19 testing.
#'
#' @return Total tests
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{total_tests_capped}{Total tests per week, given a specific testing
#'   strategy, capped by max tests possible given equipment available and
#'   allocated to COVID-19 testing. Can modify this % by modifying dataframe
#'   produced by get_country_test_capacity and changing the values for
#'   covid_capacity before passing it into calc_diagnostic_capacity and then
#'   max_tests_per_day.}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
total_tests <- function(tests_weekly,
                        additional_tests,
                        max_tests) {
  data <- merge(tests_weekly, additional_tests)
  data <- data %>%
    dplyr::mutate(
      total_tests_uncapped = (.data$tests_diagnosis_capped_sev_crit +
        .data$tests_release_capped_sev_crit + .data$tests_mild +
        .data$tests_mod + .data$tests_suspected + .data$tests_hcws_weekly +
        .data$tests_contacts_weekly),
    )
  data <- data %>%
    dplyr::mutate(
      total_tests_capped = ifelse(.data$total_tests_uncapped > max_tests * 7,
                                  max_tests * 7,
                                  .data$total_tests_uncapped),
    ) %>%
    dplyr::select(c(
      week_begins, week_ends,
      total_tests_capped, total_tests_uncapped
    ))
  return(data)
}
