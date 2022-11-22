#' Screening HCWs per week, capped and uncapped
#'
#' @param diagnostics_weekly
#' @param params
#'
#' @return Dataframe of summary
#' \describe{
#'   \item{week_ends}{xyz}
#'   \item{week_begins}{xyz}
#'   \item{screening_hcw_uncapped}{xyz}
#'   \item{screening_hcw_capped}{xyz}
#' }
#'
#' @export
screening_hcw_weekly <- function(diagnostics_weekly,
                                 params) {
  df <- diagnostics_weekly
  df <- df %>%
    mutate(screening_hcw_uncapped = (tests_suspected + tests_mild +
      tests_mod) / (7 * params$cases_screened_per_hcw_per_day))

  df <- df %>%
    mutate(
      screening_hcw_capped = min(
        screening_hcw_uncapped, params$n_hcws * params$perc_hcws_screen_covid
      )
    ) %>%
    select(c(
      week_begins, week_ends, screening_hcw_uncapped,
      screening_hcw_capped
    ))

  return(df)
}


#' Additional testing
#'
#' @param hcws
#' @param screening_hcws
#' @param params
#' @param tests_weekly
#'
#' @return Dataframe of additional testing
#' \describe{
#'   \item{week_ends}{xyz}
#'   \item{week_begins}{xyz}
#'   \item{tests_hcw_weekly}{xyz}
#'   \item{tests_contacts_weekly}{xyz}
#' }
#'
#' @export
additional_testing <- function(hcws, # from hcws_weekly
                               screening_hcws, # from screening_hcw_weekly
                               params,
                               tests_weekly) {
  df <- merge(hcws, screening_hcws)
  df <- merge(df, tests_weekly)
  df <- df %>%
    mutate(
      tests_hcws_weekly = (hcws_inpatient_capped + cleaners_inpatient_capped +
        amb_personnel_inpatient_capped +
        bio_eng_inpatient_capped + screening_hcw_capped +
        lab_staff_capped + cleaners_lab),
      tests_contacts_weekly = ifelse(
        params$testing_contacts == TRUE,
        params$perc_contacts_tested * params$avg_contacts_pos_case * (
          tests_diagnosis_uncapped_sev_crit + tests_release_uncapped_sev_crit +
            tests_mild + tests_mod), 0
      )
    ) %>%
    select(c(week_begins, week_ends, tests_hcws_weekly, tests_contacts_weekly))
  return(df)
}

#' Total tests
#'
#' @param tests_weekly
#' @param additional_tests
#' @param max_tests
#'
#' @return Total tests
#' \describe{
#'   \item{week_ends}{xyz}
#'   \item{week_begins}{xyz}
#'   \item{tests_tests_capped}{xyz}
#' }
#'
#' @export
total_tests <- function(tests_weekly,
                        additional_tests,
                        max_tests) { # from max_tests_per_day, which relies on diagnostic capacity
  df <- merge(tests_weekly, additional_tests)
  df <- df %>%
    mutate(
      total_tests_uncapped = tests_diagnosis_capped_sev_crit +
        tests_release_capped_sev_crit + tests_mild + tests_mod +
        tests_suspected + tests_hcws_weekly + tests_contacts_weekly,
    )
  df <- df %>%
    mutate(
      total_tests_capped = min(total_tests_uncapped, max_tests * 7),
    ) %>%
    select(C(week_begins, week_ends, total_tests_capped))
  return(df)
}
