#' @title Pharmaceuticals Forecast
#'
#' @description Takes total amounts of drugs per course, percent treated
#' and new cases by severity, and yields estimates of the total drugs required
#' for the forecast period. Note: due to differences in the norms around
#' rounding (R rounds from 5 to the nearest even number, Excel rounds up from 5)
#' - these numbers are slightly different from the numbers in the forecast file.
#' If desired, the code can be modified to round up (replace round with
#' ceiling in the body of the function).
#'
#'
#' @return Dataframe of Pharmaceutical Forecast
#' \describe{
#'   \item{drug}{Drug name}
#'   \item{total_drug_form_all_mild_treated}{Total drug form needed for all
#'   mild patients}
#'   \item{total_drug_form_all_mod_treated}{Total drug form needed for all
#'   moderate patients}
#'   \item{total_drug_form_all_severe_treated}{Total drug form needed for all
#'   severe patients}
#'   \item{total_drug_form_all_crit_treated}{Total drug form needed for all
#'   critical patients}
#'}
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
pharma_forecast <- function(pharmaceuticals, cases) {
  sums <- cases %>%
    dplyr::select(-c(week_ends, week_begins)) %>%
    dplyr::summarise(dplyr::across(
      tidyselect::everything(),
      ~ sum(., is.na(.), 0)
    )) %>%
    dplyr::select(c(
      new_critical_cases, new_severe_cases, new_mild_cases,
      new_mod_cases
    ))
  sums <- sums %>% dplyr::mutate_if(is.numeric, ~ round(.))

  pharmaceuticals[is.na(pharmaceuticals)] <- 0

  pharmaceuticals$total_drug_form_all_mild_treated <-
    sums$new_mild_cases * pharmaceuticals$form_per_mild_course *
      pharmaceuticals$perc_mild_treated
  pharmaceuticals$total_drug_form_all_mod_treated <-
    sums$new_mod_cases * pharmaceuticals$form_per_mod_course *
      pharmaceuticals$perc_mod_treated
  pharmaceuticals$total_drug_form_all_severe_treated <-
    sums$new_severe_cases * pharmaceuticals$form_per_sev_course *
      pharmaceuticals$perc_sev_treated
  pharmaceuticals$total_drug_form_all_crit_treated <-
    sums$new_critical_cases * pharmaceuticals$form_per_crit_course *
      pharmaceuticals$perc_crit_treated

  pharmaceuticals <- pharmaceuticals[, c(2, 25:28)]

  return(pharmaceuticals)
}
