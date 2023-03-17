#' @title Pharmaceuticals Forecast
#'
#' @description
#' Takes the pharma inputs and times them by total new cases.
#' Unclear how to work out the price setting aspect.
#'
#'
#' @import dplyr
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#'
#' @export
pharma_forecast <- function(pharmaceuticals, cases) {

  sums <- cases %>%
      dplyr::select(-c(week_ends, week_begins)) %>%
      dplyr::summarise(dplyr::across(tidyselect::everything(),
                                     ~ sum(., is.na(.), 0))) %>%
    dplyr::select(c(new_critical_cases, new_severe_cases, new_mild_cases,
                    new_mod_cases))

  pharmaceuticals[is.na(pharmaceuticals)] <- 0

  pharmaceuticals <- pharmaceuticals %>%
    dplyr::mutate(
      total_drug_form_all_mild_treated =
        sums$new_mild_cases*form_per_mild_course*perc_mild_treated,
      total_drug_form_all_mod_treated =
        sums$new_mod_cases*form_per_mod_course*perc_mod_treated,
      total_drug_form_all_severe_treated =
        sums$new_severe_cases*form_per_sev_course*perc_sev_treated,
      total_drug_form_all_crit_treated =
        sums$new_critical_cases*form_per_crit_course*perc_crit_treated
    )
}
