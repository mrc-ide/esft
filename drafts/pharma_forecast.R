#' @title Pharmaceuticals Forecast
#'
#' Could take the pharma inputs and times them by weekly forecast ratios
#' Unclear if want to move forward with costing tool though
#' Maybe I use a hashing tool or a code to deal with pharma names
#'
#' @description
#'
#' @import dplyr
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#'
#' @export
pharma_weekly <- function(pharmaceuticals, cases) {

  sums <- cases %>%
      dplyr::select(-c(week_ends, week_begins)) %>%
      dplyr::summarise(dplyr::across(tidyselect::everything(),
                                     ~ sum(., is.na(.), 0))) %>%
    dplyr::select(c(new_critical_cases, new_severe_cases, new_mild_cases,
                    new_mod_cases))

  pharmaceuticals <- pharmaceuticals %>%
    dplyr::mutate(
      total_drug_form_all_mild_treated = sums$new_mild_cases*,
      total_drug_form_all_mod_treated = ,
      total_drug_form_all_severe_treated = ,
      total_drug_form_all_crit_treated =
    )
}
