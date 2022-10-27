#' @title Get diagnostic parameters
#'
#' @description Sets the baseline diagnostic parameters.
#'
#' @param overrides Named list of values to use instead of defaults
#' The parameters are defined below, and are taken from the default settings in
#' the ESFT.
#'
#' total tests and percent antigen:
#'
#' * total_tests_mild_mod - total tests, used for diagnosis only; default = 1
#' * total_tests_sev_crit - total tests, used for diagnosis and release;
#' default = 2
#' * perc_antigen_tests - percent testing done in hospital via antigen testing,
#' max 80%; default = 20%
#'
#' @return List of diagnostic parameters.
#'
#' @export
get_diagnostic_parameters <- function(overrides = list()) {
  parameters <- list(
    total_tests_mild_mod = 1,
    total_tests_sev_crit = 2,
    perc_antigen_tests = 0.2
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    parameters[[name]] <- overrides[[name]]
  }

  if (parameters$perc_antigen_tests > 1 || parameters$perc_antigen_tests < 0) {
    stop("All percentage values must be less than or equal to 1 or greater than
         or equal to 0.")
  }

  parameters
}

#' @title Get diagnostic capacity
#'
#' @description Using country name or country code, return baseline estimates of
#' diagnostic testing capacity provided in the WHO ESFT.
#'
#' @param country Country name.
#' @param iso3c Country code, in iso3c format.
#' @param overrides a named list of parameter values to use instead of defaults.
#' Notably might include hologic_panther_fusion counts.
#' The values are described below and are taken from data provided in the ESFT.
#'
#' Counts of various machines in country:
#'
#' * roche_6800 - high throughput conventional platform
#' * roche_8800 - high throughput conventional platform
#' * abbott_m2000 - high throughput conventional platform
#' * hologic_panther - high throughput conventional platform
#' * hologic_panther_fusion - high throughput conventional platform
#' * genexpert - near-patient PCR machine modules (not platforms: here, one
#' machine contains several modules)
#' * manual - manual real-time PCR platform
#'
#' @importFrom tidyr pivot_longer
#' @importFrom base match
#' @importFrom magrittr %>%
#'
#' @return Number of diagnostic machines available within each country.
#' @source Estimates provided by the WHO Operations, Supply & Logistics Team and
#' reviewed by diagnostics tehcnical experts at the WHO.
#'
#' @export
get_country_test_capacity <- function(country = NULL,
                                            iso3c = NULL,
                                            overrides = list()) {
  if (!is.null(country) && !is.null(iso3c)) {
    # check they are the same one using the countrycodes
    iso3c_check <- countrycode::countrycode(country,
      origin = "country.name",
      destination = "iso3c"
    )
    country_check <- countrycode::countrycode(iso3c,
      origin = "iso3c",
      destination = "country.name"
    )
    if (iso3c_check != iso3c && country_check != country) {
      stop("Iso3c country code and country name do not match. Please check
           input/spelling and try again.")
    }
  }

  ## country route
  if (!is.null(country)) {
    country <- as.character(country)

    if (!country %in% unique(esft::diagnostics$country_name)) {
      stop("Country not found")
    }

    diagnostics <- subset(esft::diagnostics,
                          diagnostics$country_name == country)
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::diagnostics$country_code)) {
      stop("Iso3c not found")
    }
    diagnostics <- subset(esft::diagnostics, diagnostics$country_code == iso3c)
  }

  # not specified in spreadsheet
  diagnostics$hologic_panther_fusion <- 0

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(diagnostics))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    diagnostics[[name]] <- overrides[[name]]
  }

  diagnostics <- diagnostics %>%
    tidyr::pivot_longer(
      cols = c(
        "roche_6800", "roche_8800", "abbott_m2000", "hologic_panther",
        "hologic_panther_fusion", "genexpert", "manual"
      ),
      names_to = "platform_key",
      values_to = "modules_activated"
    )

  return(diagnostics)
}

#' @title Sets testing strategy and associated parameters
#'
#' @description
#' Default is testing strategy = all
#'
#' @param strategy testing strategy for mild/moderate presenting cases - either
#' "all" or "targeted"
#' @param perc_tested_mild_mod percent of mild or moderate tested, tied to
#' specific strategies, but can be manually changed
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below, and are taken from the default settings in
#' the ESFT.
#'
#' testing strategies:
#'
#' * all - all cases that present, regardless of severity, using standard number
#' of negatives per positive test
#' * targeted - restricted testing of mild/moderate presenting cases may be
#' employed if limited tests available. all required severe and critical cases
#' will still be tested. if selected, only the percent of
#' suspected/mild/moderate cases input here will be tested (typically high-risk
#' patients)
#'
#' further testing parameters:
#'
#' * perc_tested_sev_crit - percent of severe or critical cases tested, always
#' 100%; default = 1
#' * num_neg_per_pos_test - estimated average number of negative tests per
#' positive test; default = 10
#' * tests_per_hcw_per_week - tests per HCW or staff member per week. includes
#' tests for inpatient HCW, screening/triage HCW, ambulance personnel, cleaners,
#' lab techs, and biomedical engineers. input can also be 0 (for no tests) or a
#' decimal (e.g. 0.5, to represent tests every other week); default = 1
#' * testing_contacts - is testing done for contacts of positive cases;
#' default = TRUE
#' * avg_contacts_pos_case - average number of contacts per positive case.
#' suggested options are 5 (High/Strong, e.g. stay at home regulations),
#' 10 (Medium/Weak social distancing, e.g. travel restrictions),
#' 15 (Low/No social distancing, e.g. advice only); default = 10
#' * perc_contacts_tested - percent of contacts of a positive case who get
#' tested; default = 0.6
#'
#'
#' @return Testing strategy parameters.
#'
#' @export
set_testing_strategy <- function(strategy = "all",
                                 perc_tested_mild_mod = NULL,
                                 overrides = list()) {
  if (!is.null(strategy)) {
    strategy <- tolower(strategy)

    if (strategy == "all") {
      perc_tested_mild_mod <- 1
    } else if (strategy == "targeted") {
      if (is.null(perc_tested_mild_mod)) {
        perc_tested_mild_mod <- 0.1
      }
      # else take user specification
    }
  } else {
    strategy <- "all"
    perc_tested_mild_mod <- 1
  }

  parameters <- list(
    strategy = strategy,
    perc_tested_mild_mod = perc_tested_mild_mod,
    perc_tested_sev_crit = 1,
    num_neg_per_pos_test = 10,
    tests_per_hcw_per_week = 1,
    testing_contacts = TRUE,
    avg_contacts_pos_case = 10,
    perc_contacts_tested = 0.6
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    parameters[[name]] <- overrides[[name]]
  }

  if (parameters$perc_tested_mild_mod > 1 ||
    parameters$perc_contacts_tested > 1) {
    stop("All percentage values must be less than or equal to 1.")
    }

  if (parameters$perc_tested_mild_mod < 0 ||
    parameters$perc_contacts_tested < 0) {
    stop("All percentage values must be greater than or equal to 0.")
  }

  parameters
}

#' @title Get lab parameters
#'
#' @description Sets the baseline lab parameters.
#'
#' @param overrides Named list of parameter values to use instead of defaults
#' The parameters are defined below, and are taken from the default settings in
#' the ESFT.
#'
#' requirements per lab:
#'
#' * lab_staff_per_lab - multiplier that helps estimate lab staff equipment
#' requirements; default = 3
#' * hygienists_per_lab - multiplier that helps estimate lab hygienist/cleaner
#' equipment requirements; default = 3
#' * safety_boxes_per_unit_week - WHO recommendation for safe sharp disposal;
#' default = 8
#' * triple_packaging_per_unit - WHO recommendation for sample transport;
#' default = 4
#' * perc_wastage_manual_test_kits - percentage wastage, only of manual test
#' kits; default = 10 %
#'
#'
#' @export
get_lab_parameters <- function(overrides = list()) {
  parameters <- list(
    lab_staff_per_lab = 3,
    hygienists_per_lab = 1,
    safety_boxes_per_unit_week = 8,
    triple_packaging_per_unit = 4,
    perc_wastage_manual_test_kits = 0.1
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    parameters[[name]] <- overrides[[name]]
  }
  parameters
}
