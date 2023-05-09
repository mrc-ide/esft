#' @title Get diagnostic parameters
#'
#' @description Sets the baseline diagnostic parameters.
#'
#' @param overrides Named list of values to use instead of defaults
#' The parameters are defined below, and are taken from the default settings in
#' the ESFT.
#'
#' Total tests and percent antigen:
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
    perc_antigen_tests = 0.2,
    # option to instead code like testing scenario
    tests_diagnosis_mild_mod = 1,
    tests_diagnosis_sev_crit = 1,
    tests_release_mild_mod = 0,
    tests_release_sev_crit = 1
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
#' @importFrom magrittr %>%
#'
#' @return Number of diagnostic machines available within each country.
#' @source Estimates provided by the WHO Operations, Supply & Logistics Team and
#' reviewed by diagnostics tehcnical experts at the WHO.
#'
#' @export
get_country_test_capacity <- function(iso3c = NULL,
                                      overrides = list()) {
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
#' * num_tests_manual_test_kits - number of tests in an RT-PCR manual test kit;
#' default = 100
#'
#'
#' @export
get_lab_parameters <- function(overrides = list()) {
  parameters <- list(
    lab_staff_per_lab = 3,
    hygienists_per_lab = 1,
    safety_boxes_per_unit_week = 8,
    triple_packaging_per_unit = 4,
    perc_wastage_manual_test_kits = 0.1,
    num_tests_manual_test_kits = 100
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

#' @title Given specified parameters, calculates diagnostic country capacity.
#'
#' @details
#' As of right now, need to increase hours per shift in throughput data in order
#' to up the capacity.
#'
#' @param country_diagnostic_capacity Capacity called from the
#' get_country_test_capacity function
#' @param throughput Throughput data, loaded in package, from ESFT
#' @param shifts_per_day Either single integer (1,2, or 3) or named vector of
#' shifts per day ("shifts_day") for the specific machines ("platform_key").
#' Important to get names right if go vector rought (easiest to copy paste from
#' throughput), and shifts can only be 1, 2, or 3 (8, 12, 24 hrs).
#' Default = NULL.
#' @param hours_per_shift Hours per shift, default = 8
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
calc_diagnostic_capacity <- function(country_diagnostic_capacity,
                                     throughput,
                                     shifts_per_day = NULL, # vector
                                     hours_per_shift) {
  if (!(is.null(shifts_per_day))) {
    if (length(shifts_per_day) == 1) {
      shifts_per_day <- data.frame(shifts_day = rep(
        shifts_per_day,
        length(throughput$platform_key)
      ))
      shifts_per_day$platform_key <- throughput$platform_key
    }
    throughput <- merge(throughput, shifts_per_day, by = c(
      "platform_key",
      "shifts_day"
    ))
  }
  capacity <- merge(throughput, country_diagnostic_capacity)
  capacity <- merge(capacity, hours_per_shift,
    by.x = "shifts_day",
    by.y = "shifts"
  )

  capacity <- capacity %>%
    dplyr::mutate(throughput_per_day = case_when(
      .data$hours == 8 ~ throughput_8hrs,
      .data$hours == 16 ~ throughput_16hrs,
      .data$hours == 24 ~ throughput_24hrs,
      TRUE ~ NA_real_
    )) %>%
    dplyr::select(-c(
      throughput_8hrs, throughput_16hrs,
      throughput_24hrs
    ))

  # calculate the testing capacity available - max, and covid
  capacity$total_test_capacity <- capacity$modules_activated *
    capacity$days_week * capacity$throughput_per_day
  capacity$covid_test_capacity <- capacity$total_test_capacity *
    capacity$covid_capacity

  return(capacity)
}

#' @title Get diagnostic ratios
#'
#' @param capacity From calculate_diagnostic_capacity.
#' @param diagnostic_params From get_diagnostic_parameters
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
test_ratio <- function(capacity, diagnostic_params) {
  capacity <- capacity %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(covid_test_capacity = sum(covid_test_capacity)) %>%
    dplyr::select(c(type, covid_test_capacity))

  capacity <- capacity[!duplicated(capacity), ]

  capacity$ratio <- (1 - diagnostic_params$perc_antigen_tests) * (
    capacity$covid_test_capacity) / sum(capacity$covid_test_capacity)

  # calculate num antigen tests
  num_antigen <- sum(capacity$covid_test_capacity) / sum(capacity$ratio) -
    sum(capacity$covid_test_capacity)
  capacity[nrow(capacity) + 1, ] <- list(
    "antigen", num_antigen,
    diagnostic_params$perc_antigen_tests
  )

  return(capacity)
}

#' @title Calculates max total labs that could be available for COVID
#'
#' @param capacity From get_country_test_capacity or
#' calculate_diagnostic_capacity. Only thing is, we need the country capacity.
#'
#' @export
total_labs <- function(capacity) {
  labs <- sum(
    capacity$modules_activated[capacity$platform_key == "roche_6800"],
    capacity$modules_activated[capacity$platform_key == "roche_8800"],
    capacity$modules_activated[capacity$platform_key == "abbott_m2000"],
    capacity$modules_activated[capacity$platform_key == "hologic_panther"],
    capacity$modules_activated[capacity$platform_key ==
      "hologic_panther_fusion"],
    capacity$modules_activated[capacity$platform_key == "manual"]
  ) / 3 +
    capacity$modules_activated[capacity$platform_key == "genexpert"] / 4

  labs <- round(labs)

  return(labs)
}
