#' @title Get diagnostic parameters
#'
#' @description Sets the baseline diagnostic parameters.
#'
#' @param overrides Named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' @export
get_diagnostic_parameters <- function(overrides = list()) {

  parameters <- list(
    total_tests_mild_mod = 1,
    total_tests_sev_crit = 2,
    perc_antigen_tests = 0.2,
    shifts_day_high_throughput = 1,
    days_week_high_throughput = 5,
    covid_capacity_high_throughput = 0.5, # most of the testing machines
    shifts_day_near_patient = 1,
    days_week_near_patient = 5,
    covid_capacity_near_patient = 0.5, # genexpert
    shifts_day_manual = 1,
    days_week_manual = 5,
    covid_capacity_manual = 1)

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  if (parameters$perc_antigen_tests > 1 |
      parameters$covid_capacity_high_throughput > 1 |
      parameters$covid_capacity_near_patient > 1 |
      parameters$covid_capacity_manual > 1) {
    stop("All percentage values must be less than or equal to 1.")
  }

  if (parameters$perc_antigen_tests < 0 |
      parameters$covid_capacity_high_throughput < 0 |
      parameters$covid_capacity_near_patient < 0 |
      parameters$covid_capacity_manual < 0) {
    stop("All percentage values must be greater than or equal to 0.")
  }

  parameters
}

#' @title Get diagnostic capacity
#'
#' @description Using country name or country code, return baseline estimates of diagnostic
#' testing capacity provided in the WHO ESFT.
#'
#' @param country Country name.
#' @param iso3c Country code, in iso3c format.
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#'
#' @export
get_diagnostic_capacity <- function(country = NULL,
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
    if (iso3c_check != iso3c & country_check != country) {
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

    diagnostics <- subset(esft::diagnostics, diagnostics$country_name == country)
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::diagnostics$country_code)) {
      stop("Iso3c not found")
    }
    diagnostics <- subset(esft::diagnostics, diagnostics$country_code == iso3c)
  }


  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(diagnostics))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    diagnostics[[name]] <- overrides[[name]]
  }

  return(diagnostics)
}

#' @title Sets testing strategy and associated parameters
#'
#' @description
#' Default is testing strategy = all
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' @export
set_testing_strategy <- function(strategy = "all",
                                 perc_tested_mild_mod = NULL,
                                 overrides = list()) {

  if(!is.null(strategy)) {
    strategy <- tolower(strategy)

    if(strategy == "all"){
      perc_tested_mild_mod <- 1
    } else if (strategy == "targeted") {
      if(is.null(perc_tested_mild_mod)) {
        perc_tested_mild_mod <- 0.1
      }
      # else take user specification
    }

  } else {
    strategy = "all"
    perc_tested_mild_mod = 1
  }

  parameters <- list(
    strategy = strategy,
    perc_tested_mild_mod = perc_tested_mild_mod,
    perc_tested_sev_crit = 2,
    num_neg_per_pos_test = 10,
    tests_per_hcw_per_week = 1,
    testing_contacts = TRUE,
    avg_contacts_pos_case = 10,
    perc_contacts_tested = 0.6)

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  if (parameters$perc_tested_mild_mod > 1 |
      parameters$perc_contacts_tested > 1 ) {
    stop("All percentage values must be less than or equal to 1.")
  }

  if (parameters$perc_tested_mild_mod < 0 |
      parameters$perc_contacts_tested < 0 ) {
    stop("All percentage values must be greater than or equal to 0.")
  }

  parameters
}

#' @title Get lab parameters
#'
#' @description Sets the baseline lab parameters.
#'
#' @param overrides Named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' @export
get_lab_parmeters <- function(overrides = list()) {

  parameters <- list(
    lab_staff_per_lab = 3,
    cleaners_per_lab = 1,
    safety_boxes_per_unit_week = 8,
    triple_packaging_per_unit = 4,
    perc_wastage_manual_test_kits = 0.1)

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }
  parameters
}
