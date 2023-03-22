# i just want something to run to get the weekly summary
# can change it later to a wrapper if need be

rm(list=ls())
library(tidyverse)

# let's first pull params as they are written
source("R/parameters.R")
source("R/diagnostic_parameters.R")
source("R/scenario_parameters.R")
source("R/cases_weekly.R")
source("R/patients_weekly.R")
source("R/utils.R")
source("R/user_input.R")
source("R/hcw_caps.R")
source("R/diagnostics_weekly.R")
source("R/hcw_tests.R")
source("R/hcws_weekly.R")
# goal is to figure out hcw caps and cases:

load("data/who.rda")
load("data/population.rda")
load("data/noncovid.rda")
load("data/hwfe.rda")
load("data/diagnostics.rda")
load("data/throughput.rda")
load("data/hours_per_shift.rda")
load("data/pharmaceuticals.rda")

all <- readRDS("data-raw/all.Rds")

get_country_capacity <- function(iso3c = NULL,
                                 overrides = list()) {

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(who$country_code)) {
      stop("Iso3c not found")
    }
    country <- countrycode::countrycode(iso3c,
                                        origin = "iso3c",
                                        destination = "country.name"
    )
  }

  pop <- who$population[who$country_code == iso3c]
  yoy_growth <- population$yoy[population$country_code == iso3c]
  income_group <- who$income_group[who$country_code == iso3c]

  n_hcws <- who$doctors[who$country_code == iso3c] +
    who$nurses[who$country_code == iso3c]
  n_labs <- who$labs[who$country_code == iso3c]

  n_hosp_beds <- who$beds_total[who$country_code == iso3c]
  perc_beds_crit_covid <- who$perc_icu_beds[
    who$country_code == iso3c
  ]

  perc_beds_crit_covid <- perc_beds_crit_covid / 100
  perc_beds_not_covid <- 0.4
  perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid

  beds_covid <- round(n_hosp_beds * (1 - perc_beds_not_covid))
  severe_beds_covid <- round(n_hosp_beds * perc_beds_sev_covid)
  crit_beds_covid <- round(n_hosp_beds * perc_beds_crit_covid)

  country_capacity <- list(
    country = country,
    iso3c = iso3c,
    population = pop,
    yoy_growth = yoy_growth,
    income_group = income_group,
    n_hcws = n_hcws,
    n_labs = n_labs,
    n_hosp_beds = n_hosp_beds,
    perc_beds_crit_covid = perc_beds_crit_covid,
    perc_beds_not_covid = perc_beds_not_covid,
    perc_beds_sev_covid = perc_beds_sev_covid,
    beds_covid = beds_covid,
    severe_beds_covid = severe_beds_covid,
    crit_beds_covid = crit_beds_covid
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(country_capacity))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    country_capacity[[name]] <- overrides[[name]]
  }

  perc_beds <- c(
    country_capacity$perc_beds_crit_covid,
    country_capacity$perc_beds_not_covid,
    country_capacity$perc_beds_sev_covid
  )

  if (!approx_sum(perc_beds, 1)) {
    stop("Bed allocation percentages do not sum to 1")
  }

  return(country_capacity)
}
get_country_test_capacity <- function(iso3c = NULL,
                                      diagnostics,
                                      overrides = list()) {


  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(diagnostics$country_code)) {
      stop("Iso3c not found")
    }
    diagnostics <- subset(diagnostics, diagnostics$country_code == iso3c)
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
# getting parameters, except for dynamic hcw caps, which depend on patents_weekly
test_strat <- set_testing_strategy()
params <- get_parameters()
test_params <- get_diagnostic_parameters()
capacity <- get_country_capacity(iso3c="AFG")

hcw_caps_stat <- hcw_caps_static(params, capacity, throughput)
lab_params <- get_lab_parameters()

# capacity functions
country_test_capacity <- get_country_test_capacity(iso3c="AFG", diagnostics)
diagnostic_capacity <- calc_diagnostic_capacity(country_diagnostic_capacity =
                                                  country_test_capacity,
                                                throughput, hours_per_shift =
                                                  hours_per_shift,
                                                shifts_per_day = 1)
t_labs <- total_labs(diagnostic_capacity)
max_tests <- max_tests_per_day(diagnostic_capacity)

afg_data<-subset(all, all$iso3c == "AFG")
cases <- cases_weekly(params, capacity, test_strategy_params=test_strat,
                      data=afg_data)
patients <- patients_weekly(params, capacity, data = cases)
hcw_caps_dyn <- hcw_caps_dynamic(params, hwfe, patients)
tests <- diagnostics_weekly(params, patients, cases,
                            diagnostic_parameters = test_params)
hcws <- hcws_weekly(params, capacity = capacity, lab_params, tests, patients,
                    t_labs, hcw_dyn_caps = hcw_caps_dyn,
                    hcw_stat_caps = hcw_caps_stat)
screening_hcws <- screening_hcws_weekly(tests, params)
added_tests <- additional_testing(hcws, screening_hcws, params, test_strat,
                                  tests)
n_tests <- total_tests(tests, added_tests, max_tests)
test_ratios <- test_ratio(diagnostic_capacity, test_params)
load("data/equipment.rda")


list=ls()
list <- list[4,6,13,19,]
