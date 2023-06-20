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
source("R/noncovid_essentials.R")
source("R/pharma_forecast.R")
source("R/commodities_forecast.R")

load("data/who.rda")
load("data/population.rda")
load("data/noncovid.rda")
load("data/hwfe.rda")
load("data/diagnostics.rda")
load("data/throughput.rda")
load("data/hours_per_shift.rda")
load("data/pharmaceuticals.rda")
load("data/equipment.rda")

all <- readRDS("data-raw/all.Rds")
####### run first -------
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
# test_strat <- set_testing_strategy(strategy="targeted") - this works !!
test_strat <- set_testing_strategy()
params <- get_parameters()
test_params <- get_diagnostic_parameters()
capacity <- get_country_capacity(iso3c="AFG")

#hcw_caps_stat <- hcw_caps_static(params, capacity, throughput)
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
afg_data <- subset(afg_data, afg_data$scenario == "Maintain Status Quo")


# afg_data <- subset(afg_data, afg_data$date > as.Date("2022-01-01"))

cases <- cases_weekly(params, capacity, test_strategy_params=test_strat,
                      data=afg_data)

# note - error occurred when subset by date like this: afg_data$date > as.Date("2022-01-01"))
# have to subset before or you get carried over values from discharged and occupancy
cases <- subset(cases, cases$week_ends > as.Date("2022-01-01"))

# but patients still gives weird values for stay - it gives zeros, since those conditions havent been found and theyre super recursively difficlt to solve
patients <- patients_weekly(params, capacity, data = cases)
caps <- list(
  hcws_inpatients_cap = 5448,
  hcws_screening_cap = 919
)
patients <- patients[c(2:13),]
# subsetting gets the per bed stuff correct
# but the cleaner cap is sitll shit
hcw_caps <- hcw_caps(params,capacity,throughput,hwfe, patients, overrides=caps)
# also did weird stuff when subset by date - but tend only to be for diagnosis
# patients <- subset(patients, patients$week_ends > as.Date("2022-01-01"))

tests <- diagnostics_weekly(params = params, patients, cases,
                            diagnostic_parameters = test_params,
                            testing_scenario = test_strat)
# NOTE: THERE IS AN ERROR IN THE WAY HCW CAPS ARE CALCULATED IN THE ESFT SHEET

hcws <- hcws_weekly(params, # from get_parameters
                    capacity, # from get_country_capacity
                    lab_params, # get_lab_parameters
                    tests, # from diagnostics_weekly
                    patients, # patients_weekly
                    t_labs, # total_labs
                    hcw_caps)
screening_hcws <- screening_hcws_weekly(tests, hcw_caps,
                                        capacity)
added_tests <- additional_testing(hcws, # from hcws_weekly
                                  screening_hcws, # from screening_hcws_weekly
                                  test_strat, # from set_testing_strategy
                                  tests)
n_tests <- total_tests(tests, added_tests, max_tests)
test_ratios <- test_ratio(diagnostic_capacity, test_params)

####### forecast -------

# need to rewrite because the esft proj not loaded
# ref_hcws <- reference_hcw(iso3c = "AFG", params, who, throughput,
#                           overrides = list(
#                             n_docs = 8000,
#                             n_nurses = 5000,
#                             n_labs = 300,
#                             n_midwives = 500,
#                             n_dentists = 10,
#                             n_physiotherapists = 50,
#                             n_trad_comp_med = 4000,
#                             n_chws = 245,
#                             n_pharmacists = 818
#                           ))
# noncovid_ess <-noncovid_essentials(noncovid, ref_hcws,
#                                 forecast_length = 12,
#                                 days_week = 5)

cases <- cases[c(2:13),]
pharma <- pharma_forecast(pharmaceuticals, cases)
pharma <- pharma[,c(2,25:28)]
# hygiene is all good
hygiene <- hygiene_forecast(
  equipment, hcws, patients, cases, tests,
  screening_hcws, params
)

case_management <- case_management_forecast(equipment, patients)
case_management <- subset(case_management, case_management$week_begins < "2022-01-14")
case_management <- case_management[,c(1,3:4,8)]

ppe <- ppe_forecast(
  equipment, hcws, patients, cases, tests,
  screening_hcws, params
)
ppe <- subset(ppe, ppe$week_begins < "2022-01-14")
ppe <- ppe[,c(1:3,9)]

diagnostic_supplies <- diagnostics_forecast(
  lab_params, equipment, test_ratios,
  n_tests, patients
)
diagnostic_supplies <- subset(diagnostic_supplies, diagnostic_supplies$week_begins < "2022-01-14")
diagnostic_supplies <- diagnostic_supplies[,c(-5)]
