# hcw capacity - double checked for the GF
# run on may 16 - for final rerun of data

rm(list=ls())
library(tidyverse)
library(readr)
# let's first pull params as they are written
source("R/parameters.R")
source("R/diagnostic_parameters.R")
source("R/scenario_parameters.R")
source("R/cases_weekly.R")
source("R/patients_weekly.R")
source("R/utils.R")
source("R/user_input.R")
source("drafts/hcw_caps.R")


load("data/who.rda")
load("data/population.rda")
load("data/noncovid.rda")
load("data/hwfe.rda")
load("data/diagnostics.rda")
load("data/throughput.rda")
load("data/hours_per_shift.rda")
load("data/pharmaceuticals.rda")
c19rm <- read_csv("~/Documents/GitHub/c19rm/data/c19rm_20_21.csv")
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

# get list of countries to iterate through
# this probably still includes those that were excluded
countries <- data.frame(iso3c = unique(c19rm$iso3c))
countries$country_name <- countrycode::countrycode(sourcevar = countries$iso3c,
                                                   origin = "iso3c",
                                                   destination = "country.name")
countries <- countries[complete.cases(countries),]
countries <- countries[!(countries$iso3c %in% c("CIV", "COD", "COG", "CPV", "LAO", "STP")),]

all <- subset(all, all$scenario == "Maintain Status Quo")

perc_treating_covid = list()
for (country in countries$iso3c) {
  capacity <- get_country_capacity(iso3c=country)
  data<-subset(all, all$iso3c == country)

  cases <- cases_weekly(params, capacity, test_strategy_params=test_strat,
                        data=data)

  # note - error occurred when subset by date
  patients <- patients_weekly(params, capacity, data = cases)
  hcw_cap_list <- hcw_caps(params,capacity,throughput,hwfe, patients)
  print(hcw_cap_list$perc_treating_covid)
}

# prints out 0.511 the whole time
# so i think i'll use 51% for now, especially because i'm not certain about the calculation,and still have the range
