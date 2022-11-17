rm(list=ls())
library(tidyverse)

# let's first pull params as they are written
source("R/parameters.R")
source("R/diagnostic_parameters.R")
source("R/country_capacity.R")
source("R/cases_weekly.R")
source("R/patients_weekly.R")
source("R/utils.R")
source("R/user_input.R")
params <- get_parameters()
# goal is to figure out hcw caps and cases:
source("R/hcw_caps.R")

load("data/who.rda")
load("data/population.rda")
load("data/hwfe.rda")

all <- readRDS("data-raw/all.Rds")


afg_params <- get_country_capacity(iso3c="AFG")
params <- get_parameters()
afg_data<-subset(all, all$iso3c == "AFG")

params <- merge(afg_params, params)

# no real dependencies
input <- user_input()
test_params <- set_testing_strategy()
# dealing with structuring commodities forecast

# cases weekly needs this to becalled - wrapper function potential?
params <- append(params, input)
params <- append(params, test_params)



##### COUNTRY CAPACITY -----------------
get_country_capacity <- function(country = NULL,
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
    if (!country %in% unique(who$country_name)) {
      stop("Country not found")
    }

    pop <- who$population[who$country_name == country]
    yoy_growth <- population$yoy[population$country_wb == country]
    income_group <- who$income_group[who$country_name == country]

    n_hcws <- who$doctors[who$country_name == country] +
      who$nurses[who$country_name == country]
    n_labs <- who$labs[who$country_name == iso3c]

    n_hosp_beds <- who$beds_total[who$country_name == country]
    perc_beds_crit_covid <- who$perc_icu_beds[who$country_name == country]

    iso3c <- countrycode::countrycode(country,
                                      origin = "country.name",
                                      destination = "iso3c"
    )
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(who$country_code)) {
      stop("Iso3c not found")
    }
    pop <- who$population[who$country_code == iso3c]
    yoy_growth <- population$yoy[population$country_code == iso3c]
    income_group <- who$income_group[who$country_code == iso3c]

    n_hcws <- who$doctors[who$country_code == iso3c] +
      who$nurses[who$country_code == iso3c]
    n_labs <- who$labs[who$country_code == iso3c]

    n_hosp_beds <- who$beds_total[who$country_code == iso3c]
    perc_beds_crit_covid <- who$perc_icu_beds[who$country_code == iso3c]

    country <- countrycode::countrycode(iso3c,
                                        origin = "iso3c",
                                        destination = "country.name"
    )
  }

  perc_beds_crit_covid <- perc_beds_crit_covid/100
  perc_beds_not_covid <- 0.4
  perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid
  beds_covid = round(n_hosp_beds*(1-perc_beds_not_covid))
  severe_beds_covid = round(n_hosp_beds*perc_beds_sev_covid)
  crit_beds_covid = round(n_hosp_beds*perc_beds_crit_covid)

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
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(country_capacity))) {
      stop(paste('unknown parameter', name, sep=' '))
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




##### WEEKLY CASES ---------------------------
afg_summary <- cases_weekly(params=params,
                              data=afg_data)

# gives me beds in use
afg_summary <- patients_weekly(params,
                               afg_summary)
# get rid of the tidyr and dplyr dependencies
# do data processing and parameter setting before weekly summary
# maybe add exists calls
# finish documentsation
# sink("names.txt")
# cat(paste0("#'   \\item{",names(afg_params), "}{xyz}\n"))
# sink()

#param definitions
# sink("names.txt")
# cat(paste0("#' * ",names(params), "- INSERT; default = \n"))
# sink()

# maybe have the actual sequence be that the user themselves subset amount by starttime, etc
# might also need different reusability multiplier params per category

# source("r/patients_weekly.R")

afg_patients <- patients_weekly(params, data = afg_summary)
# sink("mylist.txt")
# cat(paste0("#'   \\item{",names(afg_patients), "}{xyz}\n"))
# sink()

load("data/diagnostics.rda")
load("data/throughput.rda")
load("data/hours_per_shift.rda")
source("r/diagnostic_parameters.R")

######
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

    if (!country %in% unique(diagnostics$country_name)) {
      stop("Country not found")
    }

    diagnostics <- subset(diagnostics,
                          diagnostics$country_name == country)
  }

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
#####
afg_tests <- get_country_test_capacity(iso3c="AFG")
afg_capacity <- calc_diagnostic_capacity(country_diagnostic_capacity=afg_tests,
                                         throughput=throughput,
                                         hours_per_shift=hours_per_shift)
##### TRY TO CALCULATE diagnostic capacity -----------------
library(tidyverse)
# afg_tests <- afg_tests %>%
#   pivot_longer(cols = c(roche_6800, roche_8800, abbott_m2000, hologic_panther,
#                         hologic_panther_fusion, genexpert, manual),
#                names_to = "platform_key",
#                values_to = "modules_activated")
#
#
# # colnames(throughput)[3:5]<-str_extract_all(names(throughput[,c(3:5)]), '[0-9]+')
afg_test_params <- get_diagnostic_parameters()
tests_weekly <- diagnostics_weekly(params=params, hwfe=hwfe, data=afg_patients,
                                   diagnostic_capacity = afg_capacity,
                                   diagnostic_parameters = afg_test_params)

hcw_static<- hcw_caps_static(params = params, throughput = throughput)
hcw_dyn <- hcw_caps_dynamic(params, hwfe, data=afg_patients)

hcw_caps <- merge(hcw_static, hcw_dyn)


afg_hcws <- hcws_weekly(params, data=afg_patients,
                        diagnostics_weekly = tests_weekly, hcw_caps = hcw_caps)

###########################
capacity <- merge(throughput, afg_tests)
capacity <- merge(capacity, hours_per_shift, by.x = "shifts_day", by.y = "shifts")

capacity <- capacity %>%
  mutate(throughput_per_day = case_when(hours == 8 ~ throughput_8hrs,
                                        hours == 16 ~ throughput_16hrs,
                                        hours == 24 ~ throughput_24hrs,
                                        TRUE ~ NA_real_)) %>%
  select(-c(throughput_8hrs, throughput_16hrs, throughput_24hrs))


# either rename platform in afg_tests to match capacity or vice versa
capacity$total_test_capacity <- capacity$modules_activated*capacity$days_week*capacity$throughput_per_day
capacity$covid_test_capacify <- capacity$total_test_capacity*capacity$covid_capacity


##### HCW CAPS + CASES --------------
# there tends to be a lot of mixing within the two
# i want to see what is going on - what is dependent on the model outputs & what is not
source("drafts/hcws_weekly.R")
source("R/patients_weekly.R")
lab_params <- get_lab_parameters()
hcws <- hcw_caps(params = params, # maybe this should already by a subsetted country vector of params?
                     hwfe = hwfe,
                     data = afg_patients,
                     country_capacity,
                     diagnostic_parameters,
                     lab_params)
# ok so here it shows the last 4 are data dependent - how do we reduce this? are flctuating caps
