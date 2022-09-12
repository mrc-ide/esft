rm(list=ls())
library(magrittr)
# let's test out functions
source("R/country_capacity.R")
source("R/parameters.R")
source("R/load_imperial_data.R")
source("R/cases_weekly.R")
source("R/user_input.R")
source("R/utils.R")
source("R/extra.R")
source("R/diagnostic_parameters.R")

load("data/who.rda")
load("data/population.rda")
load("data/hwfe.rda")

all <- readRDS("data-raw/all.Rds")


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
    perc_beds_sev_covid = perc_beds_sev_covid
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


afg_params <- get_country_capacity(iso3c="AFG")
afg_beds <- get_beds(afg_params)
afg_params <- merge(afg_params, afg_beds)
params <- get_parameters()

afg_data<-subset(all, all$iso3c == "AFG")

params <- merge(afg_params, params)

test_params <- set_testing_strategy()
# dealing with structuring commodities forecast
input <- user_input()

params <- append(params, input)
params <- append(params, test_params)

afg_summary <- cases_weekly(params=params,
                              data=afg_data)

# get rid of the tidyr and dplyr dependencies
# do data processing and parameter setting before weekly summary
# maybe add exists calls
# finish documentsation
sink("mylist.txt")
cat(paste0("#'   \\item{",names(afg_summary), "}{xyz}\n"))
sink()


# maybe have the actual sequence be that the user themselves subset amount by starttime, etc
# might also need different reusability multiplier params per category

source("r/patients_weekly.R")

afg_patients <- patients_weekly(params, data = afg_summary)
sink("mylist.txt")
cat(paste0("#'   \\item{",names(afg_patients), "}{xyz}\n"))
sink()
