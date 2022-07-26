# goal: turn afg_data -> test_data
# this will be what calculate cases will do
# to do - move this to a function file and get review
rm(list=ls())
library(tidyverse)
library(lubridate)
library(dplyr)

source("R/country_capacity.R")
source("R/parameters.R")

# load("~/esft/data/afg_data.rda") -> latest afg data from own newer models, not good for figuring out calc pathway
afg_tidy <- read.csv("data-raw/test_forecast_data.csv")
load("data/who.rda")
load("data/population.rda")
# Cumulative total cases column is fed by the selected case estimation method
# and specified transmission parameters (e.g. growth rate, infectious period,
# etc.). This is then broken down into case severity based on the inputs on
# severity breakdown

levels(as.factor(afg_tidy$compartment))
#  [1] "cumulative_deaths"     "cumulative_infections" "deaths"
# [4] "hospital_demand"       "hospital_incidence"    "ICU_demand"
# [7] "ICU_incidence"         "infections"            "prevalence"
# [10] "Reff"                  "Rt"

# Reff is probably the most relevant parameter, as is cumulative_infections
# week 0 is 02-Jan_2022 of imperial SEIR

afg_data<- readxl::read_excel("data-raw/esft.xlsx", # this is the unlocked esft that luke shared in jan
                              sheet = "Country Imperial Model Output")
# cut into weeks
# get cumulative incidence of infections
# get cumulative deaths
# get sum of everything else
# get avg R numbers
starting_date <- as.Date("2022-01-02")
afg_data$date <- as.Date(afg_data$date)

afg_data<-subset(afg_data, afg_data$scenario == "Maintain Status Quo")

afg_data <- afg_data %>%
  select(c(date, compartment, y_mean, scenario, iso3c, country))

afg_data <- afg_data %>%
  pivot_wider(names_from=compartment,
              values_from=y_mean)

afg_data<-afg_data %>%
  group_by(week = cut(date, breaks="week")) %>%
  summarise(date = last(date),
            hospital_demand = sum(hospital_demand, na.rm=TRUE),
            ICU_demand = sum(ICU_demand, na.rm=TRUE),
            hospital_incidence = sum(hospital_incidence, na.rm=TRUE),
            ICU_incidence = sum(ICU_incidence, na.rm=TRUE),
            prevalence = last(prevalence),
            Rt = last(Rt),
            Reff = last(Reff),
            infections = last(infections),
            deaths= sum(deaths, na.rm=TRUE),
            cumulative_infections = last(cumulative_infections),
            cumulative_deaths = last(cumulative_deaths)) # do i take

afg_data <- afg_data %>%
  mutate(cum_severe_cases = cumsum(hospital_incidence),
         new_severe_cases = hospital_incidence,
         cum_critical_cases = cumsum(ICU_incidence),
         new_critical_cases = ICU_incidence,
         adm_severe_cases_nocap = hospital_demand,
         adm_critical_cases_nocap = ICU_demand)

# to calculate cap:
# load country_capacity
iso3c="AFG"
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


perc_beds_crit_covid <- perc_beds_crit_covid/100
perc_beds_not_covid <- 0.4
perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid

afg_params <- list(
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

# use that and percent distribution to find number of beds allocated

beds_covid=round(n_hosp_beds*(1-perc_beds_not_covid))
severe_beds_covid=round(n_hosp_beds*perc_beds_sev_covid)
crit_beds_covid=round(n_hosp_beds*perc_beds_crit_covid)

afg_params <- list(
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

# written by giovanni, in utils
approx_sum <- function(X, n) abs(sum(X) - n) < sqrt(.Machine$double.eps)

afg_params_2 <- get_parameters()

afg_params<-merge(afg_params, afg_params_2)

afg<-merge(afg_data, afg_params)

library(tidyverse)
# capped - minimum of incidence or beds available
afg <- afg %>%
  mutate(adm_severe_cases_cap = min(hospital_incidence, severe_beds_covid),
         adm_critical_cases_cap = min(ICU_incidence, crit_beds_covid),

         # moderate and mild cases:
         new_mild_cases = sum(new_severe_cases,new_critical_cases)*
           mildI_proportion/sum(sevI_proportion,critI_proportion), # why only severe and critical here, and not moderate?
         new_mod_cases = sum(new_severe_cases,new_critical_cases)*
           modI_proportion/sum(sevI_proportion,critI_proportion),
         # what is difference between prevalence and infections here?
         # second possible method - needs review
         new_mild_cases_2 = infections*mildI_proportion,
         new_mod_cases_2 = infections*modI_proportion,
         new_severe_cases_2 = infections*sevI_proportion,
         new_critical_cases_2 = infections*critI_proportion)
# also look at removed numbers of cases
# includes dead and let out

# and quarantined cases
# quarantined is just cumulative new - cumulative removed
# at the top, we have:
# patient calcs
# mild cases = sum(new severe cases, new critical cases) * mildI_proportion/sum(sevI_proportion + critI_proportion)
# mod cases = sum(new severe cases, new critical cases) * modI_proportion/sum(sevI_proportion + critI_proportion)
# severe cases = new severe cases (as calc'd below), OR new cases * sev proportion
# critical cases = new critical cases (as calc'd below), OR new cases * crit proportion

# ok we have cumulative infections by week
# so then we get
# cumulative infections - cumulative infections
# infections - infections
# cumulative severe cases - hospital incidence cumulative
# severe cases - hospital incidence
# cumulative critical cases - ICU incidence
# admitted severe cases, no capping - hospital demand (just in past week as limit)
# admitted critical cases, no capping - ICU demand




afg_data<-subset(afg_data, afg_data$compartment %in% c("cumulative_infections",
                                                       "infections",
                                                       "Reff",
                                                       "hospital_incidence",
                                                       "ICU_incidence"))



# Calculate a rolling weekly sum
# using zoo - not sure i understand this
window <- 7
# afg_data_test <- afg_data %>%
#   group_by(compartment) %>%
#   mutate(sum_week = ~ zoo::rollapply(., window,
#                                  FUN=function(x) sum(x, na.rm=TRUE),
#                                  fill=c(first(.), NA, last(.)))) %>%
#   ungroup()


