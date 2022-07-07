# goal: turn afg_data -> test_data
# this will be what calculate cases will do
library(tidyverse)
library(lubridate)
library(dplyr)

# load("~/esft/data/afg_data.rda") -> latest afg data from own newer models, not good for figuring out calc pathway
afg_tidy <- read.csv("data-raw/test_forecast_data.csv")
who <- load("data/who.rda")
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

afg_data_test<-afg_data %>%
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

afg_data_test <- afg_data_test %>%
  mutate(cum_severe_cases = cumsum(hospital_incidence, na.rm=TRUE),
         new_severe_cases = hospital_incidence,
         cum_critical_cases = cumsum(ICU_incidence, na.rm=TRUE),
         new_critical_cases = ICU_incidence,
         adm_severe_cases_nocap = hospital_demand,
         adm_critical_cases_nocap = ICU_demand)

# to calculate cap:
# load country_capacity
# use that and percent distribution to find number of beds allocated


# also look at removed numbers of cases
# and quarantined cases

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



# capped - minimum of incidence or beds available
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


