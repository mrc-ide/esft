# goal: turn afg_data -> test_data
# this will be what calculate cases will do
library(tidyverse)
library(lubridate)

# load("~/esft/data/afg_data.rda") -> latest afg data from own newer models, not good for figuring out calc pathway
afg_tidy <- read.csv("~/esft/data-raw/test_forecast_data.csv")

# Cumulative total cases column is fed by the selected case estimation method
# and specified transmission parameters (e.g. growth rate, infectious period,
# etc.). This is then broken down into case severity based on the inputs on
# severity breakdown

levels(as.factor(afg_data$compartment))
#  [1] "cumulative_deaths"     "cumulative_infections" "deaths"
# [4] "hospital_demand"       "hospital_incidence"    "ICU_demand"
# [7] "ICU_incidence"         "infections"            "prevalence"
# [10] "Reff"                  "Rt"

# Reff is probably the most relevant parameter, as is cumulative_infections
# week 0 is 02-Jan_2022 of imperial SEIR

afg_data<- readxl::read_excel("data-raw/esft.xlsx", # this is the unlocked esft that luke shared in jan
                              sheet = "Country Imperial Model Output")

starting_date <- as.Date("2022-01-02")
afg_data$date <- as.Date(afg_data$date)
afg_data<-subset(afg_data, afg_data$date >= starting_date)

afg_data<-subset(afg_data, afg_data$compartment %in% c("cumulative_infections",
                                                       "infections",
                                                       "Reff",
                                                       "hospital_incidence",
                                                       "ICU_incidence"))
afg_data<-subset(afg_data, afg_data$scenario == "Maintain Status Quo")

afg_data <- afg_data %>%
  select(c(date, compartment, y_mean, scenario, iso3c))

# Calculate a rolling weekly sum
# using zoo - not sure i understand this
window <- 7
afg_data_test <- afg_data %>%
  group_by(compartment) %>%
  mutate(sum_week = ~ zoo::rollapply(., window,
                                 FUN=function(x) sum(x, na.rm=TRUE),
                                 fill=c(first(.), NA, last(.)))) %>%
  ungroup()

afg_data_test<-afg_data %>%
  group_by(week = week(date), compartment) %>%
  summarise(value = mean(y_mean))
