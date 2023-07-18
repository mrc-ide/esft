data <- subset(icl_data, icl_data$iso3c == "AFG")
data <- subset(data, data$scenario == "Maintain Status Quo")

library(tidyr)
library(dplyr)
library(magrittr)

data <- subset(who_data, who_data$Country == "Afghanistan")
source("R/cases_weekly.R")
source("R/patients_weekly.R")
cases <- cases_weekly(params,
                      capacity,
                      test_strategy_params = test_strat,
                      data = data,
                      user = user,
                      data_source = "WHO"
)
# the first few with severe and critical patients dont work - only one missing fordischarged_crit - but thats not present
patients <- patients_weekly(params,
                            capacity,
                            data = cases,
                            data_source = "WHO"
)
