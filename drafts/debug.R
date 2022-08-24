rm(list=ls())
# let's test out functions
source("R/country_capacity.R")
source("R/parameters.R")
source("R/load_imperial_data.R")
source("drafts/weekly_summary.R")

load("data/who.rda")
load("data/population.rda")

all <- readRDS("data-raw/all.Rds")

afg_params <- get_country_capacity(iso3c="AFG")
afg_beds <- get_beds(afg_params)
afg_params <- merge(afg_params, afg_beds)
params <- get_parameters()

afg_data<-subset(all, all$iso3c == "AFG")

params <- merge(afg_params, params)
afg_summary <- weekly_summary(iso3c="AFG",
                              params=params,
                              data=all)

# get rid of the tidyr and dplyr dependencies
# do data processing and parameter setting before weekly summary
# maybe add exists calls
# finish documentsation
sink("mylist.txt")
cat(paste0("#'   \\item{",names(afg_summary), "}{xyz}\n"))
sink()


# dealing with structuring commodities forecast
input <- user_input()

params <- append(params, input)

# maybe have the actual sequence be that the user themselves subset amount by starttime, etc
# might also need different reusability multiplier params per category
