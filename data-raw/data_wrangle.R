# data wrangling
library(readxl)
library(usethis)
library(countrycode)

# Summary information from the HWO --------------
who <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "who_summary_data"
)
usethis::use_data(who, overwrite = TRUE, internal = FALSE)

wb_beds <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "wb_beds"
)
usethis::use_data(wb_beds, overwrite = TRUE, internal = FALSE)

bed_nr_proxy <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "bed_nr_proxy"
)
usethis::use_data(bed_nr_proxy, overwrite = TRUE, internal = FALSE)

bed_perc_crit_proxy <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "bed_perc_crit_proxy"
)
usethis::use_data(bed_perc_crit_proxy, overwrite = TRUE, internal = FALSE)

hwfe <- readxl::read_excel("data-raw/who_summary_data.xlsx", sheet = "hwfe")
usethis::use_data(hwfe, overwrite = TRUE, internal = FALSE)

population <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "population"
)
usethis::use_data(population, overwrite = TRUE, internal = FALSE)

# Equipment ratios by HCW and patient ---------------
equipment <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "equipment"
)
equipment$reusable <- ifelse(equipment$reusable == "No", FALSE, TRUE)
usethis::use_data(equipment, overwrite = TRUE, internal = FALSE)

# Non-Covid Essentials by HCW cadre ---------------
noncovid <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "noncovid_essentials"
)
noncovid$reusable <- ifelse(noncovid$reusable == "No", FALSE, TRUE)
usethis::use_data(noncovid, overwrite = TRUE, internal = FALSE)

# Pharmaceuticals ---------------------
pharmaceuticals <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "pharmaceutical"
)
pharmaceuticals$covid_specific <- ifelse(pharmaceuticals$covid_specific == "No", FALSE, TRUE)
usethis::use_data(pharmaceuticals, overwrite = TRUE, internal = FALSE)

# Diagnostics ---------------------
diagnostics <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "platform_mapping"
)

diagnostics$country_code <- countrycode::countrycode(diagnostics$country_name,
  origin = "country.name",
  destination = "iso3c"
)

sum(is.na(diagnostics$country_code))
subset(diagnostics, is.na(diagnostics$country_code))
# Micronesia, Kosovo, Channel Islands
# micronesia - FSM
# kosovo - NA
# channel islands - variable

diagnostics$country_code[diagnostics$country_name == "Micronesia"] <- "FSM"
usethis::use_data(diagnostics, overwrite = TRUE, internal = FALSE)

transmission_scenarios <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "transmission_scenarios"
)
usethis::use_data(transmission_scenarios, overwrite = TRUE, internal = FALSE)

capacity_perc <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "capacity_perc"
)
usethis::use_data(capacity_perc, overwrite = TRUE, internal = FALSE)

hours_per_shift <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "hours_per_shift"
)
usethis::use_data(hours_per_shift, overwrite = TRUE, internal = FALSE)

icl_data <- readRDS("data-raw/all.Rds")
usethis::use_data(icl_data, overwrite = TRUE, internal = FALSE)

throughput <- readxl::read_excel("data-raw/who_summary_data.xlsx",
  sheet = "throughput"
)
usethis::use_data(throughput, overwrite = TRUE, internal = FALSE)
