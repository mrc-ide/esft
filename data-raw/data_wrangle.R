# data wrangling

who<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="who_summary_data")
usethis::use_data(who, overwrite = TRUE, internal = FALSE)

wb_beds<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="wb_beds")
usethis::use_data(wb_beds, overwrite = TRUE, internal = FALSE)

bed_nr_proxy<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="bed_nr_proxy")
usethis::use_data(bed_nr_proxy, overwrite = TRUE, internal = FALSE)

bed_perc_crit_proxy<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="bed_perc_crit_proxy")
usethis::use_data(bed_perc_crit_proxy, overwrite = TRUE, internal = FALSE)

hwfe<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="hwfe")
usethis::use_data(hwfe, overwrite = TRUE, internal = FALSE)

diagnostics<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="platform_mapping")
usethis::use_data(diagnostics, overwrite = TRUE, internal = FALSE)

pharmaceuticals<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="pharmaceutical")
usethis::use_data(pharmaceuticals, overwrite = TRUE, internal = FALSE)

equipment<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="equipment")
usethis::use_data(equipment, overwrite = TRUE, internal = FALSE)

transmission_scenarios<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="transmission_scenarios")
usethis::use_data(transmission_scenarios, overwrite = TRUE, internal = FALSE)

diagnostics<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="platform_mapping")
usethis::use_data(diagnostics, overwrite = TRUE, internal = FALSE)

