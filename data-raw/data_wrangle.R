# data wrangling

who<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="who_summary_data")
save(who, file="data/who.rda")
wb_beds<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="wb_beds")
save(wb_beds, file="data/wb_beds.rda")
bed_nr_proxy<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="bed_nr_proxy")
save(bed_nr_proxy, file="data/bed_nr_proxy.rda")
bed_perc_crit_proxy<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="bed_perc_crit_proxy")
save(bed_perc_crit_proxy, file="data/bed_perc_crit_proxy.rda")
hwfe<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="hwfe")
save(hwfe, file="data/hwfe.rda")
