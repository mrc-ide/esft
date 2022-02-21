# data wrangling

who<-readxl::read_excel("data-raw/who_summary_data.xlsx", sheet="who_summary_data")
save(who, file="data/who.rda")
