load("data/throughput.rda")
calc_diagnostic_capacity <- function(country_test_capacity,
                                     throughput,
                                     shifts_per_day = 1, # vector
                                     hours_per_shift) {
  if (!(is.null(shifts_per_day))) {
    if (length(shifts_per_day) == 1) {
      shifts_per_day <- data.frame(shifts_day = rep(shifts_per_day,
                                                    length(throughput$platform_key)))
      shifts_per_day$platform_key <- throughput$platform_key
    }
    throughput <- merge(throughput, shifts_per_day, by = c("platform_key","shifts_day"))
  }
  capacity <- merge(throughput, country_test_capacity)
  capacity <- merge(capacity, hours_per_shift,
                    by.x = "shifts_day",
                    by.y = "shifts"
  )

  capacity <- capacity %>%
    dplyr::mutate(throughput_per_day = case_when(
      .data$hours == 8 ~ throughput_8hrs,
      .data$hours == 16 ~ throughput_16hrs,
      .data$hours == 24 ~ throughput_24hrs,
      TRUE ~ NA_real_
    )) %>%
    dplyr::select(-c(
      .data$throughput_8hrs, .data$throughput_16hrs,
      .data$throughput_24hrs
    ))

  # calculate the testing capacity available - max, and covid
  capacity$total_test_capacity <- capacity$modules_activated *
    capacity$days_week * capacity$throughput_per_day
  capacity$covid_test_capacity <- capacity$total_test_capacity *
    capacity$covid_capacity

  return(capacity)
}

diagnostic_capacity <- calc_diagnostic_capacity(country_diagnostic_capacity =
                                                  country_test_capacity,
                                                throughput, hours_per_shift =
                                                  hours_per_shift,
                                                shifts_per_day = 1)


# i'm saving my microbenchmark code cuz i'm proud of it
mbm = microbenchmark(
  base = replace(ref_hcws, is.na(ref_hcws),
                 overrides[match(names(ref_hcws),
                                 names(overrides))][is.na(ref_hcws)]),
  utils = modifyList(ref_hcws, overrides[intersect(names(overrides),
                                                   names(which(is.na(ref_hcws))))]),
  times=100
)
mbm

modifyList(ref_hcws, overrides[intersect(names(overrides), names(which(is.na(ref_hcws))))])
l1 <- replace(ref_hcws, is.na(ref_hcws),
              overrides[match(names(ref_hcws),
                              names(overrides))][is.na(ref_hcws)])
a=names(ref_hcws)%in%names(overrides)&is.na(ref_hcws)
replace(ref_hcws, a, overrides[names(which(a))])
