#' @title Given specified parameters, calculates diagnostic country capacity.
#'
#' @param country_diagnostic_capacity Capacity called from the get_country_diagnostic_capacity function
#' @param throughput Throughput data
#' @param hours_per_shift Hours per shift
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return
#'
#' @export
calc_diagnostic_capacity <- function(country_diagnostic_capacity, # get_country_diagnostic_capacity
                                     throughput,
                                     hours_per_shift) {
  capacity <- merge(throughput, country_diagnostic_capacity)
  capacity <- merge(capacity, hours_per_shift, by.x = "shifts_day", by.y = "shifts")

  capacity <- capacity %>%
    mutate(throughput_per_day = case_when(
      hours == 8 ~ throughput_8hrs,
      hours == 16 ~ throughput_16hrs,
      hours == 24 ~ throughput_24hrs,
      TRUE ~ NA_real_
    )) %>%
    select(-c(throughput_8hrs, throughput_16hrs, throughput_24hrs))

  # calculate the testing capacity available - max, and covid
  capacity$total_test_capacity <- capacity$modules_activated * capacity$days_week * capacity$throughput_per_day
  capacity$covid_test_capacify <- capacity$total_test_capacity * capacity$covid_capacity


  return(capacity)
}
