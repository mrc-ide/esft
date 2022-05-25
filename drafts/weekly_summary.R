#' Produce Weekly Summary
#' @param
#' @return dataframe of weekly summary
#' \describe{
#'   \item{starting_data}{first date of the week}
#'   \item{current_week}{Week number}
#'   # CALCULATES UNCAPPED CASE INCIDENCE
#'   \item{total_uncapped_new_cases}{Total new cases presenting each week - uncapped}
#'   \item{new_uncapped_mild_cases}{Subset of total new cases presenting each week that are mild}
#'   \item{new_uncapped_mod_cases}{Subset of total new cases presenting each week that are moderate}
#'   \item{new_uncapped_sev_cases}{Subset of total new cases presenting each week that are severe}
#'   \item{new_uncapped_crit_cases}{Subset of total new cases presenting each week that are critical}
#'   \item{new_uncapped_sus_cases}{Subset of total new cases presenting each week that are suspected but negative}
#'   # patients that get admitted with caps
#'
#'   # uncapped sick patients per week excluding those who have been discharged/removed
#'
#'   # sick patients in beds per week (so the above with bedcaps)
#'
#'   # patients recovering or dying from illness, per week
#'   # patients discharged from beds last week
#'   \item{year}{Starting year of 5 year group, i.e. 1960 for 1960-64, etc. From
#'   1950-2100.}
#'   \item{gender}{"both", "female", or "male"}
#'   \item{value}{Age specific mortality rate, or the central rate of
#'   mortality, mx.}
#'   \item{income_group}{World Bank country income group.}
#' }
#'
#' @export
weekly_summary<-function(country=NULL, iso3c = NULL, starting_date = NULL,
                         forecast_length = NULL, delivery_leadtime = NULL,
                         model = NULL, transmission_scenario = "Medium"){

  test_data<-read.csv("data-raw/test_forecast_data.csv")

  output <- c()
}
