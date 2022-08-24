#' Produce Weekly Summary of Patients
#'
#' @param params
#' @param data Weekly summary dataframe - from cases_weekly
#' @param beds
#' @param country_capacity
#' @param starting_date
#'
#' @return Dataframe of weekly summary
#' \describe{
#'   \item{week_begins}{Date the week summarized begins}
#'   \item{week_ends}{Date the week summarized ends}
#'   \item{rem_severe_cases}{xyz}
#'   \item{rem_critical_cases}{xyz}
#'}
#' @import dplyr
#' @importFrom data.table first
#' @import countrycode
#'
#' @export
patients_weekly<-function(params,
                      data,
                      beds,
                      country_capacity,
                      starting_date)
{
  # add exists part here
  # the total bed capacity was calculated in the input excel sheet
  # basically here you take the min of the new cases by severity or the num beds available
  # and theoretically the control for time spent in bed (removal) should have already been done



}
