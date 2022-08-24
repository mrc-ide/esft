#' Produce Weekly Summary of HCWs
#'
#' @param params
#' @param beds
#' @param country_capacity
#' @param data
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
hcws_weekly<-function(params,
                      beds,
                      country_capacity,
                      data,
                      starting_date)
                      {
  # add exists part here

}
