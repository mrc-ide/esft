#' @title Commodities by week - combination of diagnostics, ppe, hygiene, treatment by week
#' plus gives total
#' maybe i should add in total cost here?
#'
#'
#' @description
#' Theres the issue now that I think the amount left depends on reusability
#' So i think by row
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
commodities_weekly <- function(params, equipment, weekly_summary) {

  case_management_weekly <- case_management_forecast(params, equipment, weekly_summary)
  hygiene_weekly <- hygiene_forecast(params, equipment, weekly_summary)
  ppe_weekly <- ppe_forecast(params, equipment, weekly_summary)
  diagnostics_weekly <- diagnostics_forecast(params, equipment, weekly_summary)

  commodities <- rbind(case_management_weekly, hygiene_weekly, ppe_weekly, diagnostics_weekly)
}

#' @title Case management weekly: accessories, consumables, and biomedical equipment
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
case_management_forecast <- function(params, equipment, weekly_summary) {

}

#' @title Hygiene weekly
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
hygiene_forecast <- function(params, equipment, weekly_summary) {
  # unnecessary, but helps me think
  hygiene <- subset(equipment, equipment$group == "Hygiene")

  # check for reusability

  # need to first get HCW caps into weekly summary
  # then I can pass the HCW caps here

  # per hcw in hospital
    # amount per hcw or per bed per day
    # times number of capped hcws
    # times either reusable or non reusable multiplier
  # per patient in hospital

  # for isolating case

}
#' @title PPE need weekly
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
ppe_forecast <- function(params, equipment, weekly_summary) {
  ppe <- subset(equipment, equipment$group== "PPE")
}
#' @title Diagnostics week
#'
#' @description
#'
#' @param params I think this needs to be the user dashboard/input activity
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
diagnostics_forecast <- function(params, equipment, weekly_summary) {

}
