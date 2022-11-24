#' @title Commodities by week - combination of diagnostics, ppe, hygiene, treatment by week
#' plus gives total
#' maybe i should add in total cost here?
#'
#'I want to add in leadtime some other way. I think
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

  # maybe add in leadtime here?
  # and get period for which forecasted for
  # like subset weekly summary here ?

  case_management_weekly <- case_management_forecast(params, equipment, weekly_summary)
  hygiene_weekly <- hygiene_forecast(params, equipment, weekly_summary)
  ppe_weekly <- ppe_forecast(params, equipment, weekly_summary)
  diagnostics_weekly <- diagnostics_forecast(params, equipment, weekly_summary)

  # maybe, get totals?
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
#' @param input User input
#' @param equipment This should be the data frame of equipment need
#' @param weekly_summary This should be a weekly summary data.frame, containing
#' the requisite columns
#'
#'
#' @export
hygiene_weekly<- function(input, equipment, hcws) {
  # unnecessary, but helps me think
  hygiene <- subset(equipment, equipment$group == "Hygiene")

  # check for reusability - if reusable, only need one per week (?) - what it
  # says in ESFT vs if not, need 7 per week
  reusable_multiplier <- ifelse(hygiene$reusable == TRUE, 1, 7)


  hygiene[,c(8:24)] <- hygiene[,c(8:24)]*reusable_multiplier
  # need to first get HCW caps into weekly summary
  # then I can pass the HCW caps here
  list2env(setNames(split(as.matrix(hygiene[,c(3,8:12)]),
                          row(hygiene[,c(3,8:12)])), paste0("Row",1:4)), envir=.GlobalEnv)


  chlorine <- hcws[,c("cleaners_inpatient_capped",
                      "amb_personnel_inpatient_capped",
                      "bio_eng_inpatient_capped")
                   ] * t(inpatient_hcw_needs[1,c(3,5,6)])

  chlorine$total <- rowSums(chlorine, na.rm = TRUE)
  inpatient_hcw_needs <- as.data.frame(t(hygiene[,c(3,8:12)]))
  names(inpatient_hcw_needs) <- inpatient_hcw_needs[1,]
  inpatient_hcw_needs <- inpatient_hcw_needs[-1,]
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
