#' Produce Weekly Summary of HCWs
#'
#' @description This function takes some of the HCW cap options and calculates
#' the section in the `Weekly Summary` tab marked HCW and staff.
#' This will then be used in some sort of capacity mapping/forecasting.
#'
#' @param params
#' @param hwfe
#' @param data
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
hcws_weekly<-function(params, # maybe this should already by a subsetted country vector of params?
                      hwfe,
                      data,
                      country_capacity,
                      hcws_in_region = NULL) # seems weird to have this at all
  # - i think in calculation, it served as both a way to get a proxy value in
  # and also allow for manual tinkering
                      {
  # add exists part here
  if(is.null(hcws_in_region)){
    hcws_inpatients_capped <- params$perc_hcws_treat_covid*params$n_hcws
    # n_hcws = num nurses + num doctors
    hcws_screening_capped <- params$perc_hcws_screen_covid*params$n_hcws

  } else { # maybe if n_hcws is null?
    hcws_inpatients_capped <- params$perc_hcws_treat_covid*hcws_in_region
    # n_hcws = num nurses + num doctors
    hcws_screening_capped <- params$perc_hcws_screen_covid*hcws_in_region

  }

  lab_staff_capped

  # here we need HWFE
  cleaners_inpatient_capped
}
