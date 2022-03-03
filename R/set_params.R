#' Sets country parameters.
#'
#' @param country
#' @param iso3c
#'
#' @return List of country name, iso3c code, and population.
#' @export
set_country <- function(country=NULL,
                       iso3c = NULL) {
  ## country route
  if(!is.null(country)) {
    country<-as.character(country)
    if(!country %in% unique(esft::who$country_name)){
      stop("Country not found")
    }
    population <- esft::who$population[who$country_name == country]
    income_group <- esft::who$income_group[who$country_name == country]
    iso3c<-countrycode::countrycode(country, origin = 'country.name',
                                    destination = 'iso3c')
  }

  # iso3c route
  if(!is.null(iso3c)) {
    iso3c<-as.character(iso3c)
    if(!iso3c %in% unique(esft::who$country_code)){
      stop("Iso3c not found")
    }
    population <- esft::who$population[who$country_code == iso3c]
    income_group <- esft::who$income_group[who$country_code == iso3c]
    country<-countrycode::countrycode(iso3c, origin = 'iso3c', destination =
                                        'country.name')
  }

  country_params<- list(country=country,
                        iso3c = iso3c,
                        population=population
                        income_group=income_group)
  return(country_params)
}

#' Set case severity distribution
#'
#' @param cum_cases Enter known diagnosed number of cumulative cases currently
#' in country. Must be at least '1'.
#' @param prop_mild1
#' @param prop_modI
#' @param prop_sevI
#' @param prop_critI
#'
#' @return List of proportions.
#' @export
set_cases <- function(cum_cases = NULL,
                        # case severity distribution
                        prop_mildI = NULL,
                        prop_modI = NULL,
                        prop_sevI = NULL,
                        prop_critI = NULL) {
  if(prop_mildI < 0 | prop_modI <0 | prop_sevI <0 | prop_critI <0 ){
    stop("Proportions must be more than 0.")
  } else if(prop_mildI > 1 | prop_modI >1 | prop_sevI >1 | prop_critI >1 )){
    stop("Proportions must be less than 1.")
  }



  # if not provided, set the minimum required amount
  if(is.null(cum_cases)){
    cum_cases <- 1
  }
  if(cum_cases <0){
    stop("Cumulative cases must be above 0")
  }
  # case severity distribution
  if(is.null(prop_mildI)){
    prop_mildI <-0.4
  }
  if(is.null(prop_modI)){
    prop_modI <-0.4
  }
  if(is.null(prop_sevI)){
    prop_sevI <-0.15
  }
  if(is.null(prop_critI)){
    prop_critI <-0.05
  }
  ##### check if there's any adjustment ------------------------------------------------------------------------
  if(prop_mildI + prop_modI + prop_sevI + prop_critI != 1 ){
    stop("The sum of the proportions of mild, moderate, severe, and critical
         patients must equal to 1.")
  }

  case_params<- list(cum_cases = cum_cases,
                     # case severity distribution
                     prop_mildI = prop_mildI,
                     prop_modI = prop_modI,
                     prop_sevI =  prop_sevI,
                     prop_critI = prop_critI)
  return(case_params)
}

#' Set average lengths of stay by case severity, in days.
#'
#' @param stay_mild Length of stay in isolation.
#' @param stay_mod Length of stay in isolation.
#' @param stay_sev Length of stay in hospital.
#' @param stay_crit Length of stay in hospital.
#'
#' @return Stay list.
#' @export
set_stays <- function(stay_mild = NULL, #in isolation
                      stay_mod = NULL,
                      # in hospital
                      stay_sev = NULL,
                      stay_crit = NULL) {

  # length of stay by case severity in days
  # in isolation
  if(is.null(stay_mild)){
    stay_mild <-2
  }
  if(is.null(stay_mod)){
    stay_mod <-2
  }
  # in hospital
  if(is.null(stay_sev)){
    stay_sev <-1
  }
  if(is.null(stay_crit)){
    stay_crit <-2
  }

  if(stay_mild < 0 | stay_mod <0 | stay_sev <0 | stay_crit <0 ){
    stop("Length of stay in isolation or hospital must be greater than 1.")
  }
  # mild and moderate in isolation, severe and critical in hospital
  stay_params<- list(stay_mild = stay_mild,
                     stay_mod = stay_mod,
                     stay_sev = stay_sev,
                     stay_crit = stay_crit)

  return(stay_params)
}

#' Set IFRs
#'
#' @param IFR_sev
#' @param IFR_crit
#'
#' @return
#' @export
set_ifrs <- function(IFR_sev = NULL,
                     IFR_crit = NULL) {

  # case fatality rate
  if(is.null(IFR_sev)){
    IFR_sev <-0.134
  }
  if(is.null(IFR_crit)){
    IFR_crit <-0.5
  }
  if(IFR_sev < 0 | IFR_crit <0 | IFR_sev>1 | IFR_crit >1){
    stop("Infection fatality ratios must be greater than 0 and less than 1.")
  }

  # mild and moderate in isolation, severe and critical in hospital
  ifrs<- list(IFR_sev = IFR_sev,
              IFR_crit = IFR_crit)

  return(ifrs)
}

#' Get HCW counts ------------------------------------------------------------------------  add in more categories? if not, where else to get ?
#'
#' @param
#' @param
#'
#' @return
#' @export
set_hcw <- function(n_hcws = NULL, # dpendent on country, so is number bioeng etc - might also be dependent on case estimation
                    iso3c = NULL,
                    country=NULL,
                    n_cleaners = NULL, # this gets from staffing tool
                    # these are assumed ratios per hospital
                    n_bioeng = NULL,
                    n_ambulanceworkers=NULL) {
  # The who did some capping here that I can't figure out. Need to ask Luke.
  # HCW
  if(!is.null(country)) {
    country<-as.character(country)
    if(!country %in% unique(esft::who$country_name)){
      stop("Country not found")
    }
    n_hcws <- esft::who$doctors[who$country_name == country] +
      esft::who$nurses[who$country_name == country]
    iso3c<-esft::who$country_code[who$country_name==country]
  }

  # iso3c route
  if(!is.null(iso3c)) {
    iso3c<-as.character(iso3c)
    if(!iso3c %in% unique(esft::who$country_code)){
      stop("Iso3c not found")
    }
    n_hcws <- esft::who$doctors[who$country_code == iso3c] +
      esft::who$nurses[who$country_code == iso3c]
    country<-esft::who$country_name[who$country_code==iso3c]
  }
  if(is.null(perc_hcws_not_covid)){
    perc_hcws_not_covid <-0.4
  }
  if(is.null(perc_hcws_treat_covid)){
    perc_hcws_treat_covid <-0.53
  }
  if(is.null(perc_hcws_screen_covid)){
    perc_hcws_screen_covid <-0.07
  }
  ##### check adds to 1 ------------------------------------------------------------------------
  # mild and moderate in isolation, severe and critical in hospital

  # maybe add the other HCWs? like the categories, if will pull in from HWFE or WHO tool anyways? ------
  n_inf_caregivers_hosp = NULL
  n_inf_caregivers_isol = NULL

  hcw_params<- list(n_hcws = n_hcws,
                    perc_hcws_not_covid = perc_hcws_not_covid,
                    perc_hcws_treat_covid = perc_hcws_treat_covid,
                    perc_hcws_screen_covid = perc_hcws_screen_covid)

  return(hcw_params)
}
#' Set HCWs ------------------------------------------------------------------------  add in more categories? if not, where else to get ?
#'
#' @param
#' @param
#'
#' @return
#' @export
set_hcw <- function(n_hcws = NULL, # dpendent on country, so is number bioeng etc - might also be dependent on case estimation
                 # input percentages
                    perc_hcws_not_covid = NULL,
                 perc_hcws_treat_covid = NULL,
                 perc_hcws_screen_covid = NULL,
                 # ratios
                 n_inf_caregivers_hosp = NULL,
                 n_inf_caregivers_isol = NULL,

                 n_cleaners = NULL, # this gets from staffing tool
                 # these are assumed ratios per hospital
                 n_bioeng = NULL,
                 n_ambulanceworkers=NULL) {
  # The who did some capping here that I can't figure out. Need to ask Luke.
  # HCW
  if(!is.null(country)) {
    country<-as.character(country)
    if(!country %in% unique(esft::who$country_name)){
      stop("Country not found")
    }
    n_hcws <- esft::who$doctors[who$country_name == country] +
      esft::who$nurses[who$country_name == country]
  }

  # iso3c route
  if(!is.null(iso3c)) {
    iso3c<-as.character(iso3c)
    if(!iso3c %in% unique(esft::who$country_code)){
      stop("Iso3c not found")
    }
    n_hcws <- esft::who$doctors[who$country_code == iso3c] +
      esft::who$nurses[who$country_code == iso3c]

  }
  if(is.null(perc_hcws_not_covid)){
    perc_hcws_not_covid <-0.4
  }
  if(is.null(perc_hcws_treat_covid)){
    perc_hcws_treat_covid <-0.53
  }
  if(is.null(perc_hcws_screen_covid)){
    perc_hcws_screen_covid <-0.07
  }
  ##### check adds to 1 ------------------------------------------------------------------------
  # mild and moderate in isolation, severe and critical in hospital

  # maybe add the other HCWs? like the categories, if will pull in from HWFE or WHO tool anyways? ------
  n_inf_caregivers_hosp = NULL
  n_inf_caregivers_isol = NULL

  hcw_params<- list(n_hcws = n_hcws,
                     perc_hcws_not_covid = perc_hcws_not_covid,
                     perc_hcws_treat_covid = perc_hcws_treat_covid,
                     perc_hcws_screen_covid = perc_hcws_screen_covid)

  return(hcw_params)
}
#' Set beds ------------------------------------------------------------------------  need to draw in external
#'
#' @param
#' @param
#'
#' @return
#' @export
set_beds <- function(n_hosp_beds = NULL,
                     perc_beds_not_covid = NULL,
                     perc_beds_sev_covid = NULL,
                     perc_beds_crit_covid = NULL,
                     n_hosp_beds_per_care_unit = NULL) {

  bed_params<- list(n_hosp_beds = n_hosp_beds,
                    perc_beds_not_covid = perc_beds_not_covid,
                    perc_beds_sev_covid = perc_beds_sev_covid,
                    perc_beds_crit_covid = perc_beds_crit_covid,
                    n_hosp_beds_per_care_unit = n_hosp_beds_per_care_unit)

  return(bed_params)
}
#' Set HCW distributions ----------------------------------------------------------------------- NEED HWFE INPUTS
#'
#' @param
#' @param
#'
#' @return
#' @export
get_hcws_per_bed <- function(hcws_per_bed = NULL,
                 cleaners_per_bed = NULL,
                 ambulancews_per_bed = NULL,
                 bioengs_per_bed = NULL) {

  # used with the inputs from the HWFE tool
  # need to add in HWFE tool inputs
  hcws_per_bed # weighted averaged based on capped beds per week - how many bds you have

  cleaners_per_bed
  ambulancews_per_bed
  bioengs_per_bed

  # mild and moderate in isolation, severe and critical in hospital
  hcw_allocations<- list(hcws_per_bed = hcws_per_bed,
                    cleaners_per_bed = cleaners_per_bed,
                    ambulancews_per_bed = ambulancews_per_bed,
                    bioengs_per_bed = bioengs_per_bed)

  return(hcw_allocations)
}

#' Calculate case screened per HCW per day -----------------------------------------------------------------------
#'
#' @param
#' @param
#'
#' @return
#' @export
get_case_screened_per_hcw_per_day <- function() {

# look at the complex capping again here

  return(cases_screened_per_hcw_per_day)
}



#' Set mechanical ventilation parameters ------------------------------------
#'
#' @param
#' @param
#'
#' @return
#' @export
set_mv_params <- function(perc_crit_inv_mv = NULL,
                           perc_crit_noninv_mv = NULL,

                           o2_flow_sev = NULL,
                           o2_flow_crit_inv_mv = NULL,
                           o2_flow_crit_noninv_mv = NULL) {
  # labs and testing
  # oxygen use
  perc_crit_inv_mv
  perc_crit_noninv_mv

  o2_flow_sev
  o2_flow_crit_inv_mv
  o2_flow_crit_noninv_mv

  mv_params<- list(perc_crit_inv_mv = perc_crit_inv_mv,
                   perc_crit_noninv_mv = perc_crit_noninv_mv,

                   o2_flow_sev = o2_flow_sev,
                   o2_flow_crit_inv_mv = o2_flow_crit_inv_mv,
                   o2_flow_crit_noninv_mv = o2_flow_crit_noninv_mv)

  return(mv_params)
}


