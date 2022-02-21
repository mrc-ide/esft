#' Sets parameters.
#'
#' Will be updated & probably broken down into more functions.
#' For now, will only load the parameters strictly necessary for my individual calculations.
#'
#'
#' @return List of parameters
#' @export
set_params <- function(country=NULL,
                       iso3c = NULL,

                       cum_cases = NULL,
                       # case severity distribution
                       perc_mildI = NULL,
                       perc_modI = NULL,
                       perc_sevI = NULL,
                       perc_critI = NULL,

                       # length of stay by case severity
                       # in isolation
                       stay_mild = NULL,
                       stay_mod = NULL,
                       # in hospital
                       stay_sev = NULL,
                       stay_crit = NULL,

                       # case fatality rate
                       IFR_sev = NULL,
                       IFR_crit = NULL,

                       # HCW
                       n_hcws = NULL,
                       perc_hcws_not_covid = NULL,
                       perc_hcws_treat_covid = NULL,
                       perc_hcws_screen_covid = NULL,
                       hcws_per_bed = NULL,
                       cleaners_per_bed = NULL,
                       ambulancews_per_bed = NULL,
                       bioengs_per_bed = NULL,

                       cases_screened_per_hcw_per_day = NULL,

                       n_inf_caregivers_hosp = NULL,
                       n_inf_caregivers_isol = NULL,
                       # hospital and care infrastructure
                       n_hosp_beds = NULL,
                       perc_beds_not_covid = NULL,
                       perc_beds_sev_covid = NULL,
                       perc_beds_crit_covid = NULL,

                       n_hosp_beds_per_care_unit = NULL,
                       # labs and testing
                       # oxygen use
                       perc_crit_inv_mv = NULL,
                       perc_crit_noninv_mv = NULL,

                       o2_flow_sev = NULL,
                       o2_flow_crit_inv_mv = NULL,
                       o2_flow_crit_noninv_mv = NULL) {

  # parameter types:
  # demography
  # methodology copied from population.R from mrc-ide squire
  ## country route
  if(!is.null(country)) {
    country<-as.character(country)
    if(!country %in% unique(esft::who$country_name)){
      stop("Country not found")
    }
    population <- esft::who$population[who$country_name == country]
  }

  # iso3c route
  if(!is.null(iso3c)) {
    iso3c<-as.character(iso3c)
    if(!iso3c %in% unique(esft::who$country_code)){
      stop("Iso3c not found")
    }
    population <- esft::who$population[who$country_code == iso3c]
  }


  # current known cases
  # if not provided, set the minimum required amount
  if(is.null(cum_cases)){
    cum_cases <- 1
  }
  # case severity distribution
  if(is.null(perc_mildI)){
    perc_mildI <-0.4
  }
  if(is.null(perc_modI)){
    perc_modI <-0.4
  }
  if(is.null(perc_sevI)){
    perc_sevI <-0.15
  }
  if(is.null(perc_critI)){
    perc_critI <-0.05
  }

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

  # case fatality rate
  if(is.null(IFR_sev)){
    IFR_sev <-0.134
  }
  if(is.null(IFR_crit)){
    IFR_crit <-0.5
  }

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

# used with the inputs from the HWFE tool
  # need to add in HWFE tool inputs
  hcws_per_bed # weighted averaged based on capped beds per week - how many bds you have

  cleaners_per_bed
  ambulancews_per_bed
  bioengs_per_bed

  cases_screened_per_hcw_per_day

  n_inf_caregivers_hosp
  n_inf_caregivers_isol
  # hospital and care infrastructure
  n_hosp_beds
  perc_beds_not_covid
  perc_beds_sev_covid
  perc_beds_crit_covid

  n_hosp_beds_per_care_unit
  # labs and testing
  # oxygen use
  perc_crit_inv_mv
  perc_crit_noninv_mv

  o2_flow_sev
  o2_flow_crit_inv_mv
  o2_flow_crit_noninv_mv
  # equipment use
  input_params<- list(country=country,
                iso3c = iso3c,
                population = population,
                cum_cases = cum_cases,
                # case severity distribution
                perc_mildI = perc_mildI,
                perc_modI = perc_modI,
                perc_sevI =  perc_sevI,
                perc_critI = perc_critI,

                # length of stay by case severity
                # in isolation
                stay_mild = stay_mild,
                stay_mod = stay_mod,
                # in hospital
                stay_sev = stay_sev,
                stay_crit = stay_crit,

                # case fatality rate
                IFR_sev = IFR_sev,
                IFR_crit = IFR_crit,

                # HCW
                n_hcws = n_hcws,
                perc_hcws_not_covid = perc_hcws_not_covid,
                perc_hcws_treat_covid = perc_hcws_treat_covid,
                perc_hcws_screen_covid = perc_hcws_screen_covid,
                hcws_per_bed = hcws_per_bed,
                cleaners_per_bed = cleaners_per_bed,
                ambulancews_per_bed = ambulancews_per_bed,
                bioengs_per_bed = bioengs_per_bed,

                cases_screened_per_hcw_per_day = cases_screened_per_hcw_per_day,

                n_inf_caregivers_hosp = n_inf_caregivers_hosp,
                n_inf_caregivers_isol = n_inf_caregivers_isol,
                # hospital and care infrastructure
                n_hosp_beds = n_hosp_beds,
                perc_beds_not_covid = perc_beds_not_covid,
                perc_beds_sev_covid = perc_beds_sev_covid,
                perc_beds_crit_covid = perc_beds_crit_covid,

                n_hosp_beds_per_care_unit = n_hosp_beds_per_care_unit,
                # labs and testing
                # oxygen use
                perc_crit_inv_mv = perc_crit_inv_mv,
                perc_crit_noninv_mv = perc_crit_noninv_mv,

                o2_flow_sev = o2_flow_sev,
                o2_flow_crit_inv_mv = o2_flow_crit_inv_mv,
                o2_flow_crit_noninv_mv = o2_flow_crit_noninv_mv)
  return(inputparams)
}
