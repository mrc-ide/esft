#' Sets country parameters.
#'
#' @param country
#' @param iso3c
#'
#' @return List of country name, iso3c code, and population.
#' @export
set_country <- function(country = NULL,
                        iso3c = NULL) {
  ## country route
  if (!is.null(country)) {
    country <- as.character(country)
    if (!country %in% unique(esft::who$country_name)) {
      stop("Country not found")
    }
    population <- esft::who$population[who$country_name == country]
    income_group <- esft::who$income_group[who$country_name == country]
    iso3c <- countrycode::countrycode(country,
      origin = "country.name",
      destination = "iso3c"
    )
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Iso3c not found")
    }
    population <- esft::who$population[who$country_code == iso3c]
    income_group <- esft::who$income_group[who$country_code == iso3c]
    country <- countrycode::countrycode(iso3c,
      origin = "iso3c", destination =
        "country.name"
    )
  }

  country_params <- list(
    country = country,
    iso3c = iso3c,
    population = population,
    income_group = income_group
  )
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
  if (prop_mildI < 0 | prop_modI < 0 | prop_sevI < 0 | prop_critI < 0) {
    stop("Proportions must be more than 0.")
  } else if (prop_mildI > 1 | prop_modI > 1 | prop_sevI > 1 | prop_critI > 1) {
    stop("Proportions must be less than 1.")
  }



  # if not provided, set the minimum required amount
  if (is.null(cum_cases)) {
    cum_cases <- 1
  }
  if (cum_cases < 0) {
    stop("Cumulative cases must be above 0")
  }
  # case severity distribution
  if (is.null(prop_mildI)) {
    prop_mildI <- 0.4
  }
  if (is.null(prop_modI)) {
    prop_modI <- 0.4
  }
  if (is.null(prop_sevI)) {
    prop_sevI <- 0.15
  }
  if (is.null(prop_critI)) {
    prop_critI <- 0.05
  }
  ##### check if there's any adjustment ------------------------------------------------------------------------
  if (prop_mildI + prop_modI + prop_sevI + prop_critI != 1) {
    stop("The sum of the proportions of mild, moderate, severe, and critical
         patients must equal to 1.")
  }

  case_params <- list(
    cum_cases = cum_cases,
    # case severity distribution
    prop_mildI = prop_mildI,
    prop_modI = prop_modI,
    prop_sevI = prop_sevI,
    prop_critI = prop_critI
  )
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
set_stays <- function(stay_mild = NULL, # in isolation
                      stay_mod = NULL,
                      # in hospital
                      stay_sev = NULL,
                      stay_crit = NULL) {

  # length of stay by case severity in days
  # in isolation
  if (is.null(stay_mild)) {
    stay_mild <- 2
  }
  if (is.null(stay_mod)) {
    stay_mod <- 2
  }
  # in hospital
  if (is.null(stay_sev)) {
    stay_sev <- 1
  }
  if (is.null(stay_crit)) {
    stay_crit <- 2
  }

  if (stay_mild < 0 | stay_mod < 0 | stay_sev < 0 | stay_crit < 0) {
    stop("Length of stay in isolation or hospital must be greater than 1.")
  }
  # mild and moderate in isolation, severe and critical in hospital
  stay_params <- list(
    stay_mild = stay_mild,
    stay_mod = stay_mod,
    stay_sev = stay_sev,
    stay_crit = stay_crit
  )

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
  if (is.null(IFR_sev)) {
    IFR_sev <- 0.134
  }
  if (is.null(IFR_crit)) {
    IFR_crit <- 0.5
  }
  if (IFR_sev < 0 | IFR_crit < 0 | IFR_sev > 1 | IFR_crit > 1) {
    stop("Infection fatality ratios must be greater than 0 and less than 1.")
  }

  # mild and moderate in isolation, severe and critical in hospital
  ifrs <- list(
    IFR_sev = IFR_sev,
    IFR_crit = IFR_crit
  )

  return(ifrs)
}

#' Get HCW counts ------------------------------------------------------------------------  add in more categories? if not, where else to get ?
#'
#' @param n_hcws
#' @param iso3c
#' @param country
#' @param perc_hcws_not_covid
#' @param perc_hcws_treat_covid
#' @param perc_hcws_screen_covid
#'
#' @return
#' @export
set_hcw <- function(n_hcws = NULL, # dependent on country
                    iso3c = NULL,
                    country = NULL,
                    perc_hcws_not_covid = NULL,
                    perc_hcws_treat_covid = NULL,
                    perc_hcws_screen_covid = NULL) {
  if (!is.null(country)) {
    country <- as.character(country)
    if (!country %in% unique(esft::who$country_name)) {
      stop("Country not found")
    }
    n_hcws <- esft::who$doctors[who$country_name == country] +
      esft::who$nurses[who$country_name == country]
    iso3c <- esft::who$country_code[who$country_name == country]
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Iso3c not found")
    }
    n_hcws <- esft::who$doctors[who$country_code == iso3c] +
      esft::who$nurses[who$country_code == iso3c]
    country <- esft::who$country_name[who$country_code == iso3c]
  }
  if (is.null(perc_hcws_not_covid)) {
    perc_hcws_not_covid <- 0.4
  }
  if (is.null(perc_hcws_treat_covid)) {
    perc_hcws_treat_covid <- 0.53
  }
  if (is.null(perc_hcws_screen_covid)) {
    perc_hcws_screen_covid <- 0.07
  }
  ##### check adds to 1 ------------------------------------------------------------------------
  if (perc_hcws_not_covid + perc_hcws_treat_covid + perc_hcws_screen_covid != 1) {
    stop("Percentages of healthcare workers allocated to not treating COVID-19,
         treating COVID-19, and screening for COVID-19 must add up to 1.")
  }


  hcw_params <- list(
    country = country,
    n_hcws = n_hcws,
    perc_hcws_not_covid = perc_hcws_not_covid,
    perc_hcws_treat_covid = perc_hcws_treat_covid,
    perc_hcws_screen_covid = perc_hcws_screen_covid
  )

  return(hcw_params)
}

#' Set beds ------------------------------------------------------------------------  need to draw in external
#'
#' @param iso3c
#' @param country
#' @param n_hosp_beds
#' @param perc_beds_not_covid
#' @param perc_beds_sev_covid
#' @param perc_beds_crit_covid
#' @param n_hosp_beds_per_care_unit
#'
#'
#' @return
#' @export
set_beds <- function(iso3c = NULL,
                     country = NULL,
                     n_hosp_beds = NULL,
                     perc_beds_not_covid = NULL,
                     perc_beds_sev_covid = NULL,
                     perc_beds_crit_covid = NULL,
                     n_hosp_beds_per_care_unit = NULL) {
  if (!is.null(country)) {
    country <- as.character(country)
    if (!country %in% unique(esft::who$country_name)) {
      stop("Country not found")
    }
    n_hosp_beds <- esft::who$beds_total[esft::who$country_name == country]
    perc_beds_crit_covid <- esft::who$perc_icu_beds[esft::who$country_name == country]
    iso3c <- esft::who$country_code[esft::who$country_name == country]
  }

  # iso3c route
  if (!is.null(iso3c)) {
    iso3c <- as.character(iso3c)
    if (!iso3c %in% unique(esft::who$country_code)) {
      stop("Iso3c not found")
    }
    n_hosp_beds <- esft::who$beds_total[esft::who$country_code == iso3c]
    perc_beds_crit_covid <- esft::who$perc_icu_beds[esft::who$country_code == iso3c]
    country <- esft::who$country_name[esft::who$country_code == iso3c]
  }

  if (is.null(perc_beds_not_covid)) {
    perc_beds_not_covid <- 0.4
  }
  if (is.null(perc_beds_sev_covid)) {
    perc_beds_sev_covid <- 1 - perc_beds_not_covid - perc_beds_crit_covid
  }

  ##### check adds to 1 ------------------------------------------------------------------------
  if (perc_beds_not_covid + perc_beds_crit_covid + perc_beds_sev_covid != 1) {
    stop("Percentages of beds allocated to not treating COVID-19 patients, to
         treating severe COVID-19 patients, and to treating critical COVID-19
         patients must add up to 1.")
  }

  if (is.null(n_hosp_beds_per_care_unit)) {
    n_hosp_beds_per_care_unit <- 40
  }

  bed_params <- list(
    iso3c = iso3c,
    country = country,
    n_hosp_beds = n_hosp_beds,
    perc_beds_not_covid = perc_beds_not_covid,
    perc_beds_sev_covid = perc_beds_sev_covid,
    perc_beds_crit_covid = perc_beds_crit_covid,
    n_hosp_beds_per_care_unit = n_hosp_beds_per_care_unit
  )

  return(bed_params)
}
#' Set HCWs per bed ---------------------------------------------------------
#'
#' @param ambulancews_per_bed
#' @param bioengs_per_bed
#' @param n_inf_caregivers_hosp
#' @param n_inf_caregivers_isol
#'
#' @return
#' @export
get_hcws_per_bed <- function(ambulancews_per_bed = NULL,
                             bioengs_per_bed = NULL,
                             n_inf_caregivers_hosp = NULL,
                             n_inf_caregivers_isol = NULL) {

  # used with the inputs from the HWFE tool
  # need to add in HWFE tool inputs
  # ALSO based on weekly summary - from imperial data

  # Ambulance personnel ratio assumes 1 ambulance per 100 bed hospital with
  # 2 operators (paramedic + driver) at all times (3x8 hour shifts) so 6/100 beds
  if (is.null(ambulancews_per_bed)) {
    ambulancews_per_bed <- 0.06
  }
  # Biomedical engineer ratio assumes 2 biomedical engineers (on 8-hour shifts)
  # per 100 bed hospital
  if (is.null(bioengs_per_bed)) {
    bioengs_per_bed <- 0.02
  }
  # Reference assumption of zero is based on current guidance that no family
  # members or other caretakers should be in hospitals.
  if (is.null(n_inf_caregivers_hosp)) {
    n_inf_caregivers_hosp <- 0
  }
  # Reference is based on management of home care guidance, with estimates of
  # 1 caregiver per patient for the duration of the roughly 2-week isolation.
  # This calculation estimates the quantity of PPE required (e.g., masks and
  # gloves) for the patient and caregiver
  if (is.null(n_inf_caregivers_isol)) {
    n_inf_caregivers_isol <- 1
  }
  # mild and moderate in isolation, severe and critical in hospital
  hcw_allocations <- list(
    ambulancews_per_bed = ambulancews_per_bed,
    bioengs_per_bed = bioengs_per_bed,
    n_inf_caregivers_hosp = n_inf_caregivers_hosp,
    n_inf_caregivers_isol = n_inf_caregivers_isol
  )

  return(hcw_allocations)
}

#' Calculate case screened per HCW per day -----------------------------------------------------------------------
#'
#' @param cases_screened_per_hcw_per_day
#'
#' @return
#' @export
set_cases_screened_per_hcw_per_day <- function(cases_screened_per_hcw_per_day = NULL) {

# Screening/triage ratio is based on the assumption that each screening/triage
# takes approximately 48 minutes, which is 10 consultations per 8-hour shift
# ((8*60)/48).
  if(is.null(cases_screened_per_hcw_per_day)){
    cases_screened_per_hcw_per_day <- ((8*60)/48)
  }
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
  # WHO recommendation
  if(is.null(perc_crit_inv_mv)){
    perc_crit_inv_mv <- 2/3
  }
  if (is.null(perc_crit_noninv_mv)){
    perc_crit_noninv_mv<- 1/3
  }

  if(is.null(o2_flow_sev)){
    o2_flow_sev<-10
  }
  if(is.null(o2_flow_crit_inv_mv)){
    o2_flow_crit_inv_mv<-30
  }
  if(is.null(o2_flow_crit_noninv_mv)){
    o2_flow_crit_noninv_mv<-30
  }

  mv_params<- list(perc_crit_inv_mv = perc_crit_inv_mv,
                   perc_crit_noninv_mv = perc_crit_noninv_mv,

                   o2_flow_sev = o2_flow_sev,
                   o2_flow_crit_inv_mv = o2_flow_crit_inv_mv,
                   o2_flow_crit_noninv_mv = o2_flow_crit_noninv_mv)

  return(mv_params)
}


