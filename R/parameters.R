#' @title Get baseline model parameters
#'
#' @description Gives baseline parameters from the inputs and user dashboard tab
#' in the WHO ESFT.
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below, and are taken from the default settings in
#' the ESFT.
#'
#' case severity distributions:
#'
#' * mild_i_proportion - proportion of cases that are mild, considered to be
#' isolating to reduce onward transmission; default = 0.4
#' * mod_i_proportion - proportion of cases that are moderate, considered to be
#' isolating to minimize onward transmission; default = 0.4
#' * sev_i_proportion - proportion of cases that are severe - depending on
#' severe bed availability, are admitted to hospital and require oxygen;
#' default = 0.15
#' * crit_i_proportion - proportion of cases that are critical - depending on
#' critical bed availability, admitted to hospital and require ventilation;
#' default = 0.05
#'
#' days of stay in isolation - WHO recommendation based onincubation period of
#' COVID-19 and case management guidelines:
#'
#' * stay_mild - average length of stay (in weeks) of mild cases in isolation;
#' default = 2
#' * stay_mod - average length of stay (in weeks) of moderate cases in
#' isolation; default = 2
#'
#' days of stay in hospital - based on studies of case severity:
#'
#' * stay_sev - average length of stay (in weeks) of severe cases in hospital;
#' default = 1
#' * stay_crit - average length of stay (in weeks) of critical cases in
#' hospital; default = 2
#'
#' case fatality rates
#'
#' * ifr_sev - infection fatality rate of severe cases, based on WHO China Joint
#' Mission Report; default = 0.134
#' * ifr_crit - infection fatality rate of critical cases, based on Imperial
#' College Report nr. 9; default = 0.5
#'
#' percentage allocation of health care workers (hcws):
#' (we set some percentages before hand, the rest are determined by the hcw_caps
#' function)
#' * perc_hcws_not_covid - assumption of percentage of HCWs not working on
#' COVID; default = 0.4
#' * n_hosp_beds_per_care_unit - assumption, used to estimate triple packaging
#' boxes (might not be necessary); default = 40
#'
#' Healthcare workers per bed or patient:
#'
#' * ambulancews_per_bed - ambulance personnel ratio assumes 1 ambulance per 100
#' bed hospital with 2 operators (paramedic + driver) at all times (3x 8 hour
#' shifts) so 6/100 beds; default = 0.06
#' * bioengs_per_bed - biomedical engineer ratio assumes 2 biomedical engineers
#' (on 8-hour shifts) per 100 bed hospital; default = 0.02
#' * n_inf_caregivers_hosp - reference assumption of zero is based on current
#' guidance that no family members or other caretakers should be in hospitals;
#' default = 0
#' * n_inf_caregivers_isol - reference is based on management of home care
#' guidance, with estimates of 1 caregiver per patient for the duration of the
#' roughly 2-week isolation - his calculation estimates the quantity of PPE
#' required (e.g., masks and gloves) for the patient and caregiver; default = 1
#'
#' percent of critical patients mechanically ventilated:
#'
#' * perc_crit_inv_mv - WHO recommendation; default = 0.667
#' * perc_crit_noninv_mv - WHO recommendation; default = 0.333
#'
#' o2 flow by severity of case, in LPM:
#'
#' * o2_flow_sev - severe patients require 5-15 LPM, so median taken;
#' default = 10
#' * o2_flow_crit_inv_mv - critical patients with invasive mechanical
#' ventilation; default = 30
#' * o2_flow_crit_noninv_mv - critical patients with non-invasive mechanical
#' ventilation; default = 30
#'
#' @return All of the misc parameters.
#'
#' @export
get_parameters <- function(overrides = list()) {
  parameters <- list(
    # case severity distribution
    mild_i_proportion = 0.4,
    mod_i_proportion = 0.4,
    sev_i_proportion = 0.15,
    crit_i_proportion = 0.05,
    # days of stay in isolation
    stay_mild = 2,
    stay_mod = 2,
    # days of stay in hospital
    stay_sev = 1,
    stay_crit = 2,
    # IFRs
    ifr_sev = 0.134,
    ifr_crit = 0.5,
    # percentage allocation of HCWs
    perc_hcws_not_covid = 0.4,
    n_hosp_beds_per_care_unit = 40,
    # HCWs per bed or patient
    ambulancews_per_bed = 0.06,
    bioengs_per_bed = 0.02,
    n_inf_caregivers_hosp = 0,
    n_inf_caregivers_isol = 1,
    # cases_screened_per_hcw_per_day = ((8 * 60) / 48),
    # cleaners_per_bed = 0.6,
    # percent of critical patients mechanically ventilated
    perc_crit_inv_mv = 2 / 3,
    perc_crit_noninv_mv = 1 / 3,
    # o2 flow for severity
    o2_flow_sev = 10,
    o2_flow_crit_inv_mv = 30,
    o2_flow_crit_noninv_mv = 30
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste("unknown parameter", name, sep = " "))
    }
    parameters[[name]] <- overrides[[name]]
  }

  props_infection <- c(
    parameters$mild_i_proportion,
    parameters$mod_i_proportion,
    parameters$sev_i_proportion,
    parameters$crit_i_proportion
  )

  if (!approx_sum(props_infection, 1)) {
    stop("Proportions of infection severity level do not sum to 1")
  }

  perc_mv <- c(
    parameters$perc_crit_inv_mv,
    parameters$perc_crit_noninv_mv
  )

  if (!approx_sum(perc_mv, 1)) {
    stop("Mechanical ventilation proportions do not sum to 1")
  }

  return(parameters)
}
