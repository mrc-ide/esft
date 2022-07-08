#' @title Get extra model parameters
#' @description
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' @return All of the misc parameters.
#'
#' @export
get_parameters <- function(overrides = list()) {
  parameters <- list(
    # case severity distribution
    mildI_proportion = 0.4,
    modI_proportion = 0.4,
    sevI_proportion = 0.15,
    critI_proportion = 0.05,
    # days of stay in isolation
    stay_mild = 2,
    stay_mod = 2,
    # days of stay in hospital
    stay_sev = 1,
    stay_crit = 2,
    # IFRs
    IFR_sev = 0.134,
    IFR_crit = 0.5,
    # percentage allocation of HCWs
    perc_hcws_not_covid = 0.4,
    perc_hcws_treat_covid = 0.53,
    perc_hcws_screen_covid = 0.07,
    n_hosp_beds_per_care_unit = 40,
    # HCWs per bed or patient
    ambulancews_per_bed = 0.06,
    bioengs_per_bed = 0.02,
    n_inf_caregivers_hosp = 0,
    n_inf_caregivers_isol = 1,

    cases_screened_per_hcw_per_day = ((8*60)/48),
    # percent of critical patients mechanically ventilated
    perc_crit_inv_mv = 2/3,
    perc_crit_noninv_mv = 1/3,
    # o2 flow for severity
    o2_flow_sev = 10,
    o2_flow_crit_inv_mv = 30,
    o2_flow_crit_noninv_mv = 30
    )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  props_I <- c(
    parameters$mildI_proportion,
    parameters$modI_proportion,
    parameters$sevI_proportion,
    parameters$critI_proportion
  )

  if (!approx_sum(props_I, 1)) {
    stop("Proportions of infection severity level do not sum to 1")
  }

  perc_hcw <- c(
    parameters$perc_hcws_not_covid,
    parameters$perc_hcws_treat_covid,
    parameters$perc_hcws_screen_covid
  )

  if (!approx_sum(perc_hcw, 1)) {
    stop("HCW allocation percentages do not sum to 1")
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
