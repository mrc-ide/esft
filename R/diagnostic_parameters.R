#' @title Get diagnostic parameters
#'
#' pull in dataframe
#' also add in the inputs and user dashboard
#' plus will include time running, estimated number of tests, etc.
#'
#' @description
#' get_parameters creates a named list of parameters for use in the model. These
#' parameters are passed to process functions. These parameters are explained in
#' "The US President's Malaria Initiative, Plasmodium falciparum transmission
#' and mortality: A modelling study."
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' fixed state transitions:
#'
#' * dd - the delay for humans to move from state D to A; default = 5
#' * dt - the delay for humans to move from state Tr to Ph; default = 5
#' * da - the delay for humans to move from state A to U; default = 200
#'
#' #' treatment parameters:
#' please set treatment parameters with the convenience functions in
#' `drug_parameters.R`
#'
#' @export
diagnostic_parameters <- function(overrides = list()) {
  parameters <- list(
    # case severity distribution
    mildI_proportion = 0.4,
    modI_proportion = 0.4,
    sevI_proportion = 0.15,
    critI_proportion = 0.05)

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

  parameters
}
