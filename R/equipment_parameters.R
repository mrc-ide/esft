#' @title Get equipment parameters
#'
#' for cost and existing amount of equipment
#'
#' @description
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#'
#' @export
equipment_parameters <- function(overrides = list()) {
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
