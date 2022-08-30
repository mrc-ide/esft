#' @title User input
#'
#' @description
#' Given that these are just three - don't know if these are useful.
#' Basically this is what is in the input/user dashboard tab that isn't captured
#' through other parameter-setting functions.
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below.
#'
#' @export
user_input <- function(overrides = list()) {
  input <- list(
    # case severity distribution
    forecast_period = 12,
    delivery_leadtime = 0,
    week1 = "2022-01-02"
  )
  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(input))) {
      stop(paste('unknown input parameter', name, sep=' '))
    }
    input[[name]] <- overrides[[name]]
  }

  input
}
