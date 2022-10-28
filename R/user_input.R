#' @title User input
#'
#' @description Given that these are just three - don't know if these are
#' useful. Basically this is what is in the input/user dashboard tab that isn't
#' captured through other parameter-setting functions.
#' Plus, there are no checks of type in this function, nor is it entirely clear
#' where this leads into. So there's a potential to get rid of this one or
#' consolidate it with another.
#'
#' @param overrides a named list of parameter values to use instead of defaults
#' The parameters are defined below, and are taken from the default values of
#' the ESFT.
#'
#' misc user input parameters:
#'
#' * forecast_period - weeks from start date that you want to forecast from;
#' default = 12
#' * delivery_leadtime - time in weeks until shipment received (between 0-1).
#' note - if this is set to 1, this pushed the forecast period out 1 week to
#' factor in delivery time; default = 0
#' * week1 - date from which the forecast starts (year-month-date);
#' default = "2022-01-02"
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
