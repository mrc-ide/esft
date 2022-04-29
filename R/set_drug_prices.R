#' @title Sets pharmaceutical prices
#'
#' @description
#' set_drug_prices takes a dataframe of drug names, concentrations, and prices,
#' and returns a parameter dataframe with updated prices.
#'
#' @param overrides A data frame with three rows: drug, concentration, and
#' price_usd. Drug name and concentration must match those found in the
#' pharmaceuticals data frame in order to effectively change the price.
#'
#' @export
set_drug_prices <- function(overrides = data.frame()) {
  parameters <- as.data.frame(esft::pharmaceuticals)
  # Override parameters with any client specified ones
  if (!is.data.frame(overrides)) {
    stop('overrides must be a data.frame')
  }

  for (i in 1:nrow(overrides)) {
    if (!(overrides$drug[i] %in% parameters$drug)) {
      stop(paste('unknown drug', name, sep=' '))
    } else if (!(overrides$concentration[i] %in% parameters$concentration)) {
      stop(paste('unknown concentration', name, sep=' '))
    }

    parameters$price_usd[i]<-overrides$price_usd[i]
  }

  parameters
}
