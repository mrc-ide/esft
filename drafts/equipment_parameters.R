#' @title Equipment list and usage forecast - gives total cost and total amount
#' Maybe this is just the parameter setting and calling function ?
#' Could just be combining the various equipment data frames? dk if we need this function
#'
#' @description
#'
#' @param overrides A data frame with
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
