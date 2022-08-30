#' written by giovanni, taken from malariasimulation
approx_sum <- function(X, n) abs(sum(X) - n) < sqrt(.Machine$double.eps)
