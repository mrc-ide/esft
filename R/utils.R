#' written by giovanni, taken from malariasimulation
approx_sum <- function(x, n) abs(sum(x) - n) < sqrt(.Machine$double.eps)
