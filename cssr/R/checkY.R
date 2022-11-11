# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to confirm that the argument y to several functions is
#' as expected
#'
#' @param y Numeric response vector.
#' @param n Number of observations of covariates; should match length of y.
#' @author Gregory Faletto, Jacob Bien
checkY <- function(y, n){
    stopifnot(all(!is.na(y)))
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(length(unique(y)) > 1)
    stopifnot(length(n) == 1)
    stopifnot(!is.na(n))
    stopifnot(is.numeric(n) | is.integer(n))
    stopifnot(n == round(n))
    stopifnot(n > 0)
    stopifnot(n == length(y))
}
