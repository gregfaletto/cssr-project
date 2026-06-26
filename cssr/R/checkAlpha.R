# Generated from _main.Rmd: do not edit by hand

#' Confirm that the elastic-net mixing parameter alpha is as expected
#'
#' Shared validator for the alpha argument of getLassoLambda, getModelSize,
#' cssSelect, and cssPredict: a single number in (0, 1] (alpha = 1 is the
#' lasso). Factored out of those four functions, which each held a verbatim
#' copy, so they cannot drift (#72).
#' @param alpha Numeric or integer; the elastic-net mixing parameter. Must be
#' length 1, non-NA, and in (0, 1].
#' @return No return value; called for the side effect of erroring on bad
#' input.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkAlpha <- function(alpha){
    stopifnot(is.numeric(alpha) | is.integer(alpha))
    stopifnot(length(alpha) == 1)
    stopifnot(!is.na(alpha))
    stopifnot(alpha > 0)
    stopifnot(alpha <= 1)
}
