# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the argument cutoff to several functions is
#' as expected
#'
#' @param cutoff Numeric; only those clusters with selection proportions equal
#' to at least cutoff will be selected by cluster stability selection. Must be
#' between 0 and 1.
#' @author Gregory Faletto, Jacob Bien
checkCutoff <- function(cutoff){
    stopifnot(is.numeric(cutoff) | is.integer(cutoff))
    stopifnot(length(cutoff) == 1)
    stopifnot(!is.na(cutoff))
    stopifnot(cutoff >= 0)
    stopifnot(cutoff <= 1)
}
