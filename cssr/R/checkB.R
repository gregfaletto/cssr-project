# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to confirm that the argument B to several functions is as
#' expected
#'
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the total number of subsamples will be `B`; for
#' sampling_type="SS" the number of subsamples will be `2*B`.
#' @author Gregory Faletto, Jacob Bien
checkB <- function(B){
    stopifnot(length(B) == 1)
    stopifnot(is.numeric(B) | is.integer(B))
    stopifnot(!is.na(B))
    stopifnot(B == round(B))
    stopifnot(B > 0)
    if(B < 10){
        warning("Small values of B may lead to poor results.")
    } else if (B > 2000){
        warning("Large values of B may require long computation times.")
    }
}
