# Generated from _main.Rmd: do not edit by hand

#' Build an informative error for a subsample whose fitfun result is invalid
#'
#' Reached only when cssLoop failed on a subsample and parallel::mclapply
#' (forking; the serial path propagates the error directly) captured the failure
#' as a try-error. Surfaces the underlying cause in the error condition rather
#' than printing it to stdout.
#' @param result The offending element of getSelMatrix's res_list (a try-error
#' under forking; the class branch is a defensive fallback).
#' @param i Integer; the index of the subsample that failed.
#' @return A length-one character string suitable for stop().
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
fitfunFailureMessage <- function(result, i){
    if(inherits(result, "try-error")){
        paste0("The feature selection method (fitfun) failed on subsample ", i,
            ": ", conditionMessage(attr(result, "condition")))
    } else{
        paste0("The feature selection method (fitfun) returned an object of ",
            "class ", paste(class(result), collapse = "/"), " on subsample ", i,
            "; it must return an integer vector of selected feature indices.")
    }
}
