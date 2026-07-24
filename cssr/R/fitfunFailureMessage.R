# Generated from _main.Rmd: do not edit by hand

#' Build an informative error for a subsample whose fitfun result is invalid
#'
#' Reached when cssLoop failed on a subsample. getSelMatrix wraps each
#' subsample's cssLoop call in a tryCatch that tags the failure with its true
#' subsample index as a cssr_loop_error (on both the serial and parallel paths),
#' so the message names the correct subsample rather than a
#' prescheduling-misattributed one. Surfaces the underlying cause in the error
#' condition rather than printing it to stdout.
#' @param result The offending element of getSelMatrix's res_list (normally a
#' cssr_loop_error carrying the tagged subsample index and message; the
#' try-error and class branches are defensive fallbacks).
#' @param i Integer; the loop index of the subsample that failed (used only by
#' the fallback branches -- a cssr_loop_error carries its own tagged index).
#' @return A length-one character string suitable for stop().
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
fitfunFailureMessage <- function(result, i){
    if(inherits(result, "cssr_loop_error")){
        return(paste0("The feature selection method (fitfun) failed on subsample ",
            result$i, ": ", result$msg))
    }
    if(inherits(result, "try-error")){
        paste0("The feature selection method (fitfun) failed on subsample ", i,
            ": ", conditionMessage(attr(result, "condition")))
    } else{
        paste0("The feature selection method (fitfun) returned an object of ",
            "class ", paste(class(result), collapse = "/"), " on subsample ", i,
            "; it must return an integer vector of selected feature indices.")
    }
}
