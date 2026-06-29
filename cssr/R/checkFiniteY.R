# Generated from _main.Rmd: do not edit by hand

#' Error if a response vector contains missing or non-finite values
#'
#' Shared guard for the y-accepting lasso entry points. Requires a numeric
#' (or integer) y and rejects any NA/NaN/Inf, so the lasso paths fail fast and
#' deterministically (getLassoLambda fits cv.glmnet on a RANDOM subsample, so a
#' non-finite y would otherwise error only when the bad value happened to be
#' drawn). Mirrors checkNoNAs()'s message for consistency.
#' @param y A numeric (real-valued) vector.
#' @param arg_name Character; the argument name as it should appear in the
#' error message. Default "y".
#' @return Invisibly, `y` (unchanged) -- called for its side effect.
#' @keywords internal
#' @noRd
checkFiniteY <- function(y, arg_name = "y"){
    if(!(is.numeric(y) | is.integer(y))){
        stop(paste0("The provided ", arg_name,
            " must be a numeric (real-valued) vector."), call. = FALSE)
    }
    if(any(!is.finite(y))){
        stop(paste0("The provided ", arg_name,
            " must not contain missing (NA) or non-finite (Inf) values; please ",
            "remove or impute them before calling this function."), call. = FALSE)
    }
    invisible(y)
}
