# Generated from _main.Rmd: do not edit by hand

#' Error if a design matrix contains missing (NA) values
#'
#' Shared guard for the X-accepting entry points. Checks the ORIGINAL input
#' (matrix or data.frame) so a data.frame's NAs are caught before
#' coerceDataFrameToMatrix()'s model.matrix() call silently drops NA rows
#' (na.action = na.omit). Pairs with coerceDataFrameToMatrix(): call this
#' first, then coerce.
#' @param X A numeric matrix or a data.frame.
#' @param arg_name Character; the argument name as it should appear in the
#' error message. Default "X".
#' @return Invisibly, `X` (unchanged) -- called for its side effect.
#' @keywords internal
#' @noRd
checkNoNAs <- function(X, arg_name = "X"){
    if(any(is.na(X))){
        stop(paste0("The provided ", arg_name,
            " must not contain missing (NA) values; please remove or impute them ",
            "before calling this function."), call. = FALSE)
    }
    invisible(X)
}
