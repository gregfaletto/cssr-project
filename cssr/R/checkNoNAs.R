# Generated from _main.Rmd: do not edit by hand

#' Error if a design matrix contains missing or non-finite values
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
    has_na <- any(is.na(X))
    has_inf <- if(is.data.frame(X)){
        any(vapply(X, function(col) is.numeric(col) && any(is.infinite(col)),
            logical(1)))
    } else if(is.numeric(X)){
        any(is.infinite(X))
    } else {
        FALSE
    }
    if(has_na || has_inf){
        stop(paste0("The provided ", arg_name,
            " must not contain missing (NA) or non-finite (Inf) values; please ",
            "remove or impute them before calling this function."), call. = FALSE)
    }
    invisible(X)
}
