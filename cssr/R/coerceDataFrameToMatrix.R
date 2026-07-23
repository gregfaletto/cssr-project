# Generated from _main.Rmd: do not edit by hand

#' Coerce a data.frame design matrix to a numeric matrix
#'
#' Shared helper for the several entry points that accept either a matrix or
#' a data.frame `X`: if `X` is a data.frame, convert it with
#' `stats::model.matrix(~ .)` and drop the intercept column. If the column
#' count changed and clusters were supplied (which would invalidate the
#' cluster indices), error. If `X` is already a numeric matrix it is returned
#' unchanged; a non-numeric matrix (e.g. a character matrix) is rejected with
#' an error.
#' @param X A numeric matrix or a data.frame.
#' @param clusters A list of integer vectors (the cluster assignments); the
#' column-count guard only fires when `length(clusters) > 0`.
#' @param arg_name Character; the name of the argument as it should appear in
#' the error message's first sentence. Default "X".
#' @param convert_phrase Character; the subject of the "Please convert ___"
#' sentence. Defaults to `arg_name`; cssSelect passes "the data.frame X" to
#' reproduce its existing message verbatim.
#' @return The (possibly coerced) numeric matrix.
#' @keywords internal
#' @noRd
coerceDataFrameToMatrix <- function(X, clusters, arg_name = "X",
    convert_phrase = arg_name){
    if(is.data.frame(X)){
        p <- ncol(X)
        X <- stats::model.matrix(~ ., X)
        # drop = FALSE: a single non-intercept column must stay a matrix.
        # Without it a 1-column data.frame collapses to a vector, ncol(X) is
        # NULL, and the guard below evaluates `... & logical(0)` -> the cryptic
        # "argument is of length zero" instead of letting the clean downstream
        # "p >= 2" check fire (#43).
        X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
        if(length(clusters) > 0 & (p != ncol(X))){
            stop(paste0("When stats::model.matrix converted the provided data.frame ",
                arg_name,
                " to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert ",
                convert_phrase,
                " to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix."))
        }
    }
    if(!is.numeric(X)){
        stop(paste0("The provided ", arg_name,
            " must be a numeric matrix; storage mode \"", storage.mode(X),
            "\" was provided. If ", arg_name,
            " came from a data.frame, check for character or factor columns."),
            call. = FALSE)
    }
    X
}
