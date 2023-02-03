# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that inputs to several functions are as expected,
#' and modify inputs if needed
#'
#' @param newx A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newX are labeled, the names must match the variable names provided to css.
#' @param css_X The X matrix provided to css, as in the output of the css
#' function (after having been coerced from a data.frame to a matrix by css if
#' needed).
#' @return A named list with the following elements. \item{feat_names}{A 
#' character vector containing the column names of newx (if the provided newx
#' had column names). If the provided newx did not have column names, feat_names
#' will be NA.} \item{newx}{The provided newx matrix, coerced from a data.frame
#' to a matrix if the provided newx was a data.frame.}
#' @author Gregory Faletto, Jacob Bien
checkXInputResults <- function(newx, css_X){

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(newx)){
        newx <- stats::model.matrix(~ ., newx)
        newx <- newx[, colnames(newx) != "(Intercept)"]
    }

    feat_names <- as.character(NA)
    if(!is.null(colnames(newx))){
        feat_names <- colnames(newx)
        stopifnot(identical(feat_names, colnames(css_X)))
    } else{
        # In this case, newx has no column names, so same better be true of
        # css_X
        if(!is.null(colnames(css_X))){
            warning("New X provided had no variable names (column names) even though the X provided to css did.")
        }
    }

    stopifnot(is.matrix(newx))
    stopifnot(all(!is.na(newx)))

    n <- nrow(newx)
    p <- ncol(newx)
    stopifnot(p >= 2)
    if(length(feat_names) > 1){
        stopifnot(length(feat_names) == p)
        stopifnot(!("(Intercept)" %in% feat_names))
    } else{
        stopifnot(is.na(feat_names))
    }

    colnames(newx) <- character()

    # Confirm that newx matches css_results$X
    if(p != ncol(css_X)){
        err <- paste("Number of columns in newx must match number of columns from matrix provided to css. Number of columns in new provided X: ",
            p, ". Number of columns in matrix provided to css: ", ncol(css_X),
            ".", sep="")
        stop(err)
    }
    if(length(feat_names) != 1 & all(!is.na(feat_names))){
        if(!identical(feat_names, colnames(css_X))){
            stop("Provided feature names for newx do not match feature names provided to css")
        }
    }

    return(list(feat_names=feat_names, newx=newx))
}
