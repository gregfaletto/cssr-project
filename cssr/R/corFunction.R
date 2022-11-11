# Generated from create-cssr.Rmd: do not edit by hand

#' Absolute value of sample correlation between two vectors
#'
#' Calculates the absolute value of correlation of t and y. If either input has
#' only one unique value, returns 0 by definition.
#' @param t A numeric or integer vector.
#' @param y A numeric or integer vector; must have the same length as t.
#' @return A numeric vector of the same length as cluster_i containing the
#' weights corresponding to each of the features in cluster_i. The weights
#' will all be nonnegative and sum to 1.
#' @author Gregory Faletto, Jacob Bien
corFunction <- function(t, y){
    # Check inputs
    stopifnot(is.numeric(t) | is.integer(t))
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(length(t) == length(y))
    if(length(unique(t)) == 1){
        return(0)
    }
    if(length(unique(y)) == 1){
        warning("The second argument to corFunction only had one unique entry")
        return(0)
    }
    return(abs(stats::cor(t, y)))
}
