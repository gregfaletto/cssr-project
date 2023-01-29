# Generated from _main.Rmd: do not edit by hand

#' Estimate prototypes from a list of clusters
#'
#' Takes in list of clusters, x, and y and returns an integer vector (of length
#' equal to the number of clusters) of the indices of the feature prototypes
#' (the features from each cluster most correlated with the response).
#'
#' @param clusters A list where each entry is an integer vector of indices of
#' features that are in a common cluster. (The length of list clusters must be
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. Must only include clusters of size 2 or larger.
#' @param x n x p numeric matrix; design matrix.
#' @param y Numeric response vector. Note: in general, the css function does not
#' require y to be a numeric vector, because the provided fitfun could use a
#' different form of y (for example, a categorical response variable). However,
#' y must be numeric in order to provide prototypes because the prototypes are
#' determined using the correlation between cluster members (columns of x) and
#' y.
#' @return An integer vector of the same length as clusters. Entry j is the
#' index of the feature identified as the prototype for cluster j.
#' @author Gregory Faletto, Jacob Bien
getPrototypes <- function(clusters, x, y){
    # Check inputs

    stopifnot(!is.list(clusters) | all(lengths(clusters) >= 1))
    stopifnot(is.list(clusters) | length(clusters) >= 1)

    stopifnot(all(!is.na(x)))
    stopifnot(is.matrix(x))

    n <- nrow(x)
    p <- ncol(x)

    checkY(y, n)

    # Identify prototypes
    if(length(clusters) > 0){
        if(is.list(clusters)){
            prototypes <- rep(as.integer(NA), length(clusters))
            for(i in 1:length(clusters)){
                prototypes[i] <- identifyPrototype(clusters[[i]], x, y)
            }
        } else{
            # If clusters is not a list, then there is only one cluster.
            prototypes <- identifyPrototype(clusters, x, y)
        }
    }  else{
        prototypes <- integer()
    }

    # Check output

    stopifnot(is.integer(prototypes))
    if(length(clusters) > 0){
        if(is.list(clusters)){
            stopifnot(length(prototypes) == length(clusters))
        } else {
            stopifnot(length(prototypes) == 1)
        }
    } else{
        stopifnot(length(prototypes) == 0)
    }

    stopifnot(all(!is.na(prototypes)))
    stopifnot(length(prototypes) == length(unique(prototypes)))

    return(prototypes)
}
