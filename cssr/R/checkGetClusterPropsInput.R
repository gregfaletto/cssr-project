# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to check inputs to getClusterProps function
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of formatClusters.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @param res A binary integer matrix of dimension `B` x `p` (if sampling_type
#' == "MB" was provided to ) or `2*B` x `p` (if sampling_type == "SS").
#' res[i, j] = 1 if feature j was selected on subsample i and equals 0
#' otherwise, as in the output of getSelMatrix. (That is, each row is a selected
#' set.)
#' @param sampling_type A character vector (either "SS" or "MB"); the sampling
#' type used for stability selection.
#' @return The parameter B, corresponding to half of the subsamples for 
#' sampling_type "SS".
#' @author Gregory Faletto, Jacob Bien
checkGetClusterPropsInput <- function(clusters, res, sampling_type){
    stopifnot(is.matrix(res))
    stopifnot(all(res %in% c(0, 1)))
    p <- ncol(res)
    if(sampling_type=="SS"){
        B <- nrow(res)/2
        stopifnot(B == round(B))
    } else{
        B <- nrow(res)
    }
    stopifnot(B > 0)

    checkClusters(clusters, p)

    return(as.integer(B))
}
