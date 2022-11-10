# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to check the output of getClusterProps
#'
#' @param feat_sel_props A numeric
#' vector of length p. The jth entry is the proportion of subsamples in which
#' feature j was selected by fitfun.
#' @param p Integer; the number of features in the X matrix provided to css.
#' @param clus_sel_props_p A numeric vector
#' of length p. The jth entry is the proportion of subsamples in which at least
#' one member of the cluster containing feature j was selected.
#' @param res_n_clusters A binary integer matrix with the same number of rows
#' as res and length(clusters) columns. res_n_clusters[i, j] = 1 if at least
#' one member of cluster j was selected on subsample i, and 0 otherwise.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of formatClusters.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @param sampling_type
#' @param B Integer; half of the subsamples for sampling_type "SS".
#' @author Gregory Faletto, Jacob Bien
checkGetClusterPropsOutput <- function(feat_sel_props, p, clus_sel_props_p,
    res_n_clusters, clusters, sampling_type, B){
    stopifnot(is.numeric(feat_sel_props))
    stopifnot(length(feat_sel_props) == p)
    stopifnot(all(feat_sel_props >= 0))
    stopifnot(all(feat_sel_props <= 1))

    stopifnot(is.numeric(clus_sel_props_p))
    stopifnot(length(clus_sel_props_p) == p)
    stopifnot(all(clus_sel_props_p >= 0))
    stopifnot(all(clus_sel_props_p <= 1))

    stopifnot(is.matrix(res_n_clusters))
    stopifnot(identical(colnames(res_n_clusters), names(clusters)))
    stopifnot(all(res_n_clusters %in% c(0, 1)))
    stopifnot(ncol(res_n_clusters) == length(clusters))
    if(sampling_type=="SS"){
        stopifnot(B == nrow(res_n_clusters)/2)
    } else{
        stopifnot(B == nrow(res_n_clusters))
    }
}
