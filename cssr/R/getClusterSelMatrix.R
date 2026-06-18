# Generated from _main.Rmd: do not edit by hand

#' Get cluster selection matrix
#'
#' Given a matrix of feature selection indicator variables and a list of
#' clusters of features, returns a matrix of cluster indicator variables.
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of formatClusters.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @param res A binary integer matrix. `res[i, j]` = 1 if feature j was selected
#' on subsample i and equals 0 otherwise, as in the output of `getSelMatrix()`.
#' (That is, each row is a selected set.)
#' @return A binary integer matrix with the same number of rows
#' as res and length(clusters) columns. Entry i, j is 1 if at least
#' one member of cluster j was selected on subsample i, and 0 otherwise.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
getClusterSelMatrix <- function(clusters, res){

    # Check input
    checkGetClusterSelMatrixInput(clusters, res)

    p <- ncol(res)

    n_clusters <- length(clusters)

    # Map each feature to its (unique) cluster, then aggregate selections per
    # cluster in one rowsum pass instead of apply()-ing over all rows once per
    # cluster (#57). Each feature is in exactly one cluster and every cluster is
    # non-empty (enforced by checkGetClusterSelMatrixInput -> checkClusters
    # above), so feat2clus is fully populated and rowsum's groups are exactly
    # 1:n_clusters. rowsum's reorder=TRUE returns groups in numeric order, so the
    # rows -- and thus columns after t() -- line up positionally with
    # names(clusters).
    feat2clus <- integer(p)
    for(j in 1:n_clusters){
        feat2clus[clusters[[j]]] <- j
    }
    clust_counts <- rowsum(t(res), group = feat2clus)   # n_clusters x nrow(res)
    res_n_clusters <- t(clust_counts > 0L) * 1L
    colnames(res_n_clusters) <- names(clusters)

    # Check output
    stopifnot(is.matrix(res_n_clusters))
    stopifnot(identical(colnames(res_n_clusters), names(clusters)))
    stopifnot(all(res_n_clusters %in% c(0, 1)))
    stopifnot(ncol(res_n_clusters) == length(clusters))

    return(res_n_clusters)
}
