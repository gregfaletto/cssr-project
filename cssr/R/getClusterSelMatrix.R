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
#' @param res A binary integer matrix. es[i, j] = 1 if feature j was selected
#' on subsample i and equals 0 otherwise, as in the output of `getSelMatrix()`.
#' (That is, each row is a selected set.)
#' @return A binary integer matrix with the same number of rows
#' as res and length(clusters) columns. Entry i, j is 1 if at least
#' one member of cluster j was selected on subsample i, and 0 otherwise.
#' @author Gregory Faletto, Jacob Bien
getClusterSelMatrix <- function(clusters, res){

    # Check input
    checkGetClusterSelMatrixInput(clusters, res)

    p <- ncol(res)

    n_clusters <- length(clusters)

    # Matrix of cluster selection proportions (with n_clusters columns)
    res_n_clusters <- matrix(rep(0L, nrow(res)*n_clusters), nrow=nrow(res),
        ncol=n_clusters)
    colnames(res_n_clusters) <- names(clusters)

    for(j in 1:n_clusters){
        # Identify rows of res where at least one feature from this cluster
        # was selected
        rows_j_sel <- apply(res, 1, function(x){any(x[clusters[[j]]] == 1)})

        # Put ones in these rows of res_n_clusters[, j]
        res_n_clusters[rows_j_sel, j] <- 1L

        if(length(clusters[[j]]) <= 1){
            next
        }
    }

    # Check output
    stopifnot(is.matrix(res_n_clusters))
    stopifnot(identical(colnames(res_n_clusters), names(clusters)))
    stopifnot(all(res_n_clusters %in% c(0, 1)))
    stopifnot(ncol(res_n_clusters) == length(clusters))

    return(res_n_clusters)
}
