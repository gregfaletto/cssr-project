# Generated from create-cssr.Rmd: do not edit by hand

#' Find cluster proportions
#'
#' Given a matrix of feature selection indicator variables and a list of
#' clusters of features, returns a matrix of cluster indicator variables, as
#' well as the selection proportions for both features and clusters.
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
#' @return A named list with four elements. \item{feat_sel_props}{A numeric
#' vector of length p. The jth entry is the proportion of subsamples in which
#' feature j was selected by fitfun.} \item{clus_sel_props_p}{A numeric vector
#' of length p. The jth entry is the proportion of subsamples in which at least
#' one member of the cluster containing feature j was selected.}
#' \item{res_clus_p}{An integer matrix of the same dimensions as res.
#' res_clus_p[i, j] = 1 if at least one member of the cluster containing feature
#' j was selected by fitfun on subsample i, and 0 otherwise.}
#' \item{res_n_clusters}{A binary integer matrix with the same number of rows
#' as res and length(clusters) columns. res_n_clusters[i, j] = 1 if at least
#' one member of cluster j was selected on subsample i, and 0 otherwise.}
#' @author Gregory Faletto, Jacob Bien
getClusterProps <- function(clusters, res, sampling_type){

    # TODO(gfaletto): consider deprecating feat_sel_props and res_clus_p
    # outputs, which don't seem to be used anywhere? (res_clus_p still needs
    # to be calculated since it is used to calculate clus_sel_props_p, but it
    # doesn't necessarily need to be returned. feat_sel_props doesn't seem to
    # be needed to be calculated or returned here at all.)

    # Check input

    B <- checkGetClusterPropsInput(clusters, res, sampling_type)

    p <- ncol(res)

    n_clusters <- length(clusters)

    # Calculate feat_sel_props
    feat_sel_props <- colMeans(res)

    # Matrix of cluster selection proportions (with p columns)
    res_clus_p <- res

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

        # Set all cluster members in these rows equal to 1 in res_clus_p
        res_clus_p[rows_j_sel, clusters[[j]]] <- 1

        # Confirm that all columns in res_clus_p corresponding to
        # clusters[[j]] have the same values
        stopifnot(all(apply(res_clus_p[, clusters[[j]]], 1,
            function(x){length(unique(x)) == 1})))
    }

    # Check res_clus_p
    stopifnot(is.matrix(res_clus_p))
    stopifnot(all.equal(dim(res), dim(res_clus_p)))
    stopifnot(all(res_clus_p %in% c(0, 1)))

    # For every cluster, confirm that all columns in res_clus_p corresponding
    # to that cluster are identical (that is, every row in the submatrix
    # res_clus_p[, clusters[[j]]] has only one unique value)
    for(j in 1:n_clusters){
        if(length(clusters[[j]]) > 1){
            stopifnot(all(apply(res_clus_p[, clusters[[j]]], 1,
                function(x){length(unique(x)) == 1})))
        }
    }

    # Calculate clus_sel_props_p
    clus_sel_props_p <- colMeans(res_clus_p)

    # Check output
    checkGetClusterPropsOutput(feat_sel_props, p, clus_sel_props_p,
        res_n_clusters, clusters, sampling_type, B)

    return(list(feat_sel_props=feat_sel_props,
        clus_sel_props_p=clus_sel_props_p, res_clus_p=res_clus_p,
        res_n_clusters=res_n_clusters))
}
