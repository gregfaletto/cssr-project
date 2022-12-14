# Generated from create-cssr.Rmd: do not edit by hand

#' Create design matrix of cluster representatives from matrix of raw features
#' using results of css function
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'. For "sparse", all the weight is put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", the weight used for each cluster member is calculated in
#' proportion to the individual selection proportions of each feature. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, the cluster representative
#' is just a simple average of all the cluster members). See Faletto and Bien
#' (2022) for details. Default is "weighted_avg".
#' @param cutoff Numeric; css will return only those clusters with selection
#' proportions equal to at least cutoff. Must be between 0 and 1. Default is 0
#' (in which case all clusters are returned in decreasing order of selection
#' proportion).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param newx A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newx are labeled, the names must match the variable names provided to css.
#' newx may be omitted if train_inds were provided to css to set aside
#' observations for model estimation. If this is the case, then when newx is
#' omitted formCssDesign will return a design matrix of cluster representatives
#' formed from the train_inds observations from the matrix X provided to css.
#' (If no train_inds were provided to css, newX must be provided to
#' formCssDesign.) Default is NA.
#' @return A design matrix with the same number of rows as newx (or the 
#' train_inds provided to css) where the columns are the constructed cluster
#' representatives.
#' @author Gregory Faletto, Jacob Bien
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
formCssDesign <- function(css_results, weighting="weighted_avg", cutoff=0,
    min_num_clusts=1, max_num_clusts=NA, newx=NA){

    # Check inputs
    ret <- checkFormCssDesignInputs(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts, newx)

    newx <- ret$newx
    max_num_clusts <- ret$max_num_clusts

    rm(ret)

    n <- nrow(newx)
    p <- ncol(newx)

    # Get the names of the selected clusters and the weights for the features
    # within each cluster, according to the provided weighting rule
    clust_sel_results <- getSelectedClusters(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts)

    selected_clusts <- clust_sel_results$selected_clusts
    weights <- clust_sel_results$weights

    rm(clust_sel_results)

    n_sel_clusts <- length(selected_clusts)

    # Form matrix of cluster representatives of selected clusters
    X_clus_reps <- matrix(rep(as.numeric(NA), n*n_sel_clusts), nrow=n,
        ncol=n_sel_clusts)
    colnames(X_clus_reps) <- rep(as.character(NA), n_sel_clusts)

    for(i in 1:n_sel_clusts){
        clust_i_name <- names(selected_clusts)[i]

        stopifnot(length(clust_i_name) == 1)
        stopifnot(clust_i_name %in% names(weights))

        colnames(X_clus_reps)[i] <- clust_i_name

        clust_i <- css_results$clusters[[clust_i_name]]

        stopifnot(length(clust_i) >= 1)
        stopifnot(all(clust_i) %in% 1:p)

        weights_i <- weights[[clust_i_name]]

        stopifnot(length(clust_i) == length(weights_i))

        if(length(weights_i) > 1){
            X_clus_reps[, i] <- newx[, clust_i] %*% weights_i
        } else{
            X_clus_reps[, i] <- newx[, clust_i]*weights_i
        }
    }

    # Check output
    stopifnot(all(!is.na(X_clus_reps)))
    stopifnot(ncol(X_clus_reps) == n_sel_clusts)
    stopifnot(nrow(X_clus_reps) == n)

    return(X_clus_reps)
}
