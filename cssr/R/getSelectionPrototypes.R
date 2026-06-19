# Generated from _main.Rmd: do not edit by hand

#' Identify selection prototypes from selected clusters
#'
#' Takes in list of selected clusters and returns an integer vector of the
#' indices of the features that were most frequently selected from each cluster
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param selected_clusts A list of integer vectors; each vector must contain
#' the indices of features in a cluster.
#' @return An integer vector (of length equal to the number of clusters) of the
#' indices of the feature prototypes (the features from each cluster that were
#' selected the most often individually by the base method in cluster stability
#' selection). In the case of a tie, the tie is broken by choosing the feature
#' most correlated with the response in the full data set provided to css.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
getSelectionPrototypes <- function(css_results, selected_clusts){
    
    # Check inputs
    stopifnot(inherits(css_results, "cssr"))

    stopifnot(is.list(selected_clusts))
    n_selected_clusts <- length(selected_clusts)
    stopifnot(n_selected_clusts >= 1)
    stopifnot(all(lengths(selected_clusts) >= 1))

    prototypes <- rep(as.integer(NA), n_selected_clusts)
    feat_sel_props <- colMeans(css_results$feat_sel_mat)
    for(i in 1:n_selected_clusts){
        clust_i <- selected_clusts[[i]]
        sel_props_i <- feat_sel_props[clust_i]
        proto_i <- clust_i[sel_props_i == max(sel_props_i)]
        stopifnot(length(proto_i) >= 1)
        if(length(proto_i) > 1){
            if(is.numeric(css_results$y) | is.integer(css_results$y)){
                # Break tie by marginal correlation. suppressWarnings + NA->0
                # mirrors corFunction / identifyPrototype (#59): a constant
                # column has undefined correlation (cor() returns NA with a
                # "standard deviation is zero" warning) -- treat it as 0 so it
                # never wins the tie-break and never poisons max() (#68).
                corrs_i <- suppressWarnings(abs(stats::cor(
                    css_results$X[, proto_i, drop = FALSE], css_results$y)[, 1]))
                corrs_i[is.na(corrs_i)] <- 0
                proto_i <- proto_i[corrs_i == max(corrs_i)]
            }
        }
        # If there is still a tie, break by choosing the first feature of those
        # remaining
        prototypes[i] <- proto_i[1]
        names(prototypes)[i] <- colnames(css_results$X)[proto_i]
    }

    # Check output

    stopifnot(is.integer(prototypes))
    stopifnot(all(!is.na(prototypes)))
    stopifnot(length(prototypes) == length(unique(prototypes)))

    return(prototypes)
}
