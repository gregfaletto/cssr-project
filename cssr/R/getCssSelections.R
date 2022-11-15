# Generated from create-cssr.Rmd: do not edit by hand

#' Obtain a selected set of clusters and features
#'
#' Generate sets of selected clusters and features from cluster stability
#' selection.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details. Default is "sparse".
#' @param cutoff Numeric; getCssSelections will select and return only of those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1. Default is 0 (in which case either all clusters are selected, or
#' max_num_clusts are selected, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @return A named list with two items. \item{selected_clusts}{A list of
#' integer vectors; each vector contains the indices of the features in one of
#' the selected clusters.} \item{selected_feats}{A named integer vector; the
#' indices of the features with nonzero weights from all of the selected
#' clusters.}
#' @author Gregory Faletto, Jacob Bien
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
#' @export
getCssSelections <- function(css_results, weighting="sparse", cutoff=0,
    min_num_clusts=1, max_num_clusts=NA){
    # Check inputs
    stopifnot(class(css_results) == "cssr")
    checkCutoff(cutoff)
    checkWeighting(weighting)

    p <- ncol(css_results$feat_sel_mat)

    checkMinNumClusts(min_num_clusts, p)

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        css_results$clusters)

    sel_results <- getSelectedClusters(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts)

    sel_clust_names <- names(sel_results$selected_clusts)

    stopifnot(all(sel_clust_names %in% names(css_results$clusters)))

    sel_clusts <- list()
    for(i in 1:length(sel_clust_names)){
        sel_clusts[[i]] <- css_results$clusters[[sel_clust_names[i]]]
        names(sel_clusts)[i] <- sel_clust_names[i]
    }

    return(list(selected_clusts=sel_clusts,
        selected_feats=sel_results$selected_feats))
}
