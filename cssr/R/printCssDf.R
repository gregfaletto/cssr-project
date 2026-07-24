# Generated from _main.Rmd: do not edit by hand

#' Prepares a data.frame summarazing cluster stability selection output to print
#'
#' Print a summary of the information from the css function.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param cutoff Numeric; the outputted data.frame will display only those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1. Default is 0 (in which case either all clusters are displayed, or
#' max_num_clusts are, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be lowered until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be raised until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored). Because clusters can have tied selection
#' proportions, ties at the threshold can cause more than max_num_clusts (or
#' fewer than min_num_clusts) clusters to be returned; when the two constraints
#' conflict, max_num_clusts takes precedence.
#' @return A data.frame; each row contains a cluster, arranged in decreasing
#' order of cluster selection proportion from top to bottom. The columns are
#' ClustName (the name of the cluster that was either provided to css or made by
#' css if no name was provided); ClustProtoName (the name of the selection
#' prototype from the cluster, which is the feature with the greatest individual
#' selection proportion among all the cluster members, with ties broken by
#' choosing the feature with the highest correlation with the response if the
#' response is real-valued; only returned if the features are named),
#' ClustProtoNum (the column number of the prototype in the X matrix provided to
#' css), ClustSelProp (the cluster's selection proportion), and ClustSize (the
#' size of the cluster).
#' @author Gregory Faletto, Jacob Bien
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' clusters <- list(cluster1 = 1:4)
#' res <- css(X = data$X, y = data$y, lambda = 0.01, clusters = clusters,
#'   B = 10)
#' printCssDf(res)
#' @export
printCssDf <- function(css_results, cutoff=0, min_num_clusts=1,
    max_num_clusts=NA){
    # Check inputs
    stopifnot(inherits(css_results, "cssr"))
    checkCutoff(cutoff)

    p <- ncol(css_results$feat_sel_mat)

    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    sel_clusts <- getCssSelections(css_results, cutoff=cutoff,
        min_num_clusts=min_num_clusts,
        max_num_clusts=max_num_clusts)$selected_clusts

    # printCssDf keeps its exact exported signature; the table-building work
    # (empty-selection handling, prototypes, selection proportions, ordering)
    # now lives in the internal buildCssDf() helper so summary.cssr() can reuse
    # an already-computed sel_clusts instead of making a second identical
    # getCssSelections() call. Byte-identical: buildCssDf() consumes sel_clusts
    # exactly as this body did. (#129)
    return(buildCssDf(css_results, sel_clusts))
}
