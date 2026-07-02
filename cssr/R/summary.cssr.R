# Generated from _main.Rmd: do not edit by hand

#' Summarize cluster stability selection output
#'
#' Produce a concise, computable overview of a fitted `cssr` object: a header
#' with the number of selected clusters and features (at the given cutoff), a
#' per-cluster table built on top of [printCssDf()], and a placeholder for
#' per-family error rate (PFER) error-control content.
#'
#' The returned object has class "summary.cssr" and an accompanying print method
#' ([print.summary.cssr()]). Its `table` element is exactly the data.frame
#' returned by [printCssDf()]: one row per selected cluster, sorted by selection
#' proportion, with columns ClustName, ClustProtoName (only if the features are
#' named), ClustProtoNum, ClustSelProp (the cluster's selection proportion) and
#' ClustSize.
#'
#' If the selection is empty (for example `cutoff = 1` with `min_num_clusts = 0`
#' when no cluster reaches the cutoff), `summary.cssr` returns a well-formed
#' zero-row object without error: `table` is NULL and the header reports
#' "0 clusters / 0 features selected". In this case [printCssDf()] and its helper
#' are deliberately NOT called, as they require at least one selected cluster.
#'
#' The `pfer` element is a documented placeholder (NA) reserved for error-control
#' content to be filled in by a future release (issue #87).
#'
#' @param object An object of class "cssr" (the output of the function [css()]).
#' @param cutoff Numeric; only those clusters with selection proportions equal to
#' at least cutoff are summarized. Must be between 0 and 1. Default is 0 (in
#' which case either all clusters are summarized, or max_num_clusts are, if
#' max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. Default is 1. May be set to 0 to allow a pure
#' cutoff-based (threshold) selection that can be empty.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. Default is NA (in which case max_num_clusts is
#' ignored).
#' @param weighting Character; passed to [getCssSelections()] to determine the
#' selected features and hence the selected-feature count in the header. As in
#' [selected()], it does NOT affect which clusters are selected or the
#' per-cluster table. Must be one of "sparse", "weighted_avg", or "simple_avg".
#' Default is "sparse".
#' @param ... Additional arguments (currently unused).
#' @return An object of class "summary.cssr": a named list with elements
#' \item{n_selected_clusts}{Integer; the number of selected clusters.}
#' \item{n_selected_feats}{Integer; the number of selected features.}
#' \item{cutoff}{Numeric; the cutoff that was used.}
#' \item{table}{A data.frame with one row per selected cluster (the output of
#' [printCssDf()]), or NULL when the selection is empty.}
#' \item{pfer}{A placeholder (NA) for per-family error rate error-control
#' content, to be filled in by a future release (issue #87).}
#' @author Gregory Faletto, Jacob Bien
#' @seealso [selected()] to extract just the selected clusters or features;
#' [getCssSelections()] for the underlying selection; [printCssDf()] for the
#' per-cluster data.frame; [print.cssr()] for the analogous printed summary.
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' clusters <- list(cluster1 = 1:4)
#' res <- css(X = data$X, y = data$y, lambda = 0.01, clusters = clusters,
#'   B = 10)
#' summary(res)
#' @export
summary.cssr <- function(object, cutoff=0, min_num_clusts=1, max_num_clusts=NA,
    weighting="sparse", ...){
    stopifnot(inherits(object, "cssr"))

    # Selected clusters + features. getCssSelections validates all inputs
    # (checkCutoff / checkWeighting / checkMinNumClusts / checkMaxNumClusts) and
    # is empty-safe (a clean empty result when e.g. cutoff = 1 and
    # min_num_clusts = 0).
    sel <- getCssSelections(object, weighting=weighting, cutoff=cutoff,
        min_num_clusts=min_num_clusts, max_num_clusts=max_num_clusts)

    n_selected_clusts <- length(sel$selected_clusts)
    n_selected_feats <- length(sel$selected_feats)

    # Empty-selection path: do NOT call printCssDf() / getSelectionPrototypes()
    # here. Both assert at least one selected cluster (printCssDf: nrow >= 1;
    # getSelectionPrototypes: n_selected_clusts >= 1) and so would die opaquely
    # on an empty selection (#107). Return a well-formed zero-row summary.
    if(n_selected_clusts >= 1){
        clust_table <- printCssDf(object, cutoff=cutoff,
            min_num_clusts=min_num_clusts, max_num_clusts=max_num_clusts)
    } else{
        clust_table <- NULL
    }

    out <- list(n_selected_clusts=n_selected_clusts,
        n_selected_feats=n_selected_feats, cutoff=cutoff,
        table=clust_table, pfer=NA_real_)

    class(out) <- "summary.cssr"

    return(out)
}
