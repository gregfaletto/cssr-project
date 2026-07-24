# Generated from _main.Rmd: do not edit by hand

#' @param type Character; either "clusters" (the default) to return the named
#' list of selected clusters, or "features" to return the flat integer vector of
#' selected features. May be abbreviated.
#' @param cutoff Numeric; only those clusters with selection proportions equal to
#' at least cutoff will be selected. Must be between 0 and 1. Default is 0 (in
#' which case either all clusters are selected, or max_num_clusts are selected,
#' if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be lowered until at least
#' min_num_clusts clusters are selected.) Default is 1. May be set to 0 to allow
#' a pure cutoff-based (threshold) selection that returns an empty result when no
#' cluster's selection proportion meets the cutoff.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be raised until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored). Because clusters can have tied selection
#' proportions, ties at the threshold can cause more than max_num_clusts (or
#' fewer than min_num_clusts) clusters to be returned; when the two constraints
#' conflict, max_num_clusts takes precedence.
#' @param weighting Character; passed to [getCssSelections()] to determine the
#' weights of individual features within the selected clusters. This affects
#' ONLY `type = "features"` (it determines which cluster members have nonzero
#' weight and are therefore returned); it is a no-op for `type = "clusters"`,
#' whose selected clusters, selection proportions, and sizes do not depend on the
#' weighting. Must be one of "sparse", "weighted_avg", or "simple_avg". See
#' [getCssSelections()] for details. Default is "sparse".
#' @rdname selected
#' @export
selected.cssr <- function(object, type=c("clusters", "features"), cutoff=0,
    min_num_clusts=1, max_num_clusts=NA, weighting="sparse", ...){
    stopifnot(inherits(object, "cssr"))
    type <- match.arg(type)

    # Thin wrapper: getCssSelections validates all inputs (checkCutoff,
    # checkWeighting, checkMinNumClusts, checkMaxNumClusts) and cleanly returns
    # empty results on an empty selection (e.g. cutoff = 1, min_num_clusts = 0).
    sel <- getCssSelections(object, weighting=weighting, cutoff=cutoff,
        min_num_clusts=min_num_clusts, max_num_clusts=max_num_clusts)

    if(type == "clusters"){
        return(sel$selected_clusts)
    }
    return(sel$selected_feats)
}
