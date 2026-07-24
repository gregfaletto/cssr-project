# Generated from _main.Rmd: do not edit by hand

#' Plot cluster (or feature) selection proportions from cluster stability
#' selection
#'
#' Produce a base-graphics bar plot of the selection proportions of a fitted
#' `cssr` object: one bar per cluster (the default) or per feature, sorted in
#' decreasing order of selection proportion. When `cutoff > 0` a dashed
#' reference line is drawn at the cutoff (for `type = "clusters"`), and the bars
#' whose cluster (or feature) is selected -- at the given `cutoff` and
#' cluster-count constraints -- are highlighted in `sel_col`, the rest in
#' `unsel_col`.
#'
#' The selection that determines the highlight is obtained from
#' [getCssSelections()] with the same `cutoff`, `min_num_clusts`,
#' `max_num_clusts`, and `weighting`. Which clusters are selected does not
#' depend on `weighting`; which features are selected does (the weighting
#' determines which members of the selected clusters have nonzero weight and are
#' therefore highlighted when `type = "features"`).
#'
#' Note that, to match the positional argument order of [print.cssr()], the
#' second positional argument is `cutoff`; `type`, `weighting`, and the
#' graphical parameters must be supplied by name (for example
#' `plot(x, type = "features")`).
#'
#' @param x An object of class "cssr" (the output of the function [css()]).
#' @param cutoff Numeric; the selection-proportion threshold used both to draw
#' the dashed reference line (when greater than 0) and to determine which bars
#' are highlighted as selected. The dashed reference line is drawn only for
#' `type = "clusters"`; for `type = "features"` no line is drawn, because the
#' bars show feature proportions while highlighting follows the cluster-level
#' selection. Must be between 0 and 1. Default is 0 (in which case no reference
#' line is drawn and, unless `max_num_clusts` restricts it, every cluster is
#' treated as selected).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' treat as selected regardless of cutoff. (If the chosen cutoff would select
#' fewer than min_num_clusts clusters, the cutoff is effectively lowered until
#' at least min_num_clusts clusters are selected.) Default is 1. May be set to 0
#' to allow a pure cutoff-based highlight that can be empty (no bar highlighted)
#' when no cluster's selection proportion meets the cutoff.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' treat as selected regardless of cutoff. (If the chosen cutoff would select
#' more than max_num_clusts clusters, the cutoff is effectively raised until at
#' most max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param type Character; either "clusters" (the default) to plot one bar per
#' cluster, or "features" to plot one bar per feature. May be abbreviated.
#' @param weighting Character; passed to [getCssSelections()] to determine which
#' individual features are selected (and therefore highlighted) when
#' `type = "features"`. It has no effect on the cluster bars or on which
#' clusters are highlighted. Must be one of "sparse", "weighted_avg", or
#' "simple_avg". Default is "sparse" (matching [getCssSelections()] and
#' [selected()]).
#' @param ylim Numeric vector of length 2; the y-axis limits for the bar plot.
#' Default is `c(0, 1)`, the natural range of a selection proportion.
#' @param sel_col The colour used to fill the bars of selected clusters (or
#' features). Default is "steelblue".
#' @param unsel_col The colour used to fill the bars of unselected clusters (or
#' features). Default is "grey70".
#' @param ... Additional graphical parameters passed on to
#' [graphics::barplot()] (for example `main` or `cex.names`). The bar heights,
#' fill colours, and `ylim` are controlled by this method; supplying `col` here,
#' or the `ylim` argument, overrides the highlight colours or the default y-axis
#' limits.
#' @return Invisibly, the numeric vector of selection proportions that were
#' plotted: the `colMeans()` of the cluster (or feature) selection matrix,
#' sorted in decreasing order, named by cluster (always) or by feature (when the
#' features of `X` were named; unnamed otherwise). Called primarily for its side
#' effect: drawing the bar plot.
#' @author Gregory Faletto, Jacob Bien
#' @seealso [print.cssr()] and [summary.cssr()] for printed / tabular overviews
#' of the same selection proportions; [selected()] to extract the selected
#' clusters or features; [getCssSelections()] for the underlying selection used
#' to highlight the bars.
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' # Name the features so the feature plot shows labelled, highlighted bars
#' # (genClusteredData returns X with no column names).
#' X <- data$X
#' colnames(X) <- paste0("V", seq_len(ncol(X)))
#' clusters <- list(cluster1 = 1:4)
#' res <- css(X = X, y = data$y, lambda = 0.01, clusters = clusters, B = 10)
#' # Cluster selection proportions (the default):
#' plot(res)
#' # Feature selection proportions, highlighting the selected features:
#' plot(res, type = "features")
#' @export
plot.cssr <- function(x, cutoff = 0, min_num_clusts = 1, max_num_clusts = NA,
    type = c("clusters", "features"), weighting = "sparse", ylim = c(0, 1),
    sel_col = "steelblue", unsel_col = "grey70", ...){
    type <- match.arg(type)
    sel <- suppressWarnings(getCssSelections(x, weighting = weighting,
        cutoff = cutoff, min_num_clusts = min_num_clusts,
        max_num_clusts = max_num_clusts))
    if(type == "clusters"){
        props  <- colMeans(x$clus_sel_mat)                              # named by cluster (guaranteed)
        is_sel <- names(props) %in% names(sel$selected_clusts)
        ylab   <- "Cluster selection proportion"
    } else {
        props  <- colMeans(x$feat_sel_mat)                             # named iff X was named
        is_sel <- seq_along(props) %in% as.integer(sel$selected_feats) # INDEX-based -> robust to unnamed X
        ylab   <- "Feature selection proportion"
    }
    ord    <- order(props, decreasing = TRUE)
    props  <- props[ord]; is_sel <- is_sel[ord]
    labels <- names(props); if(is.null(labels)) labels <- as.character(ord)  # index labels if X unnamed
    # Method defaults go on the DEFAULTS side of the merge so a user-supplied col
    # or ylim in `...` wins (matching the @param ... doc); height is forced AFTER
    # the merge because the bar heights ARE the selection proportions (#160a).
    args <- utils::modifyList(
        list(names.arg = labels, las = 2, ylab = ylab,
            col = ifelse(is_sel, sel_col, unsel_col), ylim = ylim),
        list(...))
    args$height <- props
    do.call(graphics::barplot, args)
    if(cutoff > 0 && type == "clusters") graphics::abline(h = cutoff, lty = 2, col = "red")
    invisible(props)   # sorted; names as-is (so it equals sort(colMeans(...)) for the test)
}
