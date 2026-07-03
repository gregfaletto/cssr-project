# Generated from _main.Rmd: do not edit by hand

#' Build the printable cluster-summary data.frame from selected clusters
#'
#' Constructs the data.frame returned by printCssDf() (and summary.cssr()) from
#' a css_results object and an already-obtained named vector of selected
#' clusters. Factored out of printCssDf() (#129) so callers that have already
#' computed the selected clusters (e.g. summary.cssr(), which calls
#' getCssSelections() itself) can reuse that result rather than triggering a
#' second identical getCssSelections() call. Because cluster selection is
#' invariant to the weighting rule, the sel_clusts computed by summary.cssr()
#' for any weighting matches the "sparse"-default selection printCssDf() would
#' recompute, so the returned data.frame is byte-identical either way.
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param sel_clusts A named numeric vector of selected clusters with their
#' selection proportions, as returned in the selected_clusts element of
#' getCssSelections() / getSelectedClusters() (the value printCssDf() itself
#' obtains via getCssSelections()). May be empty (length 0), in which case a
#' well-formed zero-row data.frame is returned.
#' @return A data.frame; each row contains a cluster, arranged in decreasing
#' order of cluster selection proportion from top to bottom (see printCssDf()
#' for the full column description).
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
buildCssDf <- function(css_results, sel_clusts){
    # An empty selection is valid post-#107 (e.g. cutoff = 1 with
    # min_num_clusts = 0 when no cluster clears the cutoff). Return a well-formed
    # zero-row data.frame whose columns mirror the populated branch below for the
    # same object: 5 columns (with ClustProtoName) when X has column names, 4
    # otherwise -- names(prototypes) is non-NULL iff colnames(css_results$X) is,
    # since prototype names come from colnames(X). (#120)
    if(length(sel_clusts) == 0){
        if(!is.null(colnames(css_results$X))){
            return(data.frame(ClustName=character(0),
                ClustProtoName=character(0), ClustProtoNum=integer(0),
                ClustSelProp=numeric(0), ClustSize=integer(0)))
        }
        return(data.frame(ClustName=character(0), ClustProtoNum=integer(0),
            ClustSelProp=numeric(0), ClustSize=integer(0)))
    }

    # Get prototypes (feature from each cluster with highest selection
    # proportion, breaking ties by using marginal correlations of features with
    # y from data provided to css if y is real-valued)
    prototypes <- getSelectionPrototypes(css_results, sel_clusts)

    # Cluster selection proportions
    if(length(sel_clusts) > 1){
        sel_clust_sel_props <- colMeans(css_results$clus_sel_mat[,
            names(sel_clusts)])
    } else{
        sel_clust_sel_props <- mean(css_results$clus_sel_mat[,
            names(sel_clusts)])
    }

    # Data.frame: name of cluster, cluster prototype, selection proportion,
    # cluster size

    if(!is.null(names(prototypes))){
        print_df <- data.frame(ClustName=names(sel_clusts),
            ClustProtoName=names(prototypes), ClustProtoNum=unname(prototypes),
            ClustSelProp=sel_clust_sel_props, ClustSize=lengths(sel_clusts))
    } else{
        print_df <- data.frame(ClustName=names(sel_clusts),
            ClustProtoNum=unname(prototypes), ClustSelProp=sel_clust_sel_props,
            ClustSize=lengths(sel_clusts))
    }

    print_df <- print_df[order(print_df$ClustSelProp, decreasing=TRUE), ]

    rownames(print_df) <- NULL

    stopifnot(is.data.frame(print_df))
    stopifnot(nrow(print_df) >= 1)

    return(print_df)
}
