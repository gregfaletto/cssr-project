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
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
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
