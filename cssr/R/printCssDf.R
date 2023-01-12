# Generated from create-cssr.Rmd: do not edit by hand

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
#' css), and ClustSize (the size of the cluster).
#' @author Gregory Faletto, Jacob Bien
#' @export
printCssDf <- function(css_results, cutoff=0, min_num_clusts=1,
    max_num_clusts=NA){
    # Check inputs
    stopifnot(class(css_results) == "cssr")
    checkCutoff(cutoff)

    p <- ncol(css_results$feat_sel_mat)

    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    sel_clusts <- getCssSelections(css_results, cutoff=cutoff,
        min_num_clusts=min_num_clusts,
        max_num_clusts=max_num_clusts)$selected_clusts

    # sel_clusts is guaranteed to have length at least 1 by
    # getCssSelections 

    # Get prototypes (feature from each cluster with highest selection
    # proportion, breaking ties by using marginal correlations of features with
    # y from data provided to css if y is real-valued)
    prototypes <- getSelectionPrototypes(css_results, sel_clusts)
    
    # Cluster selection proportions
    sel_clust_sel_props <- colMeans(css_results$clus_sel_mat[,
        names(sel_clusts)])

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
