# Generated from create-cssr.Rmd: do not edit by hand

#' Formats clusters in standardized way, optionally estimating cluster
#' prototypes
#'
#' @param clusters Either an integer vector of a list of integer vectors; each
#' vector should contain the indices of a cluster of features. (If there is only
#' one cluster, clusters can either be a list of length 1 or simply an integer
#' vector.) If clusters is specified then R is ignored.
#' @param p integer or numeric; the numbe of features in x (should match 
#' ncol(x), if x is provided)
#' @param clust_names A character vector of the names of the clusters in clusters.
#' @param get_prototypes Logical: if TRUE, will identify prototype from each
#' cluster (the feature from each cluster that is most correlated with the
#' response) for the protolasso. In this case, x and y must be provided.
#' @param x n x p numeric matrix; design matrix. Only needs to be provided if
#' get_prototypes is TRUE.
#' @param y Numeric response vector; only needs to be provided if get_prototypes
#' is TRUE. Note: in general, the css function does not require y to be a
#' numeric vector, because the provided fitfun could use a different form of y
#' (for example, a categorical response variable). However, y must be numeric in
#' order to provide prototypes because the prototypes are determined using the
#' correlation between cluster members (columns of x) and y.
#' @param R Numeric p x p matrix; not currently used. Entry ij contains the 
#' "substitutive value" of feature i for feature j (diagonal must consist of
#' ones, all entries must be between 0 and 1, and matrix must be symmetric)
#' @return A named list with the following elements: \item{clusters}{A named
#' list where each entry is an integer vector of indices of features that are in
#' a common cluster. (The length of list clusters is equal to the number of
#' clusters.) All identified clusters are non-overlapping. All features appear
#' in exactly one cluster (any unclustered features will be put in their own
#' "cluster" of size 1).} \item{multiple}{Logical; TRUE if there is more than
#' one cluster of size greater than 1, FALSE otherwise.} \item{prototypes}{only
#' returned if get_prototypes=TRUE. An integer vector whose length is equal to
#' the number of clusters. Entry i is the index of the feature belonging to
#' cluster i that is most highly correlated with y (that is, the prototype for
#' the cluster, as in the protolasso; see Reid and Tibshirani 2016).}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
formatClusters <- function(clusters=NA, p=-1, clust_names=NA, 
    get_prototypes=FALSE, x=NA, y=NA, R=NA){

    # Check inputs
    clusters <- checkFormatClustersInput(clusters, p, clust_names,
        get_prototypes, x, y, R)

    n <- nrow(x)

    multiple <- FALSE

    if(any(lengths(clusters) > 1)){ # & length(clusters) > 1
        # Only care about clusters with more than one element (only ones that
        # need to be treated differently)
        # keep track of whether there's more than one cluster or not
        multiple <- sum(lengths(clusters) > 1) > 1
    }

    # For any features not already in a cluster, add a cluster containing only
    # that feature
    orig_length_clusters <- length(clusters)

    stopifnot(p >= 1)
    for(i in 1:p){
        feat_i_found <- FALSE
        if(orig_length_clusters > 0){
            for(j in 1:orig_length_clusters){
                # If i is in cluster j, break out of this loop and consider the
                # next i
                if(i %in% clusters[[j]]){
                    feat_i_found <- TRUE
                    break
                }
            }
        }

        # If feature i wasn't found in any cluster, add a cluster containing
        # just feature i
        if(!feat_i_found){
            clusters[[length(clusters) + 1]] <- i
        }
    }

    n_clusters <- length(clusters)

    # Add names to clusters
    if(is.null(names(clusters))){
        names(clusters) <- paste("c", 1:n_clusters, sep="")
    } else{
        # What clusters need names?
        unnamed_clusts <- which(is.na(names(clusters)) | names(clusters) == "")
        proposed_clust_names <- paste("c", unnamed_clusts, sep="")
        # Replace any proposed cluster names that are already in use
        if(any(proposed_clust_names %in% names(clusters))){
            proposed_clust_names[proposed_clust_names %in% names(clusters)] <- paste("c",
                unnamed_clusts[proposed_clust_names %in% names(clusters)] + p,
                sep="")
        }
        while_counter <- 0
        while(any(proposed_clust_names %in% names(clusters))){
            proposed_clust_names[proposed_clust_names %in% names(clusters)] <- paste(proposed_clust_names[proposed_clust_names %in% names(clusters)],
                "_1", sep="")
            while_counter <- while_counter + 1
            if(while_counter >= 100){
                stop("Function formatClusters stuck in an infinite while loop")
            }
        }
        stopifnot(length(unnamed_clusts) == length(proposed_clust_names))
        names(clusters)[unnamed_clusts] <- proposed_clust_names
    }

    # Check output

    checkClusters(clusters, p)
    stopifnot(is.logical(multiple))
    stopifnot(length(multiple) == 1)
    stopifnot(!is.na(multiple))

    if(get_prototypes){
        prototypes <- getPrototypes(clusters, x, y)

        return(list(clusters=clusters, multiple=multiple,
            prototypes=prototypes))
    } else{
        return(list(clusters=clusters, multiple=multiple))
    }
}
