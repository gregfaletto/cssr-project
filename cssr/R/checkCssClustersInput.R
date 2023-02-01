# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that clusters input to css is as expected
#'
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of `1:p`). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves). If
#' clusters is a list of length 0 (or a list only containing clusters of length
#' 1), then `css()` returns the same results as stability selection (so the
#' returned `feat_sel_mat` will be identical to `clus_sel_mat`). Names for the
#' clusters will be needed later; any clusters that are not given names in the
#' provided list will be given names automatically by `css()`. #' CAUTION: if
#' the provided X is a data.frame that contains a categorical feature with more
#' than two levels, then the resulting matrix made from model.matrix will have a
#' different number of columns than the provided data.frame, some of the feature
#' numbers will change, and the clusters argument will not work properly (in the
#' current version of the package). To get correct results in this case, please
#' use model.matrix to convert the data.frame to a numeric matrix on your own,
#' then provide this matrix and cluster assignments with respect to this matrix.
#' Default is `list()` (so no clusters are specified).
#' @return Same as the input, but all of the clusters will be coerced to
#' integers.
#' @author Gregory Faletto, Jacob Bien
checkCssClustersInput <- function(clusters){
    stopifnot(!is.na(clusters))
    if(is.list(clusters)){
        stopifnot(all(!is.na(clusters)))
        stopifnot(length(clusters) == length(unique(clusters)))

        if(length(clusters) > 0){
            for(i in 1:length(clusters)){
                stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
                stopifnot(all(!is.na(clusters[[i]])))
                stopifnot(is.integer(clusters[[i]]) | is.numeric(clusters[[i]]))
                stopifnot(all(clusters[[i]] == round(clusters[[i]])))
                stopifnot(all(clusters[[i]] >= 1))
                clusters[[i]] <- as.integer(clusters[[i]])
            }

            if(length(clusters) >= 2){
                # Check that clusters are non-overlapping
                for(i in 1:(length(clusters) - 1)){
                    for(j in (i+1):length(clusters)){
                        if(length(intersect(clusters[[i]], clusters[[j]])) != 0){
                            error_mes <- paste("Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: ",
                                i, ", ", j, ".", sep="")
                            stop(error_mes)
                        }
                    }
                }
            }
        }
    } else{
        # If clusters is not a list, it should be a vector of indices of
        # features that are in the (one) cluster
        stopifnot(is.numeric(clusters) | is.integer(clusters))
        stopifnot(length(clusters) == length(unique(clusters)))
        stopifnot(all(!is.na(clusters)))
        stopifnot(is.integer(clusters) | is.numeric(clusters))
        stopifnot(all(clusters == round(clusters)))
        stopifnot(all(clusters >= 1))
        clusters <- as.integer(clusters)
    }
    return(clusters)
}
