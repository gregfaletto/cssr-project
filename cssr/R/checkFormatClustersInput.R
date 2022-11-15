# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to ensure that the inputs to formatClusters are as expected
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
#' @return A list of integer vectors; each vector will contain the indices of a
#' cluster of features. Any duplicated clusters provided in the input will be
#' removed.
#' @author Gregory Faletto, Jacob Bien
checkFormatClustersInput <- function(clusters, p, clust_names, 
    get_prototypes, x, y, R){

    if(any(is.na(clusters)) & any(is.na(R))){
        stop("Must specify one of clusters or R (or does one of these provided inputs contain NA?)")
    }

    stopifnot(is.integer(p) | is.numeric(p))
    stopifnot(length(p) == 1)
    stopifnot(p == round(p))
    stopifnot(!is.na(p))
    if(p > 0){
        stopifnot(p >= 2)
    }

    use_R <- FALSE
    if(any(is.na(clusters)) | length(clusters) == 0){
        if(all(!is.na(R))){
            stopifnot(is.matrix(R))
            stopifnot(all(dim(R) == p))
            stopifnot(all(diag(R) == 1))
            stopifnot(identical(R, t(R)))
            stopifnot(all(!is.na(R)))
            stopifnot(all(R %in% c(0, 1)))
            use_R <- TRUE
        }
    } else{
        stopifnot(!is.list(clusters) | all(lengths(clusters) >= 1))
        stopifnot(is.list(clusters) | length(clusters) >= 1)
        stopifnot(all(!is.na(clusters)))

        if(is.list(clusters) & length(clusters) > 0){
            for(i in 1:length(clusters)){
                stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
                stopifnot(all(!is.na(clusters[[i]])))
                stopifnot(all(clusters[[i]] >= 1))
                stopifnot(is.integer(clusters[[i]]))
            }

            stopifnot(length(clusters) == length(unique(clusters)))

            clusters <- clusters[!duplicated(clusters)]

            if(length(clusters) >= 2){
                # Check that clusters are non-overlapping
                for(i in 1:(length(clusters) - 1)){
                    for(j in (i+1):length(clusters)){
                        stopifnot(length(intersect(clusters[[i]],
                            clusters[[j]])) == 0)
                    }
                }
            }

            if(any(!is.na(clust_names))){
                stopifnot(length(clust_names) == length(clusters))
            }
        } else if(!is.list(clusters)){
            clusters_temp <- clusters
            clusters <- list()
            clusters[[1]] <- clusters_temp
            rm(clusters_temp)
        }
    }

    stopifnot(length(get_prototypes) == 1)
    stopifnot(is.logical(get_prototypes))

    if(any(!is.na(clust_names))){
        stopifnot(is.character(clust_names))
    }

    if(get_prototypes){
        stopifnot(all(!is.na(x)))
        stopifnot(is.matrix(x))

        n <- nrow(x)

        checkY(y, n)
    }

    if(use_R){
        # Determine clusters from R
        clusters <- list()

        for(i in 1:nrow(R)){
            clusters[[i]] <- as.integer(which(R[i, ] > 0))
            stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
            stopifnot(all(!is.na(clusters[[i]])))
            stopifnot(is.integer(clusters[[i]]))
        }

        clusters <- unique(clusters)
        stopifnot(is.list(clusters))

        if(length(clusters) >= 2){
            # Check that clusters are non-overlapping
            for(i in 1:(length(clusters) - 1)){
                for(j in (i+1):length(clusters)){
                    if(length(intersect(clusters[[i]], clusters[[j]])) != 0){
                        stop("Invalid R matrix with overlapping clusters (clusters must not be overlapping)")
                    }
                }
            }
        }
    }

    stopifnot(is.list(clusters))

    return(clusters)
}
