# Generated from _main.Rmd: do not edit by hand

#' Verifies the inputs for getXglmnet.
#'
#' @param x A numeric matrix; the provided matrix with n observations and p
#' features.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters should
#' be equal to the number of clusters.) All identified clusters should be
#' non-overlapping. All features should appear in exactly one cluster (any
#' unclustered features should be put in their own "cluster" of size 1).
#' @param type Character; "protolasso" for the protolasso or "clusterRepLasso"
#' for the cluster representative lasso.
#' @param prototypes Only required for type "protolasso". An integer vector
#' whose length is equal to the number of clusters. Entry i should be the
#' prototype for cluster i (the feature belonging to cluster i that is most
#' highly correlated with y; see Reid and Tibshirani 2016).
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
checkGetXglmnetInputs <- function(x, clusters, type, prototypes){
    stopifnot(is.matrix(x))

    stopifnot(is.list(clusters))
    stopifnot(all(lengths(clusters) >= 1))

    stopifnot(length(type) == 1)
    stopifnot(is.character(type))
    stopifnot(!is.na(type))
    stopifnot(type %in% c("protolasso", "clusterRepLasso"))

    stopifnot(!is.na(prototypes))
    stopifnot(is.integer(prototypes))
    stopifnot(all(!is.na(prototypes)))
    stopifnot(length(prototypes) == length(unique(prototypes)))
    stopifnot(all(prototypes %in% 1:ncol(x)))
    
    for(i in 1:length(clusters)){
        cluster_i <- clusters[[i]]
        stopifnot(sum(prototypes %in% cluster_i) == 1)
    }
}
