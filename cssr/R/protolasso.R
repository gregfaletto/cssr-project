# Generated from _main.Rmd: do not edit by hand

#' Select features via the protolasso (Reid and Tibshirani 2016)
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors
#' @param y The response; A length n numeric (or integer) real-valued vector.
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves).
#' CAUTION: if the provided X is a data.frame that contains a categorical
#' feature with more than two levels, then the resulting matrix made from
#' model.matrix will have a different number of columns than the provided
#' data.frame, some of the feature numbers will change, and the clusters
#' argument will not work properly (in the current version of the package).
#' To get correct results in this case, please use model.matrix to convert
#' the data.frame to a numeric matrix on your own, then provide this matrix
#' and cluster assignments with respect to this matrix. Default is list() (so no
#' clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the protolasso. Default is 100 (following the default for glmnet). For
#' now, nlambda must be at least 2 (using a single lambda is not supported).
#' @return A list with three elements. \item{selected_sets}{A list of integer
#' vectors. Entry k of this list contains a selected set (an integer vector) of
#' size k yielded by the protolasso (If no set of size k was selected, entry k
#' will be empty.)} \item{selected_clusts_list}{A list; each element of the list
#' is a named list of selected clusters. (That is, if a selected set of size k
#' was yielded by the protolasso, then `selected_clusts_list[[k]]` is a named
#' list of length k, where each member of the list is an integer vector
#' of cluster members. In particular, `selected_clusts_lists[[k]][[j]]` will be
#' the cluster that contains feature `selected_sets[[k]][j]`.)} \item{beta}{The
#' beta output from glmnet when the lasso was estimated on a matrix of
#' prototypes. (See documentation for the function glmnet from the glmnet
#' package for details.)}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' clusters <- list(cluster1 = 1:4)
#' res <- protolasso(X = data$X, y = data$y, clusters = clusters)
#' str(res, max.level = 1)
#' @export
protolasso <- function(X, y, clusters=list(), nlambda=100){
    clusterLassoCore(X, y, clusters, nlambda, type="protolasso")
}
