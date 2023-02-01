# Generated from _main.Rmd: do not edit by hand

#' Obtain a selected set of clusters and features using cluster stability
#' selection
#'
#' Takes in data X and y and returns a set of clusters (and a set of features)
#' that are useful for predicting y from the data in X. This is a wrapper
#' function for css and getCssSelections. Using cssSelect is simpler, but it
#' has fewer options, and it executes the full (computationally expensive)
#' subsampling procedured every time it is called. In contrast, css can be
#' called just once, and then getCssSelections can quickly return results using
#' different values of cutoff, max_num_clusts, etc. from the calculations done
#' in one call to css.
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors.
#' @param y A length-n numeric vector containing the responses; `y[i]` is the
#' response corresponding to observation `X[i, ]`. (Note that for the css
#' function, y does not have to be a numeric response, but for this function,
#' the underlying selection procedure is the lasso, so y must be a real-valued
#' response.)
#' @param clusters Optional; either an integer vector of a list of integer
#' vectors; each vector should contain the indices of a cluster of features (a
#' subset of 1:p). (If there is only one cluster, clusters can either be a list
#' of length 1 or an integer vector.) All of the provided clusters must be
#' non-overlapping. Every feature not appearing in any cluster will be assumed
#' to be unclustered (that is, they  will be treated as if they are in a
#' "cluster" containing only themselves). If clusters is a list of length 0 (or
#' a list only containing clusters of length 1), then css() returns the same
#' results as stability selection (so feat_sel_mat will be identical to
#' clus_sel_mat). Names for the clusters will be needed later; any clusters that
#' are not given names in the list clusters will be given names automatically by
#' css. CAUTION: if the provided X is a data.frame that contains a categorical
#' feature with more than two levels, then the resulting matrix made from
#' model.matrix will have a different number of columns than the provided
#' data.frame, some of the feature numbers will change, and the clusters
#' argument will not work properly (in the current version of the package). To
#' get correct results in this case, please use model.matrix to convert the
#' data.frame to a numeric matrix on your own, then provide this matrix and
#' cluster assignments with respect to this matrix. Default is list() (so no
#' clusters are specified, and every feature is assumed to be in a "cluster"
#' containing only itself).
#' @param lambda Optional; the tuning parameter to be used by the lasso for
#' feature selection in each subsample. If lambda is not provided, cssSelect
#' will choose one automatically by cross-validation. Default is NA.
#' @param cutoff Numeric; cssSelect will only select those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is NA (in which case max_num_clusts are used).
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' either cutoff is used to choose the number of clusters, or if cutoff was also
#' unspecified, cssSelect chooses max_num_clusts by cross-validation).
#' @param auto_select_size Logical; if TRUE, then max_num_clusts will be
#' automatically estimated using the lasso with cross-validation. Default is
#' TRUE, though his argument is ignored if either cutoff or max_num_clusts is
#' provided. (If desired output is to return all clusters, you should set
#' auto_select_size to FALSE and do not provide cutoff or max_num_clusts.)
#' @return A named list with two items. \item{selected_clusts}{A list of
#' integer vectors; each vector contains the indices of one of the selected
#' clusters.} \item{selected_feats}{An integer vector; the indices of the
#' all of the selected features within all of the selected clusters (typically
#' only one feature is selected from each cluster).}
#' @author Gregory Faletto, Jacob Bien
#' @export
cssSelect <- function(X, y, clusters = list(), lambda=NA, cutoff=NA,
    max_num_clusts=NA, auto_select_size=TRUE){

    # Check inputs (most inputs will be checked by called functions)

    stopifnot(!is.na(auto_select_size))
    stopifnot(length(auto_select_size) == 1)
    stopifnot(is.logical(auto_select_size))

    stopifnot(is.matrix(X) | is.data.frame(X))
    stopifnot(all(!is.na(X)))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))

    if(!is.numeric(y) & !is.integer(y)){
        stop("The provided y must be real-valued, because cssSelect uses the lasso for feature selection. (In order to use a different form of response, use the css function and provide your own selection function accommodating your choice of y.)")
    }

    stopifnot(length(lambda) == 1)
    if(is.na(lambda)){
        lambda <- getLassoLambda(X, y)
    }

    css_results <- css(X, y, lambda, clusters)

    # If no indication of how to select model size was provided, choose model
    # size by cross-validation
    if(is.na(cutoff) & is.na(max_num_clusts)){
        if(auto_select_size){
            max_num_clusts <- getModelSize(X, y, css_results$clusters)
        }
    }

    if(is.na(cutoff)){
        cutoff <- 0
    }

    # Get selected features
    getCssSelections(css_results, weighting="sparse", cutoff=cutoff,
        min_num_clusts=1, max_num_clusts=max_num_clusts)
}
