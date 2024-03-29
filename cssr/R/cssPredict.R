# Generated from _main.Rmd: do not edit by hand

#' Wrapper function to generate predictions from cluster stability selected
#' model in one step
#'
#' Select clusters using cluster stability selection, form cluster
#' representatives, fit a linear model, and generate predictions from a matrix
#' of unlabeled data. This is a wrapper function for css and getCssPreds. Using
#' cssPredict is simpler, but it has fewer options, and it executes the full
#' (computationally expensive) subsampling procedured every time it is called.
#' In contrast, css can be called just once, and then cssPredict can quickly
#' return results for different matrices of new data or using different values
#' of cutoff, max_num_clusts, etc. by using the calculations done in one call to
#' css.
#'
#' @param X_train_selec An n x p numeric matrix (preferably) or a data.frame
#' (which will be coerced internally to a matrix by the function model.matrix)
#' containing the p >= 2 features/predictors. The data from X_train_selec and
#' y_train_selec will be split into two parts; half of the data will be used for
#' feature selection by cluster stability selection, and half will be used for
#' estimating a linear model on the selected cluster representatives.
#' @param y_train_selec A length-n numeric vector containing the responses;
#' `y[i]` is the response corresponding to observation `X[i, ]`. Unlke the more
#' general setup of css, y_train_selec must be real-valued because predictions
#' will be generated by ordinary least squares.
#' @param X_test A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate predictions. Must contain the same
#' features (in the same number of columns) as X_train_selec, and if the columns
#' of X_test are named, they must match the names of X_train_selec.
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
#' cluster assignments with respect to this matrix.Default is list() (so no
#' clusters are specified, and every feature is assumed to be in a "cluster"
#' containing only itself).
#' @param lambda Optional; the tuning parameter to be used by the lasso for
#' feature selection in each subsample. If lambda is not provided, cssPredict
#' will choose one automatically by cross-validation. Default is NA.
#' @param cutoff Numeric; getCssPreds will make use only of those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is 0 (in which case either all clusters are used, or max_num_clusts
#' are used, if max_num_clusts is specified).
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param train_inds Optional; an integer or numeric vector containing the
#' indices of observations in X and y to set aside for model training after
#' feature selection. If train_inds is not provided, half of the data will be
#' used for feature selection and half for model estimation (chosen at random).
#' @param auto_select_size Logical; if TRUE, then max_num_clusts will be
#' automatically estimated using the lasso with cross-validation. Default is
#' TRUE, though his argument is ignored if either cutoff or max_num_clusts is
#' provided. (If desired output is to generate predictions using all clusters,
#' you should set auto_select_size to FALSE and do not provide cutoff or
#' max_num_clusts.)
#' @return A numeric vector of length nrow(X_test) of predictions
#' corresponding to the observations from X_test.
#' @author Gregory Faletto, Jacob Bien
#' @export
cssPredict <- function(X_train_selec, y_train_selec, X_test, clusters=list(),
    lambda=NA, cutoff=NA, max_num_clusts=NA, train_inds=NA,
    auto_select_size=TRUE){

    # Check inputs (most inputs will be checked by called functions)
    if(!is.numeric(y_train_selec) & !is.integer(y_train_selec)){
        stop("The provided y_train_selec must be real-valued, because predictions will be generated by ordinary least squares regression.")
    }

    stopifnot(!is.na(auto_select_size))
    stopifnot(length(auto_select_size) == 1)
    stopifnot(is.logical(auto_select_size))

    stopifnot(is.matrix(X_train_selec) | is.data.frame(X_train_selec))
    stopifnot(all(!is.na(X_train_selec)))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X_train_selec)){
        p <- ncol(X_train_selec)
        X_train_selec <- stats::model.matrix(~ ., X_train_selec)
        X_train_selec <- X_train_selec[, colnames(X_train_selec) !=
            "(Intercept)"]

        if(length(clusters) > 0 & (p != ncol(X_train_selec))){
            stop("When stats::model.matrix converted the provided data.frame X_train_selec to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X_train_selec to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.")
        }
    }

    stopifnot(is.matrix(X_train_selec))
    stopifnot(all(!is.na(X_train_selec)))

    n <- nrow(X_train_selec)

    if(any(is.na(train_inds))){
        train_inds <- sample(n, size=round(n/2))
    }

    stopifnot(length(lambda) == 1)
    if(is.na(lambda)){
        lambda <- getLassoLambda(X_train_selec[setdiff(1:n, train_inds), ],
            y_train_selec[setdiff(1:n, train_inds)]) 
    }

    css_results <- css(X=X_train_selec, y=y_train_selec, lambda=lambda,
        clusters=clusters, train_inds=train_inds)

    # If no indication of how to select model size was provided, choose model
    # size by cross-validation
    if(is.na(cutoff) & is.na(max_num_clusts)){
        if(auto_select_size){
            max_num_clusts <- getModelSize(X_train_selec[train_inds, ],
                y_train_selec[train_inds], css_results$clusters)
        }
    }

    if(is.na(cutoff)){
        cutoff <- 0
    }

    # Get predictions
    getCssPreds(css_results, testX=X_test, weighting="weighted_avg",
        cutoff=cutoff, max_num_clusts=max_num_clusts)
}
