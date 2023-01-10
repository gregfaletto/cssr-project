# Generated from create-cssr.Rmd: do not edit by hand

#' Generates matrix of selection indicators from stability selection.
#'
#' @param x an n x p numeric matrix or a data.frame containing the predictors.
#' @param y A response vector; can be any response that takes the form of a
#' length n vector and is used (or not used) by fitfun. Typically (and for
#' default fitfun = cssLasso), y should be an n-dimensional numeric vector
#' containing the response.
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method. For example, in the default case when
#' fitfun = cssLasso, lambda is a numeric: the penalty to use for each lasso
#' fit.
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the number of lasso fits will be `B`; for
#' sampling_type="SS" the number of lasso fits will be `2*B`.
#' @param sampling_type A character vector (either "SS" or "MB"); the sampling
#' type used for stability selection.
#' @param subsamps_object A list of length `B` (or `2*B` if sampling_type="SS"),
#' where each element is one of the following: \item{subsample}{An integer
#' vector of size `n/2` containing the indices of the observations in the
#' subsample.} \item{drop_var_input}{A named list containing two elements: one
#' named "subsample", matching the previous description, and a logical vector
#' named "feats_to_keep" containing the indices of the features to be
#' automatically selected.} (The first object is the output of the function
#' createSubsamples when the provided prop_feats_remove is 0, the default, and
#' the second object is the output of createSubsamples when prop_feats_remove >
#' 0.)
#' @param num_cores Optional; an integer. If using parallel processing, the
#' number of cores to use for parallel processing (num_cores will be supplied
#' internally as the mc.cores argument of parallel::mclapply).
#' @param fitfun A function that takes in arguments X, y, and lambda and returns
#' a vector of indices of the columns of X (selected features). Default is
#' cssLasso, an implementation of the lasso using glmnet.
#' @return A binary integer matrix of dimension `B` x `p` (if sampling_type ==
#' "MB") or `2*B` x `p` (if sampling_type == "SS"). res[i, j] = 1 if feature j
#' was selected on subsample i and equals 0 otherwise. (That is, each row is a
#' selected set.)
#' @author Gregory Faletto, Jacob Bien
getSelMatrix <- function(x, y, lambda, B, sampling_type, subsamps_object,
    num_cores, fitfun=cssLasso){

    # Check inputs

    stopifnot(is.matrix(x))
    stopifnot(all(!is.na(x)))

    n <- nrow(x)
    p <- ncol(x)

    stopifnot(length(y) == n)
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    checkSamplingType(sampling_type)

    # Get list of selected feature sets from subsamples

    res_list <- parallel::mclapply(X=subsamps_object, FUN=cssLoop, x=x, y=y,
        lambda=lambda, fitfun=fitfun, mc.cores=num_cores)

    # Store selected sets in B x p (or `2*B` x p for "SS") binary matrix
    if(sampling_type=="SS"){
        res <- matrix(0L, 2*B, p)
    } else if(sampling_type=="MB"){
        res <- matrix(0L, B, p)
    } else{
        stop("!(sampling_type %in% c(SS, MB)) (don't know how to set dimensions of res")
    }

    stopifnot(length(res_list) == nrow(res))

    # Get selected sets into matrix res

    for(i in 1:nrow(res)){
        if(length(res_list[[i]]) == 0){
            # If no features are selected, don't fill in anything in this row
            next
        }

        if(!is.integer(res_list[[i]])){
            print(paste("failed on iteration", i))
            print(res_list[[i]])
            stop("Something seems to be wrong with the feature selection method (fitfun failed to return an integer vector)")
        }
        stopifnot(length(res_list[[i]]) <= p & length(res_list[[i]]) > 0)
        stopifnot(all(!is.na(res_list[[i]])))
        stopifnot(max(res_list[[i]]) <= p)
        stopifnot(min(res_list[[i]]) >= 1)
        stopifnot(length(res_list[[i]]) == length(unique(res_list[[i]])))
        stopifnot(!("try-error" %in% class(res_list[[i]]) |
            "error" %in% class(res_list[[i]]) |
            "simpleError" %in% class(res_list[[i]]) |
            "condition" %in% class(res_list[[i]])))

        # Store selected variables in the ith row of res
        res[i, res_list[[i]]] <- 1L
    }

    # Check output

    stopifnot(is.matrix(res))
    if(sampling_type=="SS"){
        stopifnot(nrow(res) == 2*B)
    } else{
        stopifnot(nrow(res) == B)
    }
    stopifnot(ncol(res) == p)
    stopifnot(all(res %in% c(0, 1)))

    return(res)
}
