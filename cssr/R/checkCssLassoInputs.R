# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the inputs to `cssLasso()` are as expected. 
#'
#' @param X A design matrix containing the predictors. (Note that we don't need
#' to check X very much, because X will have already been checked by the
#' function `checkCssInputs()` when it was provided to `css()`.)
#' @param y A numeric vector containing the response.
#' @param lambda Either a single nonnegative number for the lasso penalty to
#' use on each subsample, or a named length-2 numeric vector
#' `c(lambda = <value>, alpha = <value>)` bundling the penalty together with the
#' elastic net mixing parameter alpha (which must be in `(0, 1]`). (For now,
#' only one lambda value can be provided to `cssLasso()`; in the future, we plan
#' to allow for multiple lambda values to be provided to `cssLasso()`, as
#' described in Faletto and Bien 2022.)
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkCssLassoInputs <- function(X, y, lambda){

    n <- nrow(X)
    p <- ncol(X)

    if(!is.numeric(y) & !is.integer(y)){
        stop("For method cssLasso, y must be a numeric or integer vector.")
    }
    if(any(!is.finite(y))){
        stop("For method cssLasso, y must not contain missing (NA) or non-finite (Inf) values.")
    }
    if(is.matrix(y)){
        stop("For method cssLasso, y must be a numeric vector (inputted y was a matrix).")
    }
    if(n != length(y)){
        stop("For method cssLasso, y must be a vector of length equal to nrow(X).")
    }
    if(length(unique(y)) <= 1){
        stop("Subsample with only one unique value of y detected: for the default cssLasso, every subsample of y (of size floor(n/2)) must have more than one unique value. css draws random subsamples, so this abort is seed-dependent and becomes more likely as B grows. It indicates that y is too discrete for the default cssLasso--supply a less discrete (more continuous) response, or pass a custom fitfun that tolerates a constant-y subsample.")
    }
    if(!is.numeric(lambda) & !is.integer(lambda)){
        stop("For method cssLasso, lambda must be a numeric.")
    }
    if(any(is.na(lambda))){
        stop("NA detected in provided lambda input to cssLasso")
    }
    # lambda may be either a single nonnegative number (pure lasso) or a named
    # length-2 numeric vector c(lambda=<value>, alpha=<value>) bundling the
    # elastic net mixing parameter alpha (in (0, 1]) alongside the penalty.
    if(length(lambda) == 1){
        if(lambda < 0){
            stop("For method cssLasso, lambda must be nonnegative.")
        }
    } else if(length(lambda) == 2){
        if(!setequal(names(lambda), c("lambda", "alpha"))){
            stop("For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).")
        }
        if(lambda["lambda"] < 0){
            stop("For method cssLasso, the lambda component of lambda must be nonnegative.")
        }
        if(lambda["alpha"] <= 0 | lambda["alpha"] > 1){
            stop("For method cssLasso, the alpha component of lambda must be in (0, 1].")
        }
    } else{
        stop("For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).")
    }
}
