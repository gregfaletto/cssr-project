# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the inputs to `cssLasso()` are as expected. 
#'
#' @param X A design matrix containing the predictors. (Note that we don't need
#' to check X very much, because X will have already been checked by the
#' function `checkCssInputs()` when it was provided to `css()`.)
#' @param y A numeric vector containing the response.
#' @param lambda Numeric; a nonnegative number for the lasso penalty to use
#' on each subsample. (For now, only one lambda value can be provided to
#' `cssLasso()`; in the future, we plan to allow for multiple lambda values to be
#' provided to `cssLasso()`, as described in Faletto and Bien 2022.)
#' @author Gregory Faletto, Jacob Bien
checkCssLassoInputs <- function(X, y, lambda){

    n <- nrow(X)
    p <- ncol(X)

    if(!is.numeric(y)){
        stop("For method cssLasso, y must be a numeric vector.")
    }
    if(is.matrix(y)){
        stop("For method cssLasso, y must be a numeric vector (inputted y was a matrix).")
    }
    if(n != length(y)){
        stop("For method cssLasso, y must be a vector of length equal to nrow(X).")
    }
    if(length(unique(y)) <= 1){
        stop("Subsample with only one unique value of y detected--for method cssLasso, all subsamples of y of size floor(n/2) must have more than one unique value.")
    }
    if(!is.numeric(lambda) & !is.integer(lambda)){
        stop("For method cssLasso, lambda must be a numeric.")
    }
    if(any(is.na(lambda))){
        stop("NA detected in provided lambda input to cssLasso")
    }
    if(length(lambda) != 1){
        stop("For method cssLasso, lambda must be a numeric of length 1.")
    }
    if(lambda < 0){
        stop("For method cssLasso, lambda must be nonnegative.")
    }
}
