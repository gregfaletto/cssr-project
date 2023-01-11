# Generated from create-cssr.Rmd: do not edit by hand

#' Get lambda value for lasso
#'
#' Chooses a lambda value for the lasso used on a subsample of size n/2 (as in
#' cluster stability selection) by cross-validation.
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors that will be used by cluster stability
#' selection.
#' @param y The response; an n-dimensional numeric or integer vector. (Unlike
#' in the more general css setup, this response must be real-valued since
#' lambda will be determined using the lasso with cross-validation.)
#' @param lambda_choice Character; either "min" or "1se". If "min", chooses
#' the lambda that minimizes the cross-validated error; if "1se", chooses the
#' largest lambda within one standard error of the minimum error lambda
#' (resulting in a smaller selected set, which may be desirable because the
#' model size corresponding to the minimum error lambda tends to be larger
#' than optimal. See, for example, Bühlmann and Meinshausen 2006, Prop. 1 and
#' Bühlmann and van de Geer 2011, Section 2.5.1.). Default is "1se".
#' @param nfolds Numeric or integer; the number of folds for cross-validation.
#' Must be at least 4 (as specified by cv.glmnet). Default is 10.
#' @return A numeric; the selected value of lambda.
#' @author Gregory Faletto, Jacob Bien
#' @references Bühlmann, P., & Meinshausen, N. (2006). High-Dimensional Graphs
#' and Variable Selection With the Lasso. \emph{The Annals of Statistics},
#' 34(3), 1436–1462. \url{https://doi.org/10.1214/009053606000000281}.
#' \cr Peter Bühlmann and Sara van de Geer. Statistics for High-Dimensional
#' Data. \emph{Springer Series in Statistics}. Springer, Heidelberg, 2011. ISBN
#' 978-3-642-20191-2. \url{http://dx.doi.org/10.1007/978-3-642-20192-9}. \cr
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
#' Paths for Generalized Linear Models via Coordinate Descent. \emph{Journal of
#' Statistical Software}, 33(1), 1-22. URL \url{https://www.jstatsoft.org/v33/i01/}.
#' @export
getLassoLambda <- function(X, y, lambda_choice="1se", nfolds=10){
    stopifnot(is.character(lambda_choice))
    stopifnot(length(lambda_choice) == 1)
    stopifnot(!is.na(lambda_choice))
    stopifnot(lambda_choice %in% c("min", "1se"))

    stopifnot(is.matrix(X))
    stopifnot(is.numeric(X) | is.integer(X))
    n <- nrow(X)

    stopifnot(is.numeric(nfolds) | is.integer(nfolds))
    stopifnot(length(nfolds) == 1)
    stopifnot(nfolds == round(nfolds))
    stopifnot(nfolds > 3)

    # Since we are using the lasso, we require y to be a real-valued response
    # (unlike for the general cluster stability selection procedure, where y
    # can have a more general format as long as a suitable feature selection
    # function is provided by the user)
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(n == length(y))

    # Sample size to use: inflate n/2 by a factor of nfolds/(nfolds - 1),
    # so that each individual lasso fit is of size floor(n/2)

    n_sample <- min(round(n/2*nfolds/(nfolds - 1)), n)
    nfolds <- min(nfolds, n_sample)

    inds_size <- sample(1:n, n_sample)
    size_results <- glmnet::cv.glmnet(x=X[inds_size, ], y=y[inds_size],
        family="gaussian", nfolds=nfolds)

    lambda_ret <- size_results[[paste("lambda.", lambda_choice, sep="")]]

    # Check output
    stopifnot(length(lambda_ret) == 1)
    stopifnot(is.numeric(lambda_ret) | is.integer(lambda_ret))
    stopifnot(lambda_ret >= 0)

    return(lambda_ret)
}
