# Provided fitfun implementing the lasso

Function used to select features with the lasso on each subsample in
cluster stability selection. Uses glmnet implementation of the lasso.

## Usage

``` r
cssLasso(X, y, lambda)
```

## Arguments

- X:

  A design matrix containing the predictors. (In practice this will be a
  subsample of the full design matrix provided to [`css()`](css.md).)

- y:

  A numeric vector containing the response.

- lambda:

  Numeric; a nonnegative number for the lasso penalty to use on each
  subsample. (For now, only one lambda value can be provided to
  `cssLasso()`; in the future, we plan to allow for multiple lambda
  values to be provided to `cssLasso()`, as described in Faletto and
  Bien 2022.)

## Value

An integer vector; the indices of the features selected by the lasso.

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
Paths for Generalized Linear Models via Coordinate Descent. *Journal of
Statistical Software*, 33(1), 1-22. URL
<https://www.jstatsoft.org/v33/i01/>.

## Author

Gregory Faletto, Jacob Bien
