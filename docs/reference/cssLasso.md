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

  Either a single nonnegative number for the lasso penalty to use on
  each subsample (in which case a pure lasso fit, alpha = 1, is used),
  or a named length-2 numeric vector
  `c(lambda = <value>, alpha = <value>)` bundling the penalty together
  with the elastic net mixing parameter alpha (which must be in
  `(0, 1]`); in the latter case an elastic net fit with that alpha is
  used. (For now, only one lambda value can be provided to `cssLasso()`;
  in the future, we plan to allow for multiple lambda values to be
  provided to `cssLasso()`, as described in Faletto and Bien 2022.)

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

## Examples

``` r
set.seed(1)
data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
# cssLasso is the default base feature-selection method used by css();
# it returns the integer indices selected at the given lambda.
selected <- cssLasso(X = data$X, y = data$y, lambda = 0.01)
selected
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
