# Helper function to confirm that the inputs to `cssLasso()` are as expected.

Helper function to confirm that the inputs to
[`cssLasso()`](cssLasso.md) are as expected.

## Usage

``` r
checkCssLassoInputs(X, y, lambda)
```

## Arguments

- X:

  A design matrix containing the predictors. (Note that we don't need to
  check X very much, because X will have already been checked by the
  function [`checkCssInputs()`](checkCssInputs.md) when it was provided
  to [`css()`](css.md).)

- y:

  A numeric vector containing the response.

- lambda:

  Either a single nonnegative number for the lasso penalty to use on
  each subsample, or a named length-2 numeric vector
  `c(lambda = <value>, alpha = <value>)` bundling the penalty together
  with the elastic net mixing parameter alpha (which must be in
  `(0, 1]`). (For now, only one lambda value can be provided to
  [`cssLasso()`](cssLasso.md); in the future, we plan to allow for
  multiple lambda values to be provided to [`cssLasso()`](cssLasso.md),
  as described in Faletto and Bien 2022.)

## Author

Gregory Faletto, Jacob Bien
