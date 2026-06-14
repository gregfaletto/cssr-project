# Helper function run on each subsample

Runs provided feature selection method `fitfun` on each subsample for
cluster stability selection (this function is called within `mclapply`).

## Usage

``` r
cssLoop(input, x, y, lambda, fitfun)
```

## Arguments

- input:

  Could be one of two things:

  - `subsample`: An integer vector of size `n/2` containing the indices
    of the observations in the subsample.

  - `drop_var_input`: A named list containing two elements: one named
    "subsample" and the same as the previous description, and a logical
    vector named "feats_to_keep" containing the indices of the features
    to be automatically selected.

  (The first object is the output of the function
  [`createSubsamples()`](createSubsamples.md) when the provided
  `prop_feats_remove` is 0, the default, and the second object is the
  output of [`createSubsamples()`](createSubsamples.md) when
  `prop_feats_remove > 0`.)

- x:

  an n x p numeric matrix containing the predictors. (This should be the
  full design matrix provided to css.)

- y:

  A response; can be any response that takes the form of a length n
  vector and is used (or not used) by `fitfun`. Typically (and for
  default `fitfun = cssLasso`), `y` should be an n-dimensional numeric
  vector containing the response. This should be the full response
  provided to css.

- lambda:

  A tuning parameter or set of tuning parameters that may be used by the
  feature selection method. For example, in the default case when
  `fitfun = cssLasso`, `lambda` is a numeric: the penalty to use for
  each lasso fit.

- fitfun:

  A function that takes in arguments X, y, and lambda and returns a
  vector of indices of the columns of X (selected features).

## Value

An integer vector; the indices of the features selected by `fitfun`.

## Author

Gregory Faletto, Jacob Bien
