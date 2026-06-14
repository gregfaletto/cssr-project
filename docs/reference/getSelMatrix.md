# Generates matrix of selection indicators from stability selection.

Generates matrix of selection indicators from stability selection.

## Usage

``` r
getSelMatrix(
  x,
  y,
  lambda,
  B,
  sampling_type,
  subsamps_object,
  num_cores,
  fitfun = cssLasso
)
```

## Arguments

- x:

  an n x p numeric matrix or a data.frame containing the predictors.

- y:

  A response vector; can be any response that takes the form of a length
  n vector and is used (or not used) by fitfun. Typically (and for
  default fitfun = cssLasso), y should be an n-dimensional numeric
  vector containing the response.

- lambda:

  A tuning parameter or set of tuning parameters that may be used by the
  feature selection method `fitfun`. In the default case when
  `fitfun = cssLasso`, lambda should be a numeric: the penalty to use
  for each lasso fit. ([`css()`](css.md) does not require lambda to be
  any particular object because for a user-specified feature selection
  method `fitfun`, lambda can be an arbitrary object. See the
  description of `fitfun` below.)

- B:

  Integer or numeric; the number of subsamples. Note: For
  `sampling_type=="MB"` the total number of subsamples will be `B`; for
  `sampling_type="SS"` the number of subsamples will be `2*B`. Default
  is 100 for `sampling_type="MB"` and 50 for `sampling_type="SS"`.

- sampling_type:

  A character vector; either "SS" or "MB". For "MB", all B subsamples
  are drawn randomly (as proposed by Meinshausen and Bühlmann 2010). For
  "SS", in addition to these B subsamples, the B complementary pair
  subsamples will be drawn as well (see Faletto and Bien 2022 or Shah
  and Samworth 2013 for details). Default is "SS", and "MB" is not
  supported yet.

- subsamps_object:

  A list of length `B` (or `2*B` if `sampling_type="SS"`), where each
  element is one of the following:

  - `subsample`: An integer vector of size `n/2` containing the indices
    of the observations in the subsample.

  - `drop_var_input`: A named list containing two elements: one named
    "subsample", matching the previous description, and a logical vector
    named "feats_to_keep" containing the indices of the features to be
    automatically selected.

  (The first object is the output of the function
  [`createSubsamples()`](createSubsamples.md) when the provided
  `prop_feats_remove` is 0, the default, and the second object is the
  output of [`createSubsamples()`](createSubsamples.md) when
  `prop_feats_remove > 0`.)

- num_cores:

  Optional; an integer. If using parallel processing, the number of
  cores to use for parallel processing (num_cores will be supplied
  internally as the mc.cores argument of parallel::mclapply).

- fitfun:

  A function; the feature selection function used on each subsample by
  cluster stability selection. This can be any feature selection method;
  the only requirement is that it accepts the arguments (and only the
  arguments) `X`, `y`, and `lambda` and returns an integer vector that
  is a subset of `1:p`. For example, `fitfun` could be best subset
  selection or forward stepwise selection or LARS and `lambda` could be
  the desired model size; or `fitfun` could be the elastic net and
  `lambda` could be a length-two vector specifying lambda and alpha.
  Default is `cssLasso`, an implementation of lasso (relying on the R
  package `glmnet`), where `lambda` must be a positive numeric
  specifying the L1 penalty for the `lasso`.

## Value

A binary integer matrix of dimension `B` x `p` (if sampling_type ==
"MB") or `2*B` x `p` (if sampling_type == "SS"). `res[i, j]` = 1 if
feature j was selected on subsample i and equals 0 otherwise. (That is,
each row is a selected set.)

## Author

Gregory Faletto, Jacob Bien
