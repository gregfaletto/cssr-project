# Helper function to check that the inputs to formCssDesign are as expected

Helper function to check that the inputs to formCssDesign are as
expected

## Usage

``` r
checkFormCssDesignInputs(
  css_results,
  weighting,
  cutoff,
  min_num_clusts,
  max_num_clusts,
  newx
)
```

## Arguments

- css_results:

  An object of class "cssr" (the output of the function css).

- weighting:

  Character; determines how to calculate the weights to combine features
  from the selected clusters into weighted averages, called cluster
  representatives. Must be one of "sparse", "weighted_avg", or
  "simple_avg'.

- cutoff:

  Numeric; css will return only those clusters with selection
  proportions equal to at least cutoff. Must be between 0 and 1.

- min_num_clusts:

  Integer or numeric; the minimum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns fewer than
  min_num_clusts clusters, the cutoff will be increased until at least
  min_num_clusts clusters are selected.)

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns more than
  max_num_clusts clusters, the cutoff will be decreased until at most
  max_num_clusts clusters are selected.)

- newx:

  A numeric matrix (preferably) or a data.frame (which will be coerced
  internally to a matrix by the function model.matrix) containing the
  data that will be used to generate the design matrix of cluster
  representatives. Must contain the same features (in the same number of
  columns) as the X matrix provided to css, and if the columns of newx
  are labeled, the names must match the variable names provided to css.
  newx may be omitted if train_inds were provided to css to set aside
  observations for model estimation. If this is the case, then when newx
  is omitted formCssDesign will return a design matrix of cluster
  representatives formed from the train_inds observations from the
  matrix X provided to css. (If no train_inds were provided to css, newX
  must be provided to formCssDesign.)

## Value

A named list with the following elements:

- newx:

  If newx was provided, the provided newx matrix, coerced from a
  data.frame to a matrix if needed. If newx was not provided, a matrix
  formed by the train_inds set aside in the original function call to
  css.

- max_num_clusts:

  The provided max_num_clusts, coerced to an integer if needed, and
  coerced to be less than or equal to the total number of clusters.

## Author

Gregory Faletto, Jacob Bien
