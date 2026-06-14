# Helper function to confirm that inputs to several functions are as expected, and modify inputs if needed

Helper function to confirm that inputs to several functions are as
expected, and modify inputs if needed

## Usage

``` r
checkXInputResults(newx, css_X)
```

## Arguments

- newx:

  A numeric matrix (preferably) or a data.frame (which will be coerced
  internally to a matrix by the function model.matrix) containing the
  data that will be used to generate the design matrix of cluster
  representatives. Must contain the same features (in the same number of
  columns) as the X matrix provided to css, and if the columns of newX
  are labeled, the names must match the variable names provided to css.

- css_X:

  The X matrix provided to css, as in the output of the css function
  (after having been coerced from a data.frame to a matrix by css if
  needed).

## Value

A named list with the following elements.

- feat_names:

  A character vector containing the column names of newx (if the
  provided newx had column names). If the provided newx did not have
  column names, feat_names will be NA.

- newx:

  The provided newx matrix, coerced from a data.frame to a matrix if the
  provided newx was a data.frame.

## Author

Gregory Faletto, Jacob Bien
