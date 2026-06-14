# Helper function to confirm that the new X matrix provided to getCssDesign or getCssPreds matches the characteristics of the X that was provided to css.

Helper function to confirm that the new X matrix provided to
getCssDesign or getCssPreds matches the characteristics of the X that
was provided to css.

## Usage

``` r
checkNewXProvided(trainX, css_results)
```

## Arguments

- trainX:

  A numeric matrix (preferably) or a data.frame (which will be coerced
  internally to a matrix by the function model.matrix). Must contain the
  same features (in the same number of columns) as the X matrix provided
  to css, and if the columns of trainX are labeled, the names must match
  the variable names provided to css. trainX may be omitted if
  train_inds were provided to css to set aside observations.

- css_results:

  An object of class "cssr" (the output of the function css).

## Value

A named list with the following elements:

- newX:

  If trainX was provided, this is the provided trainX matrix, coerced
  from a data.frame to a matrix if the provided trainX was a data.frame.
  If trainX was not provided, this is a matrix made up of the training
  indices provided to css in the train_inds argument.

- newXProvided:

  Logical; indicates whether a valid trainX input was provided.

## Author

Gregory Faletto, Jacob Bien
