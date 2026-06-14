# Helper function to confirm that the outputs of the provided feature selection method are as required.

Helper function to confirm that the outputs of the provided feature
selection method are as required.

## Usage

``` r
checkCssLoopOutput(selected, p, feats_on_subsamp)
```

## Arguments

- selected:

  An integer vector; the indices of the features selected by the lasso.

- p:

  The total number of observed features; all selected features must be
  in 1:p.

- feats_on_subsamp:

  Integer; the indices of the features considered by the feature
  selection method. All selected features must be among these features.

## Author

Gregory Faletto, Jacob Bien
