# Print cluster stability selection output

Print a summary of the information from the css function (using output
from printCssDf function).

## Usage

``` r
# S3 method for class 'cssr'
print(x, cutoff = 0, min_num_clusts = 1, max_num_clusts = NA, ...)
```

## Arguments

- x:

  An object of class "cssr" (the output of the function css).

- cutoff:

  Numeric; print.cssr will display only those clusters with selection
  proportions equal to at least cutoff. Must be between 0 and 1. Default
  is 0 (in which case either all clusters are displayed, or
  max_num_clusts are, if max_num_clusts is specified).

- min_num_clusts:

  Integer or numeric; the minimum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns fewer than
  min_num_clusts clusters, the cutoff will be lowered until at least
  min_num_clusts clusters are selected.) Default is 1.

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns more than
  max_num_clusts clusters, the cutoff will be raised until at most
  max_num_clusts clusters are selected.) Default is NA (in which case
  max_num_clusts is ignored). Because clusters can have tied selection
  proportions, ties at the threshold can cause more than max_num_clusts
  (or fewer than min_num_clusts) clusters to be returned; when the two
  constraints conflict, max_num_clusts takes precedence.

- ...:

  Additional arguments to generic print.data.frame function

## Value

Invisibly, the unchanged `cssr` object `x` (following the standard
convention for `print` methods). Called for its side effect: printing a
summary table with one row per cluster, arranged in decreasing order of
cluster selection proportion from top to bottom. The printed columns are
ClustName (the name of the cluster that was either provided to css or
made by css if no name was provided); ClustProtoName (the name of the
selection prototype from the cluster, which is the feature with the
greatest individual selection proportion among all the cluster members,
with ties broken by choosing the feature with the highest correlation
with the response if the response is real-valued; only shown if the
features are named); ClustProtoNum (the column number of the prototype
in the X matrix provided to css); ClustSelProp (the cluster's selection
proportion); and ClustSize (the size of the cluster).

## Author

Gregory Faletto, Jacob Bien
