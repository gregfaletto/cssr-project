# Absolute value of sample correlation between two vectors

Calculates the absolute value of correlation of t and y. If either input
has only one unique value, returns 0 by definition.

## Usage

``` r
corFunction(t, y)
```

## Arguments

- t:

  A numeric or integer vector.

- y:

  A numeric or integer vector; must have the same length as t.

## Value

A numeric vector of the same length as cluster_i containing the weights
corresponding to each of the features in cluster_i. The weights will all
be nonnegative and sum to 1.

## Author

Gregory Faletto, Jacob Bien
