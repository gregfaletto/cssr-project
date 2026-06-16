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

A length-one numeric vector; the absolute value of the sample
correlation between t and y (between 0 and 1), or 0 if either t or y has
only one unique value.

## Author

Gregory Faletto, Jacob Bien
