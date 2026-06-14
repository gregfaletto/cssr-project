# Get variance of noise to add to Z in order to yield proxies X with desired correlations with Z

Get variance of noise to add to Z in order to yield proxies X with
desired correlations with Z

## Usage

``` r
getNoiseVar(cor)
```

## Arguments

- cor:

  A numeric vector of desired correlations for each proxy to have
  with Z. Note: correlations must be positive.

## Value

A vector of variances of independent Gaussian random variables to add to
Z in order to yield proxies with the desired correlations with Z.

## Author

Gregory Faletto, Jacob Bien
