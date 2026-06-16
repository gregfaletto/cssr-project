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

## Examples

``` r
# Noise variance needed so a proxy Z + N(0, v) attains each target
# correlation with Z (correlations must be positive).
getNoiseVar(c(0.9, 0.5, 1))
#> [1] 0.2345679 3.0000000 0.0000000
```
