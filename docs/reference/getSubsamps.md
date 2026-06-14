# Generate list of subsamples

Generate list of subsamples

## Usage

``` r
getSubsamps(n, B, sampling_type)
```

## Arguments

- n:

  Integer or numeric; sample size of the data set.

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

## Value

A list of length `B` (or `2*B` for `sampling_type="SS"`), where each
element is an integer vector of length `floor(n/2)` containing the
indices of a subsample of `1:n`. For `sampling_type=="SS"`, the last `B`
subsamples will be complementary pairs of the first `B` subsamples (see
Faletto and Bien 2022 or Shah and Samworth 2013 for details).

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

Shah, R. D., & Samworth, R. J. (2013). Variable selection with error
control: Another look at stability selection. *Journal of the Royal
Statistical Society. Series B: Statistical Methodology*, 75(1), 55–80.
<https://doi.org/10.1109/RITA.2014.2302071>.

## Author

Gregory Faletto, Jacob Bien
