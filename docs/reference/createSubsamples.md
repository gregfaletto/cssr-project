# Creates lists of subsamples for stability selection.

Creates lists of subsamples for stability selection.

## Usage

``` r
createSubsamples(n, p, B, sampling_type, prop_feats_remove = 0)
```

## Arguments

- n:

  Integer or numeric; sample size of the data set.

- p:

  Integer or numeric; number of features.

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

- prop_feats_remove:

  Numeric; the proportion of features (between 0 and 1) that are dropped
  (and not provided to the feature selection method) on each subsample.
  Default is 0.

## Value

A list of length `B` (or `2*B` for `sampling_type = "SS"`). If
`prop_feats_remove = 0`, each list element is an integer vector of
length `floor(n/2)` containing the indices of a subsample of `1:n`. (For
`sampling_type=="SS"`, the last `B` subsamples will be complementary
pairs of the first `B` subsamples; see Faletto and Bien 2022 or Shah and
Samworth 2013 for details.) If `prop_feats_remove > 0`, each element is
a named list with members "subsample" (same as above) and
"feats_to_keep", a logical vector of length `p`;
`feats_to_keep[j] = TRUE` if feature `j` is chosen for this subsample,
and false otherwise.

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

Shah, R. D., & Samworth, R. J. (2013). Variable selection with error
control: Another look at stability selection. *Journal of the Royal
Statistical Society. Series B: Statistical Methodology*, 75(1), 55–80.
<https://doi.org/10.1109/RITA.2014.2302071>.

## Author

Gregory Faletto, Jacob Bien
