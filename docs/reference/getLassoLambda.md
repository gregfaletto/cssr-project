# Get lambda value for lasso

Chooses a lambda value for the lasso used on a subsample of size n/2 (as
in cluster stability selection) by cross-validation.

## Usage

``` r
getLassoLambda(X, y, lambda_choice = "1se", nfolds = 10, alpha = 1)
```

## Arguments

- X:

  An n x p numeric matrix (preferably) or a data.frame (which will be
  coerced internally to a matrix by the function model.matrix)
  containing the p \>= 2 features/predictors that will be used by
  cluster stability selection.

- y:

  The response; an n-dimensional numeric or integer vector. (Unlike in
  the more general css setup, this response must be real-valued since
  lambda will be determined using the lasso with cross-validation.)

- lambda_choice:

  Character; either "min" or "1se". If "min", chooses the lambda that
  minimizes the cross-validated error; if "1se", chooses the largest
  lambda within one standard error of the minimum error lambda
  (resulting in a smaller selected set, which may be desirable because
  the model size corresponding to the minimum error lambda tends to be
  larger than optimal. See, for example, Bühlmann and Meinshausen 2006,
  Prop. 1 and Bühlmann and van de Geer 2011, Section 2.5.1.). Default is
  "1se".

- nfolds:

  Numeric or integer; the number of folds for cross-validation. Must be
  at least 4 (as specified by cv.glmnet). Default is 10.

- alpha:

  Numeric; the elastic net mixing parameter. Default is 1 (in which case
  the penalty is for lasso)

## Value

A numeric; the selected value of lambda.

## References

Bühlmann, P., & Meinshausen, N. (2006). High-Dimensional Graphs and
Variable Selection With the Lasso. *The Annals of Statistics*, 34(3),
1436–1462. <https://doi.org/10.1214/009053606000000281>.  
Peter Bühlmann and Sara van de Geer. Statistics for High-Dimensional
Data. *Springer Series in Statistics*. Springer, Heidelberg, 2011. ISBN
978-3-642-20191-2. <http://dx.doi.org/10.1007/978-3-642-20192-9>.  
Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
Paths for Generalized Linear Models via Coordinate Descent. *Journal of
Statistical Software*, 33(1), 1-22. URL
<https://www.jstatsoft.org/v33/i01/>.

## Author

Gregory Faletto, Jacob Bien
