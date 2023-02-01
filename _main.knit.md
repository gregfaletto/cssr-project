---
title: "Creating the `cssr` R package"
author: "Gregory Faletto"
date: "February 01, 2023"
site: bookdown::bookdown_site
knit: litr::render
output: litr::litr_gitbook
params:
  package_name: "cssr" # <-- change this to your package name
  package_parent_dir: "." # <-- relative to this file's location
documentclass: book
---

<!-- This Rmd file contains all the code needed to define an R package.  Press "Knit" in RStudio or more generally run `rmarkdown::render("name-of-this-file.Rmd")` to generate the R package.  Remember that when you want to modify anything about the R package, you should modify this document rather than the package that is outputted.
-->

# Introduction

Cluster stability selection is a feature selection method designed to allow stability selection to work effectively in the presence of highly correlated features.  It was proposed in [this paper](https://arxiv.org/abs/2201.00494):


```r
###"faletto2022"###
#' Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
```

This bookdown uses [literate programming](https://jacobbien.github.io/litr-project/) to define the `cssr` R package, which implements the procedures described in the paper.  For a light introduction demonstrating how to use `cssr`, please see the package's vignette [here]().

TODO: CREATE VIGNETTE FOR PACKAGE AND PKGDOWN SITE FOR PACKAGE AND THEN LINK IN THE ABOVE TO THE VIGNETTE THAT APPEARS IN THE PKGDOWN SITE.

# Overview

## Background

### Complementary pairs stability selection {#cpss}

The following paper introduces complementary pairs subsampling:


```r
###"shah2013"###
#' Shah, R. D., & Samworth, R. J.
#' (2013). Variable selection with error control: Another look at stability
#' selection. \emph{Journal of the Royal Statistical Society. Series B:
#' Statistical Methodology}, 75(1), 55–80.
#' \url{https://doi.org/10.1109/RITA.2014.2302071}.
```

Subsamples \(A_1, \ldots, A_B \subset [n]\) of size \(\lfloor n/2 \rfloor\) are drawn, as well as subsamples \( \overline{A}_b \subset [n] \) of the same size with \(A_b \cap \overline{A}_b = \emptyset\).  Our function `getSubsamps()` implements this subsampling.

Writing $\hat{S}^\lambda(A)$ for the selected features from applying a variable selection procedure with tuning parameter $\lambda$ on the data in a subset $A\subset[n]$, Shah and Samworth suggest computing the following:

$$
\hat{\Pi}_B^{\text{(SS)}}(j) = \frac{1}{2B} \sum_{b=1}^B \left[1\left\{j \in \hat{S}^\lambda \left( A_b \right)\right\} + 1\left\{j \in \hat{S}^\lambda (\overline{A}_b)\right\} \right].
$$

### Cluster stability selection

We compute the proportion of the time that a feature \(j \in [p]\) is selected for at least one \(\lambda \in \Lambda\):
$$
\hat{\Pi}_B(j) := \frac{1}{2B} \sum_{b=1}^B \left[ 1\left\{   j \in \bigcup_{\lambda \in \Lambda} \hat{S}^\lambda \left( A_b \right) \right\} + 1\left\{  j \in  \bigcup_{\lambda \in \Lambda} \hat{S}^\lambda (\overline{A}_b)\right\} \right].
$$

We also compute a similar quantity at the level of clusters: For every cluster $C_k$, we calculate the proportion of the time that at least one feature from \(C_k\) is selected for at least one \(\lambda \in \Lambda\):

$$
\hat{\Theta}_B(C_k) :=  \frac{1}{2B} \sum_{b=1}^B \left[  1 \left\{    C_k \cap \bigcup_{\lambda \in \Lambda} \hat{S}^{\lambda}\left(A_b \right) \neq \emptyset   \right\} +  1 \left\{   C_k \cap \bigcup_{\lambda \in \Lambda} \hat{S}^{\lambda}\left(\overline{A}_b \right) \neq \emptyset  \right\} \right].
$$

We then calculate *cluster representatives* \(\boldsymbol{X}_{\cdot C_k}^{\text{rep}}\) as weighted averages of the cluster members:

$$
\boldsymbol{X}_{\cdot C_k}^{\text{rep}}:=  \sum_{j \in C_k } w_{kj} \boldsymbol{X}_{\cdot j}.
$$
We allow for several choices of weights:

- **Weighted averaged cluster stability selection:**

$$
w_{kj} = \frac{\hat{\Pi}_B(j)}{\sum_{j' \in C_k} \hat{\Pi}_B(j')} \qquad \forall j \in C_k  .
$$

- **Simple averaged cluster stability selection:**  

$$
w_{kj} = \frac{1}{\left| C_k \right|}  \qquad \forall j \in C_k .
$$

- **Sparse cluster stability selection:** For each \(j \in C_k \), 

$$
w_{kj} = \left. 1 \left\{ j \in \underset{j' \in C_k}{\arg \max}  \left\{ \hat{\Pi}_B(j') \right\} \right\} \middle/ \left| \underset{j' \in C_k}{\arg \max}  \left\{ \hat{\Pi}_B(j')  \right\} \right| \right. .
$$

## Outline

We will define the functions in this package in steps.

* First we define [the workhorse function of the package](#css), `css()`. This function takes as inputs a data set ($X$, $y$), a feature selection method $\hat S^\lambda(\cdot)$, and clusters of features appearing in the data: $C_1,\ldots,C_K$. It calculates random subsamples of the data ($A_1,\ldots, A_B$ and $\bar A_1,\ldots, \bar A_B$) and gets selected sets of features on each subsample (i.e., $\hat S^\lambda(A_b)$ and $\hat S^\lambda(\bar A_b)$). It returns a $2B\times p$ matrix of indicator variables for whether each feature was selected on each subsample (one row for each subsample, one column for each feature). It returns a similar $2B\times K$ matrix of indicator variables for whether any feature from each cluster was selected on each subsample. This is the most computationally intensive step of cluster stability selection, so it is isolated within its own function that is designed to be run only once on a data set.
* Next we [define functions](#sel-and-pred) that take these indicator matrices and return useful output--for example, selected sets of features, or predictions on test data. These outputs depend on the selected features, which themselves depend on user-selected parameters (for example, the cutoff for selection proportions for selected clusters). 
* Next, we define [wrapper functions](#wrapper-functions) that compute all of the above in one step in a user-friendly way. (These functions are not recommended for large data sets or for "power users" who may want to call these functions multiple times on the same data set, because these wrapper functions make a new call to `css()` every time they are called, which is slow and computationally wasteful if the inputs to `css()` are not changed.)
* Finally, we define [some other useful functions](#other-useful), like functions that generate clustered data to use for simulations or testing, or select a tuning parameter for the lasso via cross-validation.


## Package setup

Before proceeding to the actual functionality of the package, we start by specifying the information needed in the DESCRIPTION file of the R package.


```r
usethis::create_package(
  path = ".",
  fields = list(
    Package = params$package_name,
    Version = "0.1.1",
    Title = "Cluster Stability Selection",
    Description = "Implementation of Cluster Stability Selection (Faletto and Bien 2022).",
    `Authors@R` = c(person(
      given = "Gregory",
      family = "Faletto",
      email = "gregory.faletto@marshall.usc.edu",
      role = c("aut", "cre")
      ), person(
      given = "Jacob",
      family = "Bien",
      email = "jbien@usc.edu",
      role = c("aut")
      ))
  )
)
usethis::use_mit_license(copyright_holder = "F. Last")
```

We also define the package-level documentation that shows up when someone types `package?cssr` in the console:


```package_doc
#' Cluster Stability Selection
#'
#' DESCRIBE PACKAGE HERE AND PERHAPS POINT USER TO PKGDOWN WEBSITE ONCE 
#' IT EXISTS.
#' 
#' @docType package
#' @seealso \code{\link{css}}
```

# The core function: `css` {#css}

The main inputs that `css()` accepts are the following:

* a design matrix `X`
* a response `y`
* a selection method `fitfun` with specified tuning parameter `lambda`.  This specifies $\hat{S}^\lambda(A)$.  In particular:


```r
###"param-fitfun"###
#' @param fitfun A function; the feature selection function used on each
#' subsample by cluster stability selection. This can be any feature selection
#' method; the only requirement is that it accepts the arguments (and only the
#' arguments) `X`, `y`, and `lambda` and returns an integer vector that is a 
#' subset of `1:p`. For example, `fitfun` could be best subset selection or 
#' forward stepwise selection or LARS and `lambda` could be the desired model 
#' size; or `fitfun` could be the elastic net and `lambda` could be a length-two
#' vector specifying lambda and alpha. Default is `cssLasso`, an implementation 
#' of lasso (relying on the R package `glmnet`), where `lambda` must be a 
#' positive numeric specifying the L1 penalty for the `lasso`.
```

and


```r
###"param-lambda"###
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method `fitfun`. In the default case when
#' `fitfun = cssLasso`, lambda should be a numeric: the penalty to use for each
#' lasso fit. (`css()` does not require lambda to be any particular object because
#' for a user-specified feature selection method `fitfun`, lambda can be an
#' arbitrary object. See the description of `fitfun` below.)
```

* a specification of which features belong together in highly correlated clusters. This specifies the clusters $C_1,\ldots, C_K$. In particular:


```r
###"param-clusters"###
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of `1:p`). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves). If
#' clusters is a list of length 0 (or a list only containing clusters of length
#' 1), then `css()` returns the same results as stability selection (so the
#' returned `feat_sel_mat` will be identical to `clus_sel_mat`). Names for the
#' clusters will be needed later; any clusters that are not given names in the
#' provided list will be given names automatically by `css()`. Default is 
#' `list()` (so no clusters are specified).
```

* and a specification of the type of subsamples.  For now we only consider the [complementary subsets sampling of Shah and Samworth](#cpss).  However, we have included an option for Meinshausen-Buhlmann sampling which we may add later:


```r
###"param-sampling_type"###
#' @param sampling_type A character vector; either "SS" or "MB". For "MB",
#' all B subsamples are drawn randomly (as proposed by Meinshausen and Bühlmann
#' 2010). For "SS", in addition to these B subsamples, the B complementary pair
#' subsamples will be drawn as well (see Faletto and Bien 2022 or Shah and
#' Samworth 2013 for details). Default is "SS", and "MB" is not supported yet.
```

The number of subsamples is also up to the user:


```r
###"param-B"###
#' @param B Integer or numeric; the number of subsamples. Note: For
#' `sampling_type=="MB"` the total number of subsamples will be `B`; for
#' `sampling_type="SS"` the number of subsamples will be `2*B`. Default is 100
#' for `sampling_type="MB"` and 50 for `sampling_type="SS"`.
```


In this section, we will build the function `css()` step-by-step.

* First, `css()` creates random subsamples of the data: $A_1,\ldots, A_B$ (and $\bar A_1,\ldots, \bar A_B$ for Shah-Samworth subsampling). This is implemented in the function `createSubsamples()`.

* Next, `css()` executes the specified feature selection method $\hat S^\lambda(\cdot)$ on each subsample. This generates a binary matrix, `feat_sel_mat`, with one row for each subsample and one column for each feature in `X`. In each row, the entry corresponding to each feature equals 1 if the feature was selected on that subsample and 0 otherwise. `feat_sel_mat` is one of the outputs of `css()`; it is used to calculate the selection proportions for individual features. This is implemented in the function `getSelMatrix()`.
* So far this is the same as the original stability selection procedure. Now we take into account the clusters. The function `getClusterSelMatrix()` takes in the formatted clusters as well as `feat_sel_mat` and generates the binary matrix `clus_sel_mat`, which contains selection indicators for each cluster. `clus_sel_mat` has the same number of rows as `feat_sel_mat` and one column for each cluster rather than one for each feature. This is used to calculate the cluster selection proportions.

The function `css()` is specified below. (There are some extra details that I omitted in the above description for brevity; you can read the full details below.)


```r
#' Cluster Stability Selection
#'
#' Executes cluster stability selection algorithm. Takes subsamples of data,
#' executes feature selection algorithm on each subsample, and returns matrices
#' of feature selection indicators as well as cluster selection indicators.
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors.
#' @param y The response; can be anything that takes the form of an
#' n-dimensional vector, with the ith entry corresponding to the ith row of X.
#' Typically (and for default `fitfun = cssLasso`), `y` should be an n-dimensional
#' numeric vector.
<<param-lambda>>
<<param-clusters>>
<<param-fitfun>>
<<param-sampling_type>>
<<param-B>>
#' @param prop_feats_remove Numeric; if `prop_feats_remove` is greater than 0,
#' then on each subsample, each feature is randomly dropped from the design
#' matrix that is provided to `fitfun` with probability `prop_feats_remove`
#' (independently across features). That is, in a typical subsample,
#' `prop_feats_remove*p` features will be dropped (though this number will vary).
#' This is similar in spirit (but distinct from) extended stability selection
#' (Beinrucker et. al. 2016); see their paper for some of the benefits of
#' dropping features (besides increasing computational speed and decreasing
#' memory requirements). For `sampling_type="SS"`, the features dropped in
#' each complementary pair of subsamples are identical in order to ensure that
#' the theoretical guarantees of Faletto and Bien (2022) are retained within
#' each individual pair of subsamples. (Note that this feature is not
#' investigated either theoretically or in simulations by Faletto and Bien
#' 2022). Must be between 0 and 1. Default is 0.
#' @param train_inds Optional; an integer or numeric vector containing the
#' indices of observations in `X` and `y` to set aside for model training by the
#' function `getCssPreds()` after feature selection. (This will only work if `y` is
#' real-valued, because `getCssPreds()` using ordinary least squares regression to
#' generate predictions.) If `train_inds` is not provided, all of the observations
#' in the provided data set will be used for feature selection.
#' @param num_cores Optional; an integer. If using parallel processing, the
#' number of cores to use for parallel processing (`num_cores` will be supplied
#' internally as the `mc.cores` argument of `parallel::mclapply()`).
#' @return A list containing the following items:
#' \item{`feat_sel_mat`}{A `B` (or `2*B` for `sampling_type="SS"`) x `p` numeric (binary) matrix. `feat_sel_mat[i, j] = 1` if feature `j` was selected by the base feature selection method on subsample `i`, and 0 otherwise.}
#' \item{`clus_sel_mat`}{A `B` (or `2*B` for SS sampling) x `length(clusters)` numeric (binary) matrix. `clus_sel_mat[i, j] = 1` if at least one feature from cluster j was selected by the base feature selection method on subsample `i`, and 0 otherwise.}
#' \item{`X`}{The `X` matrix provided to `css()`, coerced from a data.frame to a 
#' matrix if needed.}
#' \item{`y`}{The `y` vector provided to `css()`.}
#' \item{`clusters`}{A named list of integer vectors containing all of the clusters provided to `css()`, as well as size 1 clusters of any features not listed in any
#' of the clusters provided to `css()`. All clusters will have names; any 
#' clusters not provided with a name in the input to `css()` will be given names
#' automatically by `css()` (of the form c1, etc.).}
#' \item{`train_inds`}{Identical to the `train_inds` provided to `css()`.}
#' @author Gregory Faletto, Jacob Bien
#' @references
<<faletto2022>>
#' 
<<shah2013>>
#' 
#' Meinshausen, N., & Bühlmann, P. (2010). Stability Selection. \emph{Journal of the Royal
#' Statistical Society. Series B: Statistical Methodology}, 72(4), 417–473.
#' \url{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-9868.2010.00740.x}.
#' 
#' Beinrucker, A., Dogan, Ü., &
#' Blanchard, G. (2016). Extensions of stability selection using subsamples of
#' observations and covariates. \emph{Statistics and Computing}, 26(5), 1059-
#' 1077. \url{https://doi.org/10.1007/s11222-015-9589-y}.
#' 
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized
#' Linear Models via Coordinate Descent. \emph{Journal of Statistical Software},
#' 33(1), 1-22. URL \url{https://www.jstatsoft.org/v33/i01/}.
#' @export
css <- function(X, y, lambda, clusters = list(), fitfun = cssLasso,
    sampling_type = "SS", B = ifelse(sampling_type == "MB", 100L, 50L),
    prop_feats_remove = 0, train_inds = integer(), num_cores = 1L
    ){

    # Check inputs

    check_list <- checkCssInputs(X, y, lambda, clusters, fitfun, sampling_type,
        B, prop_feats_remove, train_inds, num_cores)

    feat_names <- check_list$feat_names
    X <- check_list$X
    clusters <- check_list$clusters

    rm(check_list)

    n <- nrow(X)
    p <- ncol(X)

    train_inds <- as.integer(train_inds)

    ### Create subsamples

    sel_inds <- setdiff(1:n, train_inds)
    n_sel <- length(sel_inds)
    if(n_sel < 4){
        stop("Too many training indices provided (must be at least 4 observations left for feature selection, and ideally many more)")
    }

    subsamps_object <- createSubsamples(n_sel, p, B, sampling_type,
        prop_feats_remove)

    ### Get matrix of selected feature sets from subsamples

    stopifnot(!is.matrix(y))

    feat_sel_mat <- getSelMatrix(X[sel_inds, ], y[sel_inds], lambda, B,
        sampling_type, subsamps_object, num_cores, fitfun)

    if(any(!is.na(feat_names))){
        colnames(feat_sel_mat) <- feat_names
        colnames(X) <- feat_names
    }

    ### Get selection proportions for clusters corresponding to each feature

    clus_sel_mat <- getClusterSelMatrix(clusters, feat_sel_mat)

    # Check outputs
    stopifnot(!is.null(colnames(clus_sel_mat)))
    stopifnot(all(colnames(clus_sel_mat) == names(clusters)))

    ret <- list(feat_sel_mat = feat_sel_mat,
        clus_sel_mat = clus_sel_mat,
        X = X,
        y = y,
        clusters = clusters,
        train_inds = train_inds
        )

    class(ret) <- "cssr"

    return(ret)
}
```

Tests for `css()` and the other functions in this section are located at the end of this document. Next, we'll build the individual functions one at a time. (The one function above that I didn't mention at all is `checkCssInputs()`, which confirms that the inputs to `css()` are as expected and does some basic formatting on the inputs. To streamline presentation, this and other helper functions are specified later.)

## Main components of `css`

### Generating subsamples

The function `createSubsamples()` is responsible for generating the subsamples $A_b$, where $b$ ranges from 1 to $B$ or 1 to $2B$ depending on the type of subsampling:


```r
#' Creates lists of subsamples for stability selection.
#'
#' @param n Integer or numeric; sample size of the data set.
#' @param p Integer or numeric; number of features.
<<param-B>>
<<param-sampling_type>>
#' @param num_feats_remove Integer; number of features select automatically on
#' every iteration. Determined earlier from input prop_feats_remove to css.
#' @return A list of length `B` (or `2*B` for `sampling_type = "SS"`). If
#' `prop_feats_remove = 0`, each list element is an integer vector of length
#' `floor(n/2)` containing the indices of a subsample of `1:n`. (For
#' `sampling_type=="SS"`, the last `B` subsamples will be complementary pairs of
#' the first `B` subsamples; see Faletto and Bien 2022 or Shah and Samworth 2013
#' for details.) If `prop_feats_remove > 0`, each element is a named list with
#' members "subsample" (same as above) and "feats_to_keep", a logical vector
#' of length `p`; `feats_to_keep[j] = TRUE` if feature `j` is chosen for this
#' subsample, and false otherwise.
#' @author Gregory Faletto, Jacob Bien
#' @references
<<faletto2022>>
#' 
<<shah2013>>
createSubsamples <- function(n, p, B, sampling_type, prop_feats_remove=0){

    # Check inputs

    stopifnot(length(n) == 1)
    stopifnot(is.numeric(n) | is.integer(n))
    stopifnot(n == round(n))
    stopifnot(n > 0)

    stopifnot(length(p) == 1)
    stopifnot(is.numeric(p) | is.integer(p))
    stopifnot(p == round(p))
    stopifnot(p > 0)

    checkSamplingType(sampling_type)
    checkPropFeatsRemove(prop_feats_remove, p)

    if(prop_feats_remove == 0){
        subsamples <- getSubsamps(n, B, sampling_type)
        return(subsamples)
    } else{
        # In this case, we generate subsamples as well as logical vectors
        # of features to keep
        subsamps_and_feats <- list()
        subsamples <- getSubsamps(n, B, sampling_type)
        for(i in 1:B){
            # Logical p-vector, where each entry is TRUE with probability
            # 1 - prop_feats_remove
            feats_to_keep_i <- as.logical(stats::rbinom(n=p, size=1,
                prob=1 - prop_feats_remove))
            # Make sure at least two entries are equal to TRUE (so that at
            # least two features are present for every subsample)--if not,
            # randomly choose features to add
            while(sum(feats_to_keep_i) < 2){
                false_inds <- which(!feats_to_keep_i)
                sel_feat <- sample(false_inds, size=1)
                feats_to_keep_i[sel_feat] <- TRUE
            }
            subsamps_and_feats[[i]] <- list(subsample=subsamples[[i]],
                feats_to_keep=feats_to_keep_i)
        }

        if(sampling_type=="SS"){
            stopifnot(length(subsamples) == 2*B)
            for(i in 1:B){
                # Keep the same features as in the other subsample (this
                # ensures that the theoretical guarantee of Shah and Samworth
                # 2013 remains valid on every individual pair of subsamples)
                subsamps_and_feats[[B + i]] <- list(subsample=subsamples[[B + i]],
                    feats_to_keep=subsamps_and_feats[[i]]$feats_to_keep)
            }
        }

        # Check output
        stopifnot(all(names(subsamps_and_feats) == c("subsample",
            "feats_to_keep")))

        return(subsamps_and_feats)
    }
    # Shouldn't be possible to reach this part of function
    stop("createSubsamples failed to return anything")
}
```

Notice that `createSubsamples()` calls some helper functions to check the inputs (again, these are specified later in the helper functions section). `createSubsamples()` also calls the workhorse function `getSubsamps()` to generate a list of subsamples.


```r
#' Generate list of subsamples
#'
#` Generate list of `B` (or `2*B` for sampling_type="SS") subsamples of size
#` `n/2`
#' @param n Integer or numeric; sample size of the data set.
<<param-B>>
<<param-sampling_type>>
#' @return A list of length `B` (or `2*B` for `sampling_type="SS"`), where each
#' element is an integer vector of length `floor(n/2)` containing the indices
#' of a subsample of `1:n`. For `sampling_type=="SS"`, the last `B` subsamples
#' will be complementary pairs of the first `B` subsamples (see Faletto and
#' Bien 2022 or Shah and Samworth 2013 for details).
#' @author Gregory Faletto, Jacob Bien
#' @references
#' 
<<faletto2022>>
#' 
<<shah2013>>
getSubsamps <- function(n, B, sampling_type){
    subsamples <- list()
    for(i in 1:B){
        subsamples[[i]] <- sort(sample.int(n=n, size=floor(n/2), replace=FALSE))
    }
    stopifnot(length(subsamples) == B)
    # TODO(gregfaletto): add support for sampling_type="MB"
    if(sampling_type=="SS"){
        for(i in 1:B){
            # For the ith entry, take a subsample of size floor(n/2) from the
            # remaining n - floor(n/2) observations. (Only necessary to actually
            # take the subsample if n is odd; if not, the set we want is
            # setdiff(1:n, subsamples[[i]]), so skip the sample function.)
            if(n/2 == floor(n/2)){
                subsamples[[B + i]] <- sort(setdiff(1:n, subsamples[[i]]))
            } else{
                subsamples[[B + i]] <- sort(sample(x=setdiff(1:n,
                    subsamples[[i]]), size=floor(n/2), replace=FALSE))
            }

            # Check output

            stopifnot(is.integer(subsamples[[B + i]]))
            stopifnot(all(subsamples[[B + i]] ==
                round(subsamples[[B + i]])))
            stopifnot(floor(n/2) == length(subsamples[[B + i]]))
            stopifnot(length(subsamples[[B + i]]) ==
                length(unique(subsamples[[B + i]])))
        }
        stopifnot(length(subsamples) == 2*B)
    }
    return(subsamples)
}
```

### Forming selection matrices

The next function called in the body of `css()` is `getSelMatrix()`, which records for each feature $j$ and subsample $b$ whether $j\in \hat S^\lambda(A_b)$:


```r
#' Generates matrix of selection indicators from stability selection.
#'
#' @param x an n x p numeric matrix or a data.frame containing the predictors.
#' @param y A response vector; can be any response that takes the form of a
#' length n vector and is used (or not used) by fitfun. Typically (and for
#' default fitfun = cssLasso), y should be an n-dimensional numeric vector
#' containing the response.
<<param-lambda>>
<<param-B>>
<<param-sampling_type>>
#' @param subsamps_object A list of length `B` (or `2*B` if `sampling_type="SS"`),
#' where each element is one of the following: \item{subsample}{An integer
#' vector of size `n/2` containing the indices of the observations in the
#' subsample.} \item{drop_var_input}{A named list containing two elements: one
#' named "subsample", matching the previous description, and a logical vector
#' named "feats_to_keep" containing the indices of the features to be
#' automatically selected.} (The first object is the output of the function
#' createSubsamples when the provided prop_feats_remove is 0, the default, and
#' the second object is the output of createSubsamples when prop_feats_remove >
#' 0.)
#' @param num_cores Optional; an integer. If using parallel processing, the
#' number of cores to use for parallel processing (num_cores will be supplied
#' internally as the mc.cores argument of parallel::mclapply).
<<param-fitfun>>
#' @return A binary integer matrix of dimension `B` x `p` (if sampling_type ==
#' "MB") or `2*B` x `p` (if sampling_type == "SS"). res[i, j] = 1 if feature j
#' was selected on subsample i and equals 0 otherwise. (That is, each row is a
#' selected set.)
#' @author Gregory Faletto, Jacob Bien
getSelMatrix <- function(x, y, lambda, B, sampling_type, subsamps_object,
    num_cores, fitfun=cssLasso){

    # Check inputs

    stopifnot(is.matrix(x))
    stopifnot(all(!is.na(x)))

    n <- nrow(x)
    p <- ncol(x)

    stopifnot(length(y) == n)
    stopifnot(!is.matrix(y))
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    checkSamplingType(sampling_type)

    # Get list of selected feature sets from subsamples

    res_list <- parallel::mclapply(X=subsamps_object, FUN=cssLoop, x=x, y=y,
        lambda=lambda, fitfun=fitfun, mc.cores=num_cores)

    # Store selected sets in B x p (or `2*B` x p for "SS") binary matrix
    if(sampling_type=="SS"){
        res <- matrix(0L, 2*B, p)
    } else if(sampling_type=="MB"){
        res <- matrix(0L, B, p)
    } else{
        stop("!(sampling_type %in% c(SS, MB)) (don't know how to set dimensions of res")
    }

    stopifnot(length(res_list) == nrow(res))

    # Get selected sets into matrix res

    for(i in 1:nrow(res)){
        if(length(res_list[[i]]) == 0){
            # If no features are selected, don't fill in anything in this row
            next
        }

        if(!is.integer(res_list[[i]])){
            print(paste("failed on iteration", i))
            print(res_list[[i]])
            stop("Something seems to be wrong with the feature selection method (fitfun failed to return an integer vector)")
        }
        stopifnot(length(res_list[[i]]) <= p & length(res_list[[i]]) > 0)
        stopifnot(all(!is.na(res_list[[i]])))
        stopifnot(max(res_list[[i]]) <= p)
        stopifnot(min(res_list[[i]]) >= 1)
        stopifnot(length(res_list[[i]]) == length(unique(res_list[[i]])))
        stopifnot(!("try-error" %in% class(res_list[[i]]) |
            "error" %in% class(res_list[[i]]) |
            "simpleError" %in% class(res_list[[i]]) |
            "condition" %in% class(res_list[[i]])))

        # Store selected variables in the ith row of res
        res[i, res_list[[i]]] <- 1L
    }

    # Check output

    stopifnot(is.matrix(res))
    if(sampling_type=="SS"){
        stopifnot(nrow(res) == 2*B)
    } else{
        stopifnot(nrow(res) == B)
    }
    stopifnot(ncol(res) == p)
    stopifnot(all(res %in% c(0, 1)))

    return(res)
}
```

Again, `getSelMatrix()` uses some helper functions to check the inputs for safety. `getSelMatrix()` leverages the function `parallel::mclapply()` package, in order to allow for parallel processing if the user has set this up. This requires a helper function `cssLoop()`, which is also specified in the later section for helper functions. 
We add the `parallel` package to the DESCRIPTION file:


```r
usethis::use_package("parallel")
```

```
## ✔ Adding 'parallel' to Imports field in DESCRIPTION
## • Refer to functions with `parallel::fun()`
```

`getSelMatrix()` also uses a feature selection function `fitfun` as an input. `fitfun` can be provided by the user as an input to `css()`. We provide one default `fitfun`, `cssLasso()`, which works on a real-valued response y given a specified penalty parameter lambda > 0. Both for its own importance and to show an example of a valid `fitfun`, we show `cssLasso()` here.


```r
#' Provided fitfun implementing the lasso
#'
#' Function used to select features with the lasso on each subsample in cluster
#' stability selection. Uses glmnet implementation of the lasso.
#' @param X A design matrix containing the predictors. (In practice this will
#' be a subsample of the full design matrix provided to `css()`.)
#' @param y A numeric vector containing the response.
#' @param lambda Numeric; a nonnegative number for the lasso penalty to use
#' on each subsample. (For now, only one lambda value can be provided to
#' `cssLasso()`; in the future, we plan to allow for multiple lambda values to be
#' provided to `cssLasso()`, as described in Faletto and Bien 2022.)
#' @return An integer vector; the indices of the features selected by the lasso.
#' @author Gregory Faletto, Jacob Bien
#' @references 
#' 
<<faletto2022>>
#' 
#' Jerome Friedman, Trevor Hastie,
#' Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models
#' via Coordinate Descent. \emph{Journal of Statistical Software}, 33(1), 1-22.
#' URL \url{https://www.jstatsoft.org/v33/i01/}.
#' @export
cssLasso <- function(X, y, lambda){
    # Check inputs

    # TODO(gregfaletto) allow cssLasso to accept a vector of lambda values rather
    # than just a single one.
    checkCssLassoInputs(X, y, lambda)

    n <- nrow(X)
    p <- ncol(X)

    # Fit a lasso path (full path for speed, per glmnet documentation)

    lasso_model <- glmnet::glmnet(X, y, family="gaussian")
    stopifnot(all.equal(class(lasso_model), c("elnet", "glmnet")))

    # Get coefficients at desired lambda

    pred <- glmnet::predict.glmnet(lasso_model, type="nonzero",
        s=lambda, exact=TRUE, newx=X, x=X, y=y)

    if(is.null(pred[[1]])){return(integer())}

    stopifnot(is.data.frame(pred))
    stopifnot(!("try-error" %in% class(pred) | "error" %in% class(pred) |
        "simpleError" %in% class(pred) | "condition" %in% class(pred)))

    if(length(dim(pred)) == 2){
        selected_glmnet <- pred[, 1]
    } else if(length(dim(pred)) == 3){
        selected_glmnet <- pred[, 1, 1]
    } else if(length(dim(pred)) == 1){
        selected_glmnet <- pred
    } else{
        stop("length(dim(pred)) not 1, 2, or 3")
    }

    stopifnot(length(selected_glmnet) >= 1)
    stopifnot(length(selected_glmnet) <= ncol(X))
    stopifnot(all(selected_glmnet == round(selected_glmnet)))
    stopifnot(length(selected_glmnet) == length(unique(selected_glmnet)))
    selected_glmnet <- as.integer(selected_glmnet)

    selected <- sort(selected_glmnet)

    return(selected)
}
```

Notice that `cssLasso()` depends on the package `glmnet`, and calls the function `checkCssLassoInputs()`, which verifies that the inputs to `cssLasso()` are as needed. (In particular, the function `css()` imposes very few requirements on `lambda` and `y` to allow the end user flexibility for any specified `fitfun`. `checkCssLassoInputs()` ensures that `y` is a real-valued response, `lambda` is a nonnegative real number, etc.) `checkCssLassoInputs()` is specified in the helper functions section later.

We add `glmnet` to the DESCRIPTION file as well:


```r
usethis::use_package("glmnet")
```

```
## ✔ Adding 'glmnet' to Imports field in DESCRIPTION
## • Refer to functions with `glmnet::fun()`
```



Finally, `getClusterSelMatrix()` is the last significant function called within `css()`.  It takes the information from `getSelMatrix()`, i.e. whether feature $j\in\hat S^\lambda(A_b)$, and outputs for every cluster $C$ whether $C\cap S^\lambda(A_b)\neq\emptyset$.


```r
#' Get cluster selection matrix
#'
#' Given a matrix of feature selection indicator variables and a list of
#' clusters of features, returns a matrix of cluster indicator variables.
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of formatClusters.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @param res A binary integer matrix. es[i, j] = 1 if feature j was selected
#' on subsample i and equals 0 otherwise, as in the output of `getSelMatrix()`.
#' (That is, each row is a selected set.)
#' @return A binary integer matrix with the same number of rows
#' as res and length(clusters) columns. Entry i, j is 1 if at least
#' one member of cluster j was selected on subsample i, and 0 otherwise.
#' @author Gregory Faletto, Jacob Bien
getClusterSelMatrix <- function(clusters, res){

    # Check input
    checkGetClusterSelMatrixInput(clusters, res)

    p <- ncol(res)

    n_clusters <- length(clusters)

    # Matrix of cluster selection proportions (with n_clusters columns)
    res_n_clusters <- matrix(rep(0L, nrow(res)*n_clusters), nrow=nrow(res),
        ncol=n_clusters)
    colnames(res_n_clusters) <- names(clusters)

    for(j in 1:n_clusters){
        # Identify rows of res where at least one feature from this cluster
        # was selected
        rows_j_sel <- apply(res, 1, function(x){any(x[clusters[[j]]] == 1)})

        # Put ones in these rows of res_n_clusters[, j]
        res_n_clusters[rows_j_sel, j] <- 1L

        if(length(clusters[[j]]) <= 1){
            next
        }
    }

    # Check output
    stopifnot(is.matrix(res_n_clusters))
    stopifnot(identical(colnames(res_n_clusters), names(clusters)))
    stopifnot(all(res_n_clusters %in% c(0, 1)))
    stopifnot(ncol(res_n_clusters) == length(clusters))

    return(res_n_clusters)
}
```

The helper function `checkGetClusterSelMatrixInput()` verifies the inputs to `getClusterSelMatrix()`.

With the above and the helper functions below, the `css()` function is complete!

## Helper functions

Below, we specify some of the helper functions for `css()`, which are less important for understanding what `css()` does.

* The function `checkCssInputs()` is called at the beginning of the `css()` function. It confirms that the inputs to `css()` are as expected. Also, `css()` allows some flexibility in how the inputs are formatted, and `checkCssInputs()` converts the inputs into consistent formats for later use.
  - The function `checkCssClustersInput()` is called within `checkCssInputs()`, and specifically checks the clusters input.
  - `formatClusters()` modifies clusters, ensuring every feature appears in exactly one cluster (any unclustered features are put in a "cluster" by themselves).
    * `checkFormatClustersInput()` verifies the input to `formatClusters()` for safety (this might seem a little redundant here, but `formatClusters()` is also called by different functions in this package).
      - `checkY()` ensures (here and elsewhere) that the provided `y` is as expected. (In particular, in general the inputted `y` can be a vector of any type as long as the specified `fitfun` can handle `y` appropriately. For some function inputs, `y` must be a numeric vector. `checkY()` enforces this, among other things.)
    * `checkClusters()` verifies that the output of `formatClusters()` is as expected.
    * `getPrototypes()` identifies the prototypes of each cluster--the feature within each cluster that has the largest marginal sample correlation with `y`.
      - `identifyPrototype()` is the workhorse of `getPrototypes()`, identifying the prototype given a single cluster.
        * `corFunction()` is a helper function called within `identifyPrototype()` to calculate the absolute value of the correlation between two vectors.
  - `checkSamplingType()` ensures that the input `sampling_type` is as expected. 
  - `checkB()` ensures that the input `B` is as expected.
  - `checkPropFeatsRemove()` ensures that the input `prop_feats_remove` is as expected.
* The function `cssLoop()` is called within `css()`. In particular, it is a helper function needed in order to allow for parallel processing of the feature selection method on each of the subsamples.
  - `checkCssLoopOutput()` verifies that the output of `cssLoop()` is as expected. This is particularly important because the user can specify their own feature selection method; `checkCssLoopOutput()` verifies that the output of the feature selection method is valid, and provides an informative error message if not.
* `checkCssLassoInputs()` checks that the inputs to the provided `fitfun`, `cssLasso()`, are as needed.
* `checkGetClusterSelMatrixInput()` verifies that the input to `getClusterSelMatrix()` is as expected.

Tests are written for all of these functions and appear as early as possible after the function is defined. (For functions that don't depend on any other functions, tests appear immediately after the function; for functions with dependencies, the tests appear after all dependencies have been defined.)

`checkCssInputs()`:


```r
#' Helper function to confirm that inputs to the function `css()` are as expected,
#' and modify inputs if needed
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors.
#' @param y The response; can be anything that takes the form of an
#' n-dimensional vector, with the ith entry corresponding to the ith row of X.
#' Typically (and for default fitfun = cssLasso), y should be an n-dimensional
#' numeric vector.
<<param-lambda>>
<<param-clusters>>
#' @param fitfun A function; the feature selection function used on each
#' subsample by cluster stability selection. This can be any feature selection
#' method; the only requirement is that it accepts the arguments (and only the
#' arguments) X, y, and lambda and returns an integer vector that is a subset of
#' 1:p. For example, fitfun could be best subset selection or forward stepwise
#' selection or LARS and lambda could be the desired model size; or fitfun could be the
#' elastic net and lambda could be a length-two vector specifying lambda and
#' alpha. Default is cssLasso, an implementation of lasso (relying on the R
#' package glmnet), where lambda must be a positive numeric specifying the L1
#' penalty for the lasso.
#' @param sampling_type A character vector; either "SS" or "MB". For "MB",
#' all B subsamples are drawn randomly (as proposed by Meinshausen and Bühlmann
#' 2010). For "SS", in addition to these B subsamples, the B complementary pair
#' subsamples will be drawn as well (see Faletto and Bien 2022 or Shah and
#' Samworth 2013 for details). Default is "SS", and "MB" is not supported yet.
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the total number of subsamples will be `B`; for
#' sampling_type="SS" the number of subsamples will be `2*B`. Default is 100
#' for sampling_type="MB" and 50 for sampling_type="SS".
#' @param prop_feats_remove Numeric; if prop_feats_remove is greater than 0,
#' then on each subsample, each feature is randomly dropped from the design
#' matrix that is provided to fitfun with probability prop_feats_remove
#' (independently across features). That is, in a typical subsample,
#' prop_feats_remove*p features will be dropped (though this number will vary).
#' This is similar in spirit (but distinct from) extended stability selection
#' (Beinrucker et. al. 2016); see their paper for some of the benefits of
#' dropping features (besides increasing computational speed and decreasing
#' memory requirements). For sampling_type="SS", the features dropped in
#' each complementary pair of subsamples are identical in order to ensure that
#' the theoretical guarantees of Faletto and Bien (2022) are retained within
#' each individual pair of subsamples. (Note that this feature is not
#' investigated either theoretically or in simulations by Faletto and Bien
#' 2022). Must be between 0 and 1. Default is 0.
#' @param train_inds Optional; an integer or numeric vector containing the
#' indices of observations in X and y to set aside for model training by the
#' function getCssPreds after feature selection. (This will only work if y is
#' real-valued, because getCssPreds using ordinary least squares regression to
#' generate predictions.) If train_inds is not provided, all of the observations
#' in the provided data set will be used for feature selection.
#' @param num_cores Optional; an integer. If using parallel processing, the
#' number of cores to use for parallel processing (num_cores will be supplied
#' internally as the mc.cores argument of parallel::mclapply).
#' @return A named list with the following elements: \item{feat_names}{A 
#' character vector containing the column names of X (if the provided X
#' had column names). If the provided X did not have column names, feat_names
#' will be NA.} \item{X}{The provided X, converted to a matrix if it was
#' originally provided as a data.frame, and with feature names removed if they
#' had been provided.}\item{clusters}{A list of integer vectors; each vector
#' will contain the indices of a cluster of features. Any duplicated clusters
#' provided in the input will be removed.}
#' @author Gregory Faletto, Jacob Bien
checkCssInputs <- function(X, y, lambda, clusters, fitfun, sampling_type, B,
    prop_feats_remove, train_inds, num_cores){

    stopifnot(is.matrix(X) | is.data.frame(X))

    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))

    feat_names <- as.character(NA)
    if(!is.null(colnames(X))){
        feat_names <- colnames(X)
    }

    n <- nrow(X)
    p <- ncol(X)

    if(!is.null(colnames(X))){
        feat_names <- colnames(X)
    }

    stopifnot(p >= 2)
    if(length(feat_names) > 1){
        stopifnot(length(feat_names) == p)
    } else{
        stopifnot(is.na(feat_names))
    }

    colnames(X) <- character()

    stopifnot(length(y) == n)
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    ### Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clusters <- formatClusters(clusters, p=p, clust_names=clust_names)$clusters

    stopifnot(class(fitfun) == "function")
    stopifnot(length(fitfun) == 1)
    if(!identical(formals(fitfun), formals(cssLasso))){
        err_mess <- paste("fitfun must accept arguments named X, y, and lambda. Detected arguments to fitfun:",
            paste(names(formals(fitfun)), collapse=", "))
        stop(err_mess)
    }

    checkSamplingType(sampling_type)
    checkB(B)
    checkPropFeatsRemove(prop_feats_remove, p)

    stopifnot(is.numeric(train_inds) | is.integer(train_inds))
    if(length(train_inds) > 0){
        stopifnot(all(!is.na(train_inds)))
        stopifnot(all(round(train_inds) == train_inds))
        stopifnot(length(train_inds) == length(unique(train_inds)))
        stopifnot(all(train_inds >= 1))
        stopifnot(all(train_inds) <= n)
        stopifnot(length(train_inds) <= n - 2)
        stopifnot(length(train_inds) >= 1)
    }

    stopifnot(length(num_cores) == 1)
    stopifnot(is.integer(num_cores) | is.numeric(num_cores))
    stopifnot(!is.na(num_cores))
    stopifnot(num_cores == round(num_cores))
    stopifnot(num_cores >= 1)
    stopifnot(num_cores <= parallel::detectCores())

    return(list(feat_names=feat_names, X=X, clusters=clusters))
}
```

checkCssClustersInput:


```r
#' Helper function to confirm that clusters input to css is as expected
#'
<<param-clusters>>
#' @return Same as the input, but all of the clusters will be coerced to
#' integers.
#' @author Gregory Faletto, Jacob Bien
checkCssClustersInput <- function(clusters){
    stopifnot(!is.na(clusters))
    if(is.list(clusters)){
        stopifnot(all(!is.na(clusters)))
        stopifnot(length(clusters) == length(unique(clusters)))

        if(length(clusters) > 0){
            for(i in 1:length(clusters)){
                stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
                stopifnot(all(!is.na(clusters[[i]])))
                stopifnot(is.integer(clusters[[i]]) | is.numeric(clusters[[i]]))
                stopifnot(all(clusters[[i]] == round(clusters[[i]])))
                stopifnot(all(clusters[[i]] >= 1))
                clusters[[i]] <- as.integer(clusters[[i]])
            }

            if(length(clusters) >= 2){
                # Check that clusters are non-overlapping
                for(i in 1:(length(clusters) - 1)){
                    for(j in (i+1):length(clusters)){
                        if(length(intersect(clusters[[i]], clusters[[j]])) != 0){
                            error_mes <- paste("Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: ",
                                i, ", ", j, ".", sep="")
                            stop(error_mes)
                        }
                    }
                }
            }
        }
    } else{
        # If clusters is not a list, it should be a vector of indices of
        # features that are in the (one) cluster
        stopifnot(is.numeric(clusters) | is.integer(clusters))
        stopifnot(length(clusters) == length(unique(clusters)))
        stopifnot(all(!is.na(clusters)))
        stopifnot(is.integer(clusters) | is.numeric(clusters))
        stopifnot(all(clusters == round(clusters)))
        stopifnot(all(clusters >= 1))
        clusters <- as.integer(clusters)
    }
    return(clusters)
}
```

tests for checkCssClustersInput:


```r
testthat::test_that("checkCssClustersInput works", {
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  res <- checkCssClustersInput(good_clusters)
  
  # clusters
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), length(names(res)))
  testthat::expect_equal(length(res), length(unique(names(res))))
  testthat::expect_true(all(!is.na(names(res))))
  testthat::expect_true(all(!is.null(names(res))))
  clust_feats <- integer()
  for(i in 1:length(res)){
    clust_feats <- c(clust_feats, res[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:8)))

  ## Trying other inputs
  
  unnamed_clusters <- list(1L:3L, 5L:8L)
  
  res <- checkCssClustersInput(unnamed_clusters)
  
  # clusters
  testthat::expect_true(is.list(res))
  clust_feats <- integer()
  for(i in 1:length(res)){
    clust_feats <- c(clust_feats, res[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:8)))
  
  testthat::expect_error(checkCssClustersInput(list(1:4, 4:6)),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssClustersInput(list(2:3, 2:3)),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssClustersInput(list(2:3, as.integer(NA))),
                         "!is.na(clusters) are not all TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssClustersInput(list(2:3, c(4, 4, 5))),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssClustersInput(list(2:3, -1)),
                         "all(clusters[[i]] >= 1) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssClustersInput(c(0.4, 0.6)),
                         "all(clusters == round(clusters)) is not TRUE",
                         fixed=TRUE)
  
  # Single cluster
  res_sing_clust <- checkCssClustersInput(2:5)
  testthat::expect_equal(length(res_sing_clust), 4)


})
```

```
## Test passed 🥇
```


`formatClusters()`:


```r
#' Formats clusters in standardized way, optionally estimating cluster
#' prototypes
#'
#' @param clusters Either an integer vector of a list of integer vectors; each
#' vector should contain the indices of a cluster of features. (If there is only
#' one cluster, clusters can either be a list of length 1 or simply an integer
#' vector.) If clusters is specified then R is ignored.
#' @param p integer or numeric; the numbe of features in x (should match 
#' ncol(x), if x is provided)
#' @param clust_names A character vector of the names of the clusters in
#' clusters.
#' @param get_prototypes Logical: if TRUE, will identify prototype from each
#' cluster (the feature from each cluster that is most correlated with the
#' response) for the protolasso. In this case, x and y must be provided.
#' @param x n x p numeric matrix; design matrix. Only needs to be provided if
#' get_prototypes is TRUE.
#' @param y Numeric response vector; only needs to be provided if get_prototypes
#' is TRUE. Note: in general, the css function does not require y to be a
#' numeric vector, because the provided fitfun could use a different form of y
#' (for example, a categorical response variable). However, y must be numeric in
#' order to provide prototypes because the prototypes are determined using the
#' correlation between cluster members (columns of x) and y.
#' @param R Numeric p x p matrix; not currently used. Entry ij contains the 
#' "substitutive value" of feature i for feature j (diagonal must consist of
#' ones, all entries must be between 0 and 1, and matrix must be symmetric)
#' @return A named list with the following elements: \item{clusters}{A named
#' list where each entry is an integer vector of indices of features that are in
#' a common cluster. (The length of list clusters is equal to the number of
#' clusters.) All identified clusters are non-overlapping. All features appear
#' in exactly one cluster (any unclustered features will be put in their own
#' "cluster" of size 1).} \item{multiple}{Logical; TRUE if there is more than
#' one cluster of size greater than 1, FALSE otherwise.} \item{prototypes}{only
#' returned if get_prototypes=TRUE. An integer vector whose length is equal to
#' the number of clusters. Entry i is the index of the feature belonging to
#' cluster i that is most highly correlated with y (that is, the prototype for
#' the cluster, as in the protolasso; see Reid and Tibshirani 2016).}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
formatClusters <- function(clusters=NA, p=-1, clust_names=NA, 
    get_prototypes=FALSE, x=NA, y=NA, R=NA){

    # Check inputs
    clusters <- checkFormatClustersInput(clusters, p, clust_names,
        get_prototypes, x, y, R)

    n <- nrow(x)

    multiple <- FALSE

    if(any(lengths(clusters) > 1)){ # & length(clusters) > 1
        # Only care about clusters with more than one element (only ones that
        # need to be treated differently)
        # keep track of whether there's more than one cluster or not
        multiple <- sum(lengths(clusters) > 1) > 1
    }

    # For any features not already in a cluster, add a cluster containing only
    # that feature
    orig_length_clusters <- length(clusters)

    stopifnot(p >= 1)
    for(i in 1:p){
        feat_i_found <- FALSE
        if(orig_length_clusters > 0){
            for(j in 1:orig_length_clusters){
                # If i is in cluster j, break out of this loop and consider the
                # next i
                if(i %in% clusters[[j]]){
                    feat_i_found <- TRUE
                    break
                }
            }
        }

        # If feature i wasn't found in any cluster, add a cluster containing
        # just feature i
        if(!feat_i_found){
            clusters[[length(clusters) + 1]] <- i
        }
    }

    n_clusters <- length(clusters)

    # Add names to clusters
    if(is.null(names(clusters))){
        names(clusters) <- paste("c", 1:n_clusters, sep="")
    } else{
        # What clusters need names?
        unnamed_clusts <- which(is.na(names(clusters)) | names(clusters) == "")
        if(length(unnamed_clusts) > 0){
            proposed_clust_names <- paste("c", unnamed_clusts, sep="")
            # Replace any proposed cluster names that are already in use
            if(any(proposed_clust_names %in% names(clusters))){
                proposed_clust_names[proposed_clust_names %in% names(clusters)] <- paste("c",
                    unnamed_clusts[proposed_clust_names %in% names(clusters)] + p,
                    sep="")
            }
            while_counter <- 0
            while(any(proposed_clust_names %in% names(clusters))){
                proposed_clust_names[proposed_clust_names %in% names(clusters)] <- paste(proposed_clust_names[proposed_clust_names %in% names(clusters)],
                    "_1", sep="")
                while_counter <- while_counter + 1
                if(while_counter >= 100){
                    stop("Function formatClusters stuck in an infinite while loop")
                }
            }
            stopifnot(length(unnamed_clusts) == length(proposed_clust_names))
            names(clusters)[unnamed_clusts] <- proposed_clust_names
        }
    }

    # Check output

    checkClusters(clusters, p)
    stopifnot(is.logical(multiple))
    stopifnot(length(multiple) == 1)
    stopifnot(!is.na(multiple))

    if(get_prototypes){
        prototypes <- getPrototypes(clusters, x, y)

        return(list(clusters=clusters, multiple=multiple,
            prototypes=prototypes))
    } else{
        return(list(clusters=clusters, multiple=multiple))
    }
}
```

`checkFormatClustersInput()`:


```r
#' Helper function to ensure that the inputs to formatClusters are as expected
#'
#' @param clusters Either an integer vector of a list of integer vectors; each
#' vector should contain the indices of a cluster of features. (If there is only
#' one cluster, clusters can either be a list of length 1 or simply an integer
#' vector.) If clusters is specified then R is ignored.
#' @param p integer or numeric; the numbe of features in x (should match 
#' ncol(x), if x is provided)
#' @param clust_names A character vector of the names of the clusters in clusters.
#' @param get_prototypes Logical: if TRUE, will identify prototype from each
#' cluster (the feature from each cluster that is most correlated with the
#' response) for the protolasso. In this case, x and y must be provided.
#' @param x n x p numeric matrix; design matrix. Only needs to be provided if
#' get_prototypes is TRUE.
#' @param y Numeric response vector; only needs to be provided if get_prototypes
#' is TRUE. Note: in general, the css function does not require y to be a
#' numeric vector, because the provided fitfun could use a different form of y
#' (for example, a categorical response variable). However, y must be numeric in
#' order to provide prototypes because the prototypes are determined using the
#' correlation between cluster members (columns of x) and y.
#' @param R Numeric p x p matrix; not currently used. Entry ij contains the 
#' "substitutive value" of feature i for feature j (diagonal must consist of
#' ones, all entries must be between 0 and 1, and matrix must be symmetric)
#' @return A list of integer vectors; each vector will contain the indices of a
#' cluster of features. Any duplicated clusters provided in the input will be
#' removed.
#' @author Gregory Faletto, Jacob Bien
checkFormatClustersInput <- function(clusters, p, clust_names, 
    get_prototypes, x, y, R){

    if(any(is.na(clusters)) & any(is.na(R))){
        stop("Must specify one of clusters or R (or does one of these provided inputs contain NA?)")
    }

    stopifnot(is.integer(p) | is.numeric(p))
    stopifnot(length(p) == 1)
    stopifnot(p == round(p))
    stopifnot(!is.na(p))
    if(p > 0){
        stopifnot(p >= 2)
    }

    use_R <- FALSE
    if(any(is.na(clusters)) | length(clusters) == 0){
        if(all(!is.na(R))){
            stopifnot(is.matrix(R))
            stopifnot(all(dim(R) == p))
            stopifnot(all(diag(R) == 1))
            stopifnot(identical(R, t(R)))
            stopifnot(all(!is.na(R)))
            stopifnot(all(R %in% c(0, 1)))
            use_R <- TRUE
        }
    } else{
        stopifnot(!is.list(clusters) | all(lengths(clusters) >= 1))
        stopifnot(is.list(clusters) | length(clusters) >= 1)
        stopifnot(all(!is.na(clusters)))

        if(is.list(clusters) & length(clusters) > 0){
            for(i in 1:length(clusters)){
                stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
                stopifnot(all(!is.na(clusters[[i]])))
                stopifnot(all(clusters[[i]] >= 1))
                stopifnot(is.integer(clusters[[i]]))
            }

            stopifnot(length(clusters) == length(unique(clusters)))

            clusters <- clusters[!duplicated(clusters)]

            if(length(clusters) >= 2){
                # Check that clusters are non-overlapping
                for(i in 1:(length(clusters) - 1)){
                    for(j in (i+1):length(clusters)){
                        stopifnot(length(intersect(clusters[[i]],
                            clusters[[j]])) == 0)
                    }
                }
            }

            if(any(!is.na(clust_names))){
                stopifnot(length(clust_names) == length(clusters))
            }
        } else if(!is.list(clusters)){
            clusters_temp <- clusters
            clusters <- list()
            clusters[[1]] <- clusters_temp
            rm(clusters_temp)
        }
    }

    stopifnot(length(get_prototypes) == 1)
    stopifnot(is.logical(get_prototypes))

    if(any(!is.na(clust_names))){
        stopifnot(is.character(clust_names))
    }

    if(get_prototypes){
        stopifnot(all(!is.na(x)))
        stopifnot(is.matrix(x))

        n <- nrow(x)

        checkY(y, n)
    }

    if(use_R){
        # Determine clusters from R
        clusters <- list()

        for(i in 1:nrow(R)){
            clusters[[i]] <- as.integer(which(R[i, ] > 0))
            stopifnot(length(clusters[[i]]) == length(unique(clusters[[i]])))
            stopifnot(all(!is.na(clusters[[i]])))
            stopifnot(is.integer(clusters[[i]]))
        }

        clusters <- unique(clusters)
        stopifnot(is.list(clusters))

        if(length(clusters) >= 2){
            # Check that clusters are non-overlapping
            for(i in 1:(length(clusters) - 1)){
                for(j in (i+1):length(clusters)){
                    if(length(intersect(clusters[[i]], clusters[[j]])) != 0){
                        stop("Invalid R matrix with overlapping clusters (clusters must not be overlapping)")
                    }
                }
            }
        }
    }

    stopifnot(is.list(clusters))

    return(clusters)
}
```

`checkY()`:


```r
#' Helper function to confirm that the argument y to several functions is
#' as expected
#'
#' @param y Numeric response vector.
#' @param n Number of observations of covariates; should match length of y.
#' @author Gregory Faletto, Jacob Bien
checkY <- function(y, n){
    stopifnot(all(!is.na(y)))
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(length(unique(y)) > 1)
    stopifnot(length(n) == 1)
    stopifnot(!is.na(n))
    stopifnot(is.numeric(n) | is.integer(n))
    stopifnot(n == round(n))
    stopifnot(n > 0)
    stopifnot(n == length(y))
}
```

Tests for `checkY()`:


```r
testthat::test_that("checkY works", {
  testthat::expect_null(checkY(as.numeric(1:20)*.1, 20))
  testthat::expect_null(checkY(1L:15L, 15))
  testthat::expect_error(checkY(1:7, 8), "n == length(y) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkY(1:7, -7), "n > 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkY(rep(as.numeric(NA), 13), 13),
                         "all(!is.na(y)) is not TRUE", fixed=TRUE)
  testthat::expect_error(checkY(rep(5.2, 9), 9),
                         "length(unique(y)) > 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkY(c(TRUE, FALSE, TRUE), 3),
                         "is.numeric(y) | is.integer(y) is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 🌈
```

Tests for `checkFormatClustersInput()`:


```r
testthat::test_that("checkFormatClustersInput works", {
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  res <- checkFormatClustersInput(good_clusters, p=10,
                                  clust_names=c("red_cluster", "green_cluster"),
                                  get_prototypes=FALSE, x=NA, y=NA, R=NA)
  
  testthat::expect_true(is.list(res))
  clust_feats <- integer()
  for(i in 1:length(res)){
    clust_feats <- c(clust_feats, res[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:8)))

  ## Trying other inputs
  
  unnamed_clusters <- list(1L:3L, 5L:8L)

  res <- checkFormatClustersInput(unnamed_clusters, p=10, clust_names=NA,
                                  get_prototypes=FALSE, x=NA, y=NA, R=NA)

  # clusters
  testthat::expect_true(is.list(res))
  clust_feats <- integer()
  for(i in 1:length(res)){
    clust_feats <- c(clust_feats, res[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:8)))

  testthat::expect_error(checkFormatClustersInput(list(1:4, 4:6), p=10,
                                                  clust_names=NA,
                                                  get_prototypes=FALSE, x=NA,
                                                  y=NA, R=NA),
                         "length(intersect(clusters[[i]], clusters[[j]])) == 0 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkFormatClustersInput(list(2:3, 2:3), p=10,
                                  clust_names=NA, get_prototypes=FALSE, x=NA,
                                  y=NA, R=NA),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkFormatClustersInput(list(2:3, as.integer(NA)),
                                                  p=10,
                                                  clust_names=NA,
                                                  get_prototypes=FALSE, x=NA,
                                                  y=NA, R=NA),
                         "Must specify one of clusters or R (or does one of these provided inputs contain NA?)",
                         fixed=TRUE)
  
  testthat::expect_error(checkFormatClustersInput(list(2:3, c(4, 4, 5)),
                                                  p=10,
                                                  clust_names=NA,
                                                  get_prototypes=FALSE, x=NA,
                                                  y=NA, R=NA),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)
  
   testthat::expect_error(checkFormatClustersInput(list(1:4, -1),
                                                  p=10,
                                                  clust_names=NA,
                                                  get_prototypes=FALSE, x=NA,
                                                  y=NA, R=NA),
                         "all(clusters[[i]] >= 1) is not TRUE",
                         fixed=TRUE)
   
   testthat::expect_error(checkFormatClustersInput(list(1:4, c(2.3, 1.2)),
                                                  p=10,
                                                  clust_names=NA,
                                                  get_prototypes=FALSE, x=NA,
                                                  y=NA, R=NA),
                         "is.integer(clusters[[i]]) is not TRUE",
                         fixed=TRUE)

  # Single cluster
   testthat::expect_true(is.list(checkFormatClustersInput(c(1:5), p=10,
                                                          clust_names=NA,
                                                          get_prototypes=FALSE,
                                                          x=NA, y=NA, R=NA)))
})
```

```
## Test passed 😸
```


`checkClusters()`:


```r
#' Helper function to confirm that the argument clusters to several functions is
#' as expected
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of css or
#' formatClusters. (The length of list clusters is equal to the number of
#' clusters.) All identified clusters must be non-overlapping, and all features
#' must appear in exactly one cluster (any unclustered features should be in
#' their own "cluster" of size 1).
#' @param p The number of features; must be at least as large as the number of
#' clusters.
#' @author Gregory Faletto, Jacob Bien
checkClusters <- function(clusters, p){
    stopifnot(is.list(clusters))
    stopifnot(all(lengths(clusters) >= 1))
    stopifnot(all(!is.na(clusters)))

    n_clusters <- length(clusters)

    stopifnot(n_clusters == length(unique(clusters)))
    stopifnot(n_clusters <= p)
    stopifnot(!is.null(names(clusters)))
    stopifnot(is.character(names(clusters)))
    stopifnot(all(!is.na(names(clusters)) & names(clusters) != ""))
    stopifnot(length(unique(names(clusters))) == n_clusters)

    all_clustered_feats <- integer()
    for(i in 1:n_clusters){
        stopifnot(is.integer(clusters[[i]]))
        all_clustered_feats <- c(all_clustered_feats, clusters[[i]])
    }

    stopifnot(length(all_clustered_feats) == p)
    stopifnot(length(unique(all_clustered_feats)) == p)
    stopifnot(all(all_clustered_feats <= p))
    stopifnot(all(all_clustered_feats >= 1))
}
```

Tests for `checkClusters()`:


```r
testthat::test_that("checkClusters works", {
  good_clusters <- list(c1=1L:5L, c2=6L:8L, c3=9L)
  
  testthat::expect_null(checkClusters(good_clusters, 9))
  testthat::expect_error(checkClusters(good_clusters, 10),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(1L:10L, 10),
                         "is.list(clusters) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=1L:5L, c2=6L:8L, c3=9L,
                                            c4=integer()), 9),
                         "all(lengths(clusters) >= 1) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=1L:5L, c2=6L:8L, c3=9L,
                                            c4=as.integer(NA)), 9),
                         "all(!is.na(clusters)) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=1L:5L, c2=6L:8L, c3=9L,
                                            c2=6L:8L), 9),
                         "n_clusters == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=1L:5L, c2=6L:8L, c3=10L), 9),
                         "all(all_clustered_feats <= p) is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 🎉
```


`getPrototypes()`:


```r
#' Estimate prototypes from a list of clusters
#'
#' Takes in list of clusters, x, and y and returns an integer vector (of length
#' equal to the number of clusters) of the indices of the feature prototypes
#' (the features from each cluster most correlated with the response).
#'
#' @param clusters A list where each entry is an integer vector of indices of
#' features that are in a common cluster. (The length of list clusters must be
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. Must only include clusters of size 2 or larger.
#' @param x n x p numeric matrix; design matrix.
#' @param y Numeric response vector. Note: in general, the css function does not
#' require y to be a numeric vector, because the provided fitfun could use a
#' different form of y (for example, a categorical response variable). However,
#' y must be numeric in order to provide prototypes because the prototypes are
#' determined using the correlation between cluster members (columns of x) and
#' y.
#' @return An integer vector of the same length as clusters. Entry j is the
#' index of the feature identified as the prototype for cluster j.
#' @author Gregory Faletto, Jacob Bien
getPrototypes <- function(clusters, x, y){
    # Check inputs

    stopifnot(!is.list(clusters) | all(lengths(clusters) >= 1))
    stopifnot(is.list(clusters) | length(clusters) >= 1)

    stopifnot(all(!is.na(x)))
    stopifnot(is.matrix(x))

    n <- nrow(x)
    p <- ncol(x)

    checkY(y, n)

    # Identify prototypes
    if(length(clusters) > 0){
        if(is.list(clusters)){
            prototypes <- rep(as.integer(NA), length(clusters))
            for(i in 1:length(clusters)){
                prototypes[i] <- identifyPrototype(clusters[[i]], x, y)
            }
        } else{
            # If clusters is not a list, then there is only one cluster.
            prototypes <- identifyPrototype(clusters, x, y)
        }
    }  else{
        prototypes <- integer()
    }

    # Check output

    stopifnot(is.integer(prototypes))
    if(length(clusters) > 0){
        if(is.list(clusters)){
            stopifnot(length(prototypes) == length(clusters))
        } else {
            stopifnot(length(prototypes) == 1)
        }
    } else{
        stopifnot(length(prototypes) == 0)
    }

    stopifnot(all(!is.na(prototypes)))
    stopifnot(length(prototypes) == length(unique(prototypes)))

    return(prototypes)
}
```

`identifyPrototype()`:


```r
#' Estimate prototypes from a single cluster
#'
#' Takes in a single cluster, x, and y and returns an integer of the index of
#' the feature prototype (the feature from the cluster most correlated with the
#' response).
#'
#' @param cluster_members_i An integer vector of indices of features that are in
#' a common cluster. Must have length at least 2.
#' @param x n x p numeric matrix; design matrix.
#' @param y Numeric response vector. Note: in general, the css function does not
#' require y to be a numeric vector, because the provided fitfun could use a
#' different form of y (for example, a categorical response variable). However,
#' y must be numeric in order to provide prototypes because the prototypes are
#' determined using the correlation between cluster members (columns of x) and
#' y.
#' @return integer; the index of the feature identified as the prototype for
#' the cluster.
#' @author Gregory Faletto, Jacob Bien
identifyPrototype <- function(cluster_members_i, x, y){
    # Check input
    stopifnot(is.integer(cluster_members_i))
    # If cluster only has one member, that member is the prototype
    if(length(cluster_members_i) == 1){
        return(cluster_members_i)
    }

    # Choose which cluster member to represent cluster for stability
    # metric purposes by choosing the one most highly correlated
    # with y

    cors_i <- apply(x[, cluster_members_i], 2, corFunction, y=y)
    max_index_i <- which.max(cors_i)[1]

    stopifnot(length(max_index_i) == 1)
    stopifnot(max_index_i %in% 1:length(cluster_members_i))

    ret <- cluster_members_i[max_index_i]

    # Check output

    stopifnot(is.integer(ret))
    stopifnot(length(ret) == 1)
    stopifnot(ret %in% cluster_members_i)
    stopifnot(identical(x[, cluster_members_i][, max_index_i],
        x[, ret]))

    return(ret)
}
```

`corFunction()`:


```r
#' Absolute value of sample correlation between two vectors
#'
#' Calculates the absolute value of correlation of t and y. If either input has
#' only one unique value, returns 0 by definition.
#' @param t A numeric or integer vector.
#' @param y A numeric or integer vector; must have the same length as t.
#' @return A numeric vector of the same length as cluster_i containing the
#' weights corresponding to each of the features in cluster_i. The weights
#' will all be nonnegative and sum to 1.
#' @author Gregory Faletto, Jacob Bien
corFunction <- function(t, y){
    # Check inputs
    stopifnot(is.numeric(t) | is.integer(t))
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(length(t) == length(y))
    if(length(unique(t)) == 1){
        return(0)
    }
    if(length(unique(y)) == 1){
        warning("The second argument to corFunction only had one unique entry")
        return(0)
    }
    return(abs(stats::cor(t, y)))
}
```


Tests for `corFunction()`


```r
testthat::test_that("corFunction works", {
  testthat::expect_identical(corFunction(rep(1, 10), 1:10), 0)
  testthat::expect_identical(corFunction(rep(1.2, 5), 1:5), 0)
  
  set.seed(23451)
  
  x <- stats::rnorm(8)
  y <- stats::rnorm(8)
  testthat::expect_identical(corFunction(x, y), abs(stats::cor(x, y)))
  testthat::expect_warning(corFunction(1:5, rep(1.2, 5)),
                           "The second argument to corFunction only had one unique entry",
                           fixed=TRUE)
  testthat::expect_error(corFunction("1", "2"),
                         "is.numeric(t) | is.integer(t) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(corFunction(3:8, "2"),
                         "is.numeric(y) | is.integer(y) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(corFunction(3:8, 1:2),
                         "length(t) == length(y) is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 😸
```

Tests for `identifyPrototype()` (requires `corFunction()`):


```r
testthat::test_that("identifyPrototype works", {
  testthat::expect_identical(identifyPrototype(10L, "a", 5), 10L)
  n <- 10
  p <- 5
  
  set.seed(9834)
  
  X <- matrix(stats::rnorm(n*p), nrow=n, ncol=p)
  y <- X[, p]
  testthat::expect_equal(identifyPrototype(as.integer(p), X, y), p)
  testthat::expect_equal(identifyPrototype(2L, X, y), 2)
  testthat::expect_equal(identifyPrototype(as.integer(2:p), X, y), p)
  testthat::expect_error(identifyPrototype(as.integer(2:p), y, X),
                         "incorrect number of dimensions",
                         fixed=TRUE)
  
  y2 <- rnorm(n)

  res <- identifyPrototype(c(2L, 3L), X, y2)

  testthat::expect_true(is.integer(res))

  testthat::expect_equal(length(res), 1)

  testthat::expect_true(res %in% c(2L, 3L))

})
```

```
## Test passed 🎊
```

Tests for `getPrototypes()`


```r
testthat::test_that("getPrototypes works", {
  n <- 10
  p <- 5
  
  set.seed(902689)
  
  X <- matrix(stats::rnorm(n*p), nrow=n, ncol=p)
  y <- X[, p]

  testthat::expect_identical(getPrototypes(list(1L, 2L, 3L, 4L, 5L), X, y), 1:5)

  testthat::expect_identical(getPrototypes(list(1L:5L), X, y), 5L)

  testthat::expect_identical(getPrototypes(list(1L, 2L:5L), X, y), c(1L, 5L))

  testthat::expect_identical(getPrototypes(list(3L:5L), X, y), 5L)

  y2 <- rnorm(n)

  res <- getPrototypes(list(1L, c(2L, 3L), c(4L, 5L)), X, y2)

  testthat::expect_true(is.integer(res))

  testthat::expect_equal(length(res), 3)

  testthat::expect_identical(res[1], 1L)

  testthat::expect_true(res[2] %in% c(2L, 3L))

  testthat::expect_true(res[3] %in% c(4L, 5L))

  testthat::expect_error(getPrototypes(list(1L, 2L, 3L, 4L, 5L), y, X),
                          "is.matrix(x) is not TRUE",
                          fixed=TRUE)

  testthat::expect_error(getPrototypes(list(1L, 2L, 3L, 4L, 5L), X, y[1:9]),
                         "n == length(y) is not TRUE",
                         fixed=TRUE)

})
```

```
## Test passed 🎊
```

Finally, tests for `formatClusters()`:



```r
testthat::test_that("formatClusters works", {
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1:3, 5:8)
  
  res <- formatClusters(good_clusters, p=10)
  
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("clusters", "multiple"))
  
  # Clusters
  testthat::expect_true(is.list(res$clusters))
  testthat::expect_equal(length(res$clusters), 5)
  testthat::expect_equal(5, length(names(res$clusters)))
  testthat::expect_equal(5, length(unique(names(res$clusters))))
  testthat::expect_true("red_cluster" %in% names(res$clusters))
  testthat::expect_true(all(!is.na(names(res$clusters))))
  testthat::expect_true(all(!is.null(names(res$clusters))))
  testthat::expect_true(all(names(res$clusters) != ""))

  clust_feats <- integer()
  true_list <- list(1:3, 5:8, 4, 9, 10)
  for(i in 1:length(res$clusters)){
    testthat::expect_true(is.integer(res$clusters[[i]]))
    testthat::expect_equal(length(intersect(clust_feats, res$clusters[[i]])), 0)
    testthat::expect_true(all(res$clusters[[i]] %in% 1:10))
    testthat::expect_equal(length(res$clusters[[i]]),
                           length(unique(res$clusters[[i]])))
    testthat::expect_true(all(res$clusters[[i]] == true_list[[i]]))
    clust_feats <- c(clust_feats, res$clusters[[i]])
  }

  testthat::expect_equal(length(clust_feats), 10)
  testthat::expect_equal(10, length(unique(clust_feats)))
  testthat::expect_equal(10, length(intersect(clust_feats, 1:10)))
  
  # Multiple
  testthat::expect_true(res$multiple)
  testthat::expect_false(formatClusters(3:5, p=10)$multiple)

  ## Trying other inputs

  testthat::expect_error(formatClusters(list(3:7, 7:10), p=15),
                         "length(intersect(clusters[[i]], clusters[[j]])) == 0 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(formatClusters(list(5:8, 5:8), p=9),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formatClusters(list(5:8), p=7),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formatClusters(list(2:3, as.integer(NA)), p=10),
                         "Must specify one of clusters or R (or does one of these provided inputs contain NA?)",
                         fixed=TRUE)

  testthat::expect_error(formatClusters(list(2:3, c(4, 4, 5)), p=8),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)

   testthat::expect_error(formatClusters(list(1:4, -1), p=10),
                         "all(clusters[[i]] >= 1) is not TRUE",
                         fixed=TRUE)

   testthat::expect_error(formatClusters(list(1:4, c(2.3, 1.2))),
                         "is.integer(clusters[[i]]) is not TRUE",
                         fixed=TRUE)
   
   ### Test prototypes feature
   
    n <- 8
    p <- 6
    
    set.seed(690289)
    
    X <- matrix(stats::rnorm(n*p), nrow=n, ncol=p)
    y <- X[, p]
    
    res <- formatClusters(clusters=list(), p=p, get_prototypes=TRUE, x=X, y=y)
    
    testthat::expect_true(is.list(res))
    testthat::expect_identical(names(res), c("clusters", "multiple",
                                             "prototypes"))
    testthat::expect_true(is.integer(res$prototypes))
    testthat::expect_identical(res$prototypes, 1:p)

    testthat::expect_equal(formatClusters(clusters=1:p, p=p,
                                              get_prototypes=TRUE, x=X,
                                              y=y)$prototypes, p)
    
    testthat::expect_identical(formatClusters(clusters=list(1L, 2L:p), p=p,
                                              get_prototypes=TRUE, x=X,
                                              y=y)$prototypes,
                               as.integer(c(1, p)))
    
    testthat::expect_identical(formatClusters(clusters=3L:p, p=p,
                                              get_prototypes=TRUE, x=X,
                                              y=y)$prototypes,
                               as.integer(c(p, 1, 2)))
    
    y2 <- rnorm(n)

    res <- formatClusters(clusters=list(2:3, 4:5), p=p, get_prototypes=TRUE,
                          x=X, y=y2)$prototypes

    testthat::expect_true(is.integer(res))

    testthat::expect_equal(length(res), 4)

    testthat::expect_true(res[1] %in% c(2L, 3L))

    testthat::expect_true(res[2] %in% c(4L, 5L))
    
    testthat::expect_equal(res[3], 1L)
    
    testthat::expect_equal(res[4], p)

    testthat::expect_error(formatClusters(clusters=list(2:3, 4:5), p=p,
                                          get_prototypes=TRUE, x=y2, y=X),
                           "is.matrix(x) is not TRUE", fixed=TRUE)

    testthat::expect_error(formatClusters(clusters=list(2:3, 4:5), p=p,
                                          get_prototypes=TRUE, x=X,
                                          y=y2[1:(n-1)]),
                           "n == length(y) is not TRUE", fixed=TRUE)
})
```

```
## Test passed 😀
```

`checkSamplingType()`:


```r
#' Helper function to confirm that the argument sampling_type to several 
#' functions is as expected
#'
#' @param sampling_type A character vector; either "SS" or "MB". "MB" is not
#' supported yet.
#' @author Gregory Faletto, Jacob Bien
checkSamplingType <- function(sampling_type){
    stopifnot(is.character(sampling_type))
    stopifnot(length(sampling_type) == 1)
    stopifnot(!is.na(sampling_type))
    stopifnot(sampling_type %in% c("SS", "MB"))
    if(sampling_type == "MB"){
        stop("sampling_type MB is not yet supported (and isn't recommended anyway)")
    }
}
```

Tests for `checkSamplingType()`:


```r
testthat::test_that("checkSamplingType works", {
  testthat::expect_null(checkSamplingType("SS"))
  testthat::expect_error(checkSamplingType("MB"),
                         "sampling_type MB is not yet supported (and isn't recommended anyway)",
                         fixed=TRUE)
  testthat::expect_error(checkSamplingType(c("SS", "SS")),
                         "length(sampling_type) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkSamplingType(1),
                         "is.character(sampling_type) is not TRUE", fixed=TRUE)
  testthat::expect_error(checkSamplingType(as.character(NA)),
                         "!is.na(sampling_type) is not TRUE", fixed=TRUE)
})
```

```
## Test passed 🥇
```


`checkB()`:


```r
#' Helper function to confirm that the argument B to several functions is as
#' expected
#'
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the total number of subsamples will be `B`; for
#' sampling_type="SS" the number of subsamples will be `2*B`.
#' @author Gregory Faletto, Jacob Bien
checkB <- function(B){
    stopifnot(length(B) == 1)
    stopifnot(is.numeric(B) | is.integer(B))
    stopifnot(!is.na(B))
    stopifnot(B == round(B))
    stopifnot(B > 0)
    if(B < 10){
        warning("Small values of B may lead to poor results.")
    } else if (B > 2000){
        warning("Large values of B may require long computation times.")
    }
}
```

Tests for `checkB()`:


```r
testthat::test_that("checkB works", {
  testthat::expect_null(checkB(1500))
  testthat::expect_null(checkB(15))
  testthat::expect_error(checkB("B"),
                         "is.numeric(B) | is.integer(B) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkB(20:25), "length(B) == 1 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkB(as.integer(NA)), "!is.na(B) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkB(1.2), "B == round(B) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkB(-100), "B > 0 is not TRUE",
                         fixed=TRUE)
  testthat::expect_warning(checkB(5),
                           "Small values of B may lead to poor results.",
                           fixed=TRUE)
  testthat::expect_warning(checkB(2200),
                           "Large values of B may require long computation times.",
                           fixed=TRUE)
})
```

```
## Test passed 🎉
```


`checkPropFeatsRemove()`:


```r
#' Helper function to confirm that the argument prop_feats_remove to several 
#' functions is as expected
#'
#' @param prop_feats_remove Numeric; proportion of features that are dropped on
#' each subsample. Must be between 0 and 1.
#' @param p The number of features; must be greater than 2 if prop_feats_remove
#' is greater than 0.
#' @author Gregory Faletto, Jacob Bien
checkPropFeatsRemove <- function(prop_feats_remove, p){
    stopifnot(length(prop_feats_remove) == 1)
    stopifnot(is.numeric(prop_feats_remove) | is.integer(prop_feats_remove))
    stopifnot(!is.na(prop_feats_remove))
    stopifnot(prop_feats_remove >= 0 & prop_feats_remove < 1)
    if(prop_feats_remove > 0){
        # Make sure p is at least 2 or else this doesn't make sense
        stopifnot(p >= 2)
    }
}
```

Tests for `checkPropFeatsRemove()`:


```r
testthat::test_that("checkPropFeatsRemove works", {
  testthat::expect_null(checkPropFeatsRemove(0, 5))
  testthat::expect_null(checkPropFeatsRemove(.3, 10))
  testthat::expect_error(checkPropFeatsRemove(1, 3),
                         "prop_feats_remove >= 0 & prop_feats_remove < 1 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkPropFeatsRemove(c(.5, .6), 17),
                         "length(prop_feats_remove) == 1 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkPropFeatsRemove(".3", 99),
                         "is.numeric(prop_feats_remove) | is.integer(prop_feats_remove) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkPropFeatsRemove(as.numeric(NA), 172),
                         "!is.na(prop_feats_remove) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkPropFeatsRemove(.1, 1),
                         "p >= 2 is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 🎉
```

Finally, tests for `checkCssInputs()`:


```r
testthat::test_that("checkCssInputs works", {
  set.seed(80526)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:5L,
                        green_cluster=6L:8L
                        # , c4=10:11
                        )
  
  res <- checkCssInputs(X=x, y=y, lambda=0.01, clusters=good_clusters,
                        fitfun = cssLasso, sampling_type = "SS", B = 13,
                        prop_feats_remove = 0, train_inds = integer(),
                        num_cores = 1L)
  
  # Basic output
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("feat_names", "X", "clusters"))
  
  # feat_names
  testthat::expect_true(is.character(res$feat_names))
  testthat::expect_true(is.na(res$feat_names))
  testthat::expect_equal(length(res$feat_names), 1)

  # X
  testthat::expect_true(is.matrix(res$X))
  testthat::expect_true(all(!is.na(res$X)))
  testthat::expect_true(is.numeric(res$X))
  testthat::expect_equal(ncol(res$X), 11)
  testthat::expect_equal(nrow(res$X), 15)

  # clusters
  testthat::expect_true(is.list(res$clusters))
  testthat::expect_equal(length(res$clusters), length(names(res$clusters)))
  testthat::expect_equal(length(res$clusters),
                         length(unique(names(res$clusters))))
  testthat::expect_true(all(!is.na(names(res$clusters))))
  testthat::expect_true(all(!is.null(names(res$clusters))))
  clust_feats <- integer()
  for(i in 1:length(res$clusters)){
    clust_feats <- c(clust_feats, res$clusters[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:11)))

  ## Trying other inputs

  # Custom fitfun with nonsense lambda (which will be ignored by fitfun, and
  # shouldn't throw any error, because the acceptable input for lambda should be
  # enforced only by fitfun)

  testFitfun <- function(X, y, lambda){
    p <- ncol(X)
    stopifnot(p >= 2)
    # Choose p/2 features randomly
    selected <- sample.int(p, size=floor(p/2))
    return(selected)
  }

  res_fitfun <- checkCssInputs(X=x, y=y, lambda=x, clusters=1:3,
                               fitfun = testFitfun, sampling_type = "SS",
                               B = 13, prop_feats_remove = 0,
                               train_inds = integer(), num_cores = 1L)
  testthat::expect_true(is.list(res_fitfun))

  # Single cluster
  res_sing_clust <- checkCssInputs(X=x, y=y,
                                   lambda=c("foo", as.character(NA), "bar"),
                                   clusters=1:3, fitfun = testFitfun,
                                   sampling_type = "SS", B = 13,
                                   prop_feats_remove = 0,
                                   train_inds = integer(), num_cores = 1L)
  testthat::expect_true(is.list(res_sing_clust))
  testthat::expect_equal(length(res_sing_clust$clusters), 11 - 3 + 1)
  testthat::expect_true(length(unique(names(res_sing_clust$clusters))) == 11 -
                          3 + 1)
  testthat::expect_true(all(!is.na(names(res_sing_clust$clusters))))
  testthat::expect_true(all(!is.null(names(res_sing_clust$clusters))))

  # Other sampling types
  testthat::expect_error(checkCssInputs(X=x, y=y, lambda=c("foo",
                                                           as.character(NA),
                                                           "bar"), clusters=1:3,
                                        fitfun = testFitfun,
                                        sampling_type = "MB", B = 13,
                                        prop_feats_remove = 0,
                                        train_inds = integer(), num_cores = 1L),
                         "sampling_type MB is not yet supported (and isn't recommended anyway)",
                         fixed=TRUE)

  # Error has quotation marks in it
  testthat::expect_error(checkCssInputs(X=x, y=y, lambda=c("foo",
                                                           as.character(NA),
                                                           "bar"), clusters=1:3,
                                        fitfun = testFitfun,
                                        sampling_type = "S", B = 13,
                                        prop_feats_remove = 0,
                                        train_inds = integer(), num_cores = 1L))
  
  testthat::expect_error(checkCssInputs(X=x, y=y, lambda=c("foo", "bar",
                                                           as.character(NA)),
                                        clusters=1:3, fitfun = testFitfun,
                                        sampling_type = 2, B = 13,
                                        prop_feats_remove = 0,
                                        train_inds = integer(), num_cores = 1L),
                         "is.character(sampling_type) is not TRUE",
                         fixed=TRUE)

  # B
  testthat::expect_warning(checkCssInputs(X=x, y=y, lambda=c("foo", "bar",
                                                           as.character(NA)),
                                        clusters=1:3, fitfun = testFitfun,
                                        sampling_type = "SS", B = 5,
                                        prop_feats_remove = 0,
                                        train_inds = integer(), num_cores = 1L),
                           "Small values of B may lead to poor results.",
                           fixed=TRUE)

  testthat::expect_error(checkCssInputs(X=x, y=y, lambda=c("foo", "bar",
                                                           as.character(NA)),
                                        clusters=1:3, fitfun = testFitfun,
                                        sampling_type = "SS", B = "foo",
                                        prop_feats_remove = 0,
                                        train_inds = integer(), num_cores = 1L),
                           "is.numeric(B) | is.integer(B) is not TRUE",
                           fixed=TRUE)

  # prop_feats_remove
  testthat::expect_true(is.list(checkCssInputs(X=x, y=y,
                                               lambda=c("foo", "bar",
                                                        as.character(NA)),
                                               clusters=1:3, fitfun=testFitfun,
                                               sampling_type = "SS", B = 12,
                                               prop_feats_remove = 0.3,
                                               train_inds = integer(),
                                               num_cores = 1L)))

  # Use train_inds argument
  testthat::expect_true(is.list(checkCssInputs(X=x, y=y,
                                               lambda=c("foo", "bar",
                                                        as.character(NA)),
                                               clusters=1:3, fitfun=testFitfun,
                                               sampling_type = "SS", B = 12,
                                               prop_feats_remove = 0.3,
                                               train_inds = 11:15,
                                               num_cores = 1L)))

})
```

```
## Test passed 🥳
```


`cssLoop()`:


```r
#' Helper function run on each subsample
#' 
#' Runs provided feature selection method `fitfun` on each subsample for cluster
#' stability selection (this function is called within `mclapply`).
#' @param input Could be one of two things: \item{subsample}{An integer vector
#' of size `n/2` containing the indices of the observations in the subsample.}
#' \item{drop_var_input}{A named list containing two elements: one named
#' "subsample" and the same as the previous description, and a logical vector
#' named "feats_to_keep" containing the indices of the features to be
#' automatically selected.} (The first object is the output of the function
#' `createSubsamples()` when the provided `prop_feats_remove` is 0, the default, and
#' the second object is the output of `createSubsamples()` when `prop_feats_remove >
#' 0`.)
#' @param x an n x p numeric matrix containing the predictors. (This should be
#' the full design matrix provided to css.)
#' @param y A response; can be any response that takes the form of a length n
#' vector and is used (or not used) by `fitfun`. Typically (and for default
#' `fitfun = cssLasso`), `y` should be an n-dimensional numeric vector containing the
#' response. This should be the full response provided to css.
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method. For example, in the default case when
#' `fitfun = cssLasso`, `lambda` is a numeric: the penalty to use for each lasso
#' fit.
#' @param fitfun A function that takes in arguments X, y, and lambda and returns
#' a vector of indices of the columns of X (selected features).
#' @return An integer vector; the indices of the features selected by `fitfun`.
#' @author Gregory Faletto, Jacob Bien
cssLoop <- function(input, x, y, lambda, fitfun){
    # Check inputs
    stopifnot(is.matrix(x))
    stopifnot(all(!is.na(x)))

    colnames(x) <- character()
    n <- nrow(x)
    p <- ncol(x)

    stopifnot(length(y) == n)
    stopifnot(!is.matrix(y))
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    if(!is.list(input)){
        subsample <- input
        feats_to_keep <- rep(TRUE, p)
    } else{
        stopifnot(all(names(input) == c("subsample", "feats_to_keep")))
        subsample <- input$subsample
        feats_to_keep <- input$feats_to_keep
    }

    stopifnot(is.integer(subsample))
    stopifnot(all(subsample == round(subsample)))
    stopifnot(floor(n/2) == length(subsample))
    stopifnot(length(subsample) == length(unique(subsample)))

    stopifnot(is.logical(feats_to_keep))
    stopifnot(length(feats_to_keep) == p)

    selected <- do.call(fitfun, list(X=x[subsample, feats_to_keep],
        y=y[subsample], lambda=lambda))

    selected <- which(feats_to_keep)[selected]

    # Check output
    checkCssLoopOutput(selected, p, as.integer(which(feats_to_keep)))

    return(as.integer(selected))
}
```

`checkCssLoopOutput()`:


```r
#' Helper function to confirm that the outputs of the provided feature selection
#' method are as required. 
#'
#' @param selected An integer vector; the indices of the features selected by
#' the lasso.
#' @param p The total number of observed features; all selected features must be
#' in 1:p.
#' @param feats_on_subsamp Integer; the indices of the features considered by
#' the feature selection method. All selected features must be among these
#' features.
#' @author Gregory Faletto, Jacob Bien
checkCssLoopOutput <- function(selected, p, feats_on_subsamp){
    if(!exists("selected")){
        stop("The provided feature selection method fitfun failed to return anything on (at least) one subsample")
    }
    if(!is.integer(selected) & !is.numeric(selected)){
        stop("The provided feature selection method fitfun failed to return an integer or numeric vector on (at least) one subsample")
    }
    if(any(is.na(selected))){
        stop("The provided feature selection method fitfun returned a vector containing NA values on (at least) one subsample")
    }
    if(!all(selected == round(selected))){
        stop("The provided feature selection method fitfun failed to return a vector of valid (integer) indices on (at least) one subsample")
    }
    if(length(selected) != length(unique(selected))){
        stop("The provided feature selection method fitfun returned a vector of selected features containing repeated indices on (at least) one subsample")
    }
    if(length(selected) > p){
        stop("The provided feature selection method fitfun returned a vector of selected features longer than p on (at least) one subsample")
    }
    if(length(selected) > 0){
        if(max(selected) > p){
            stop("The provided feature selection method fitfun returned a vector of selected features containing an index greater than ncol(X) on (at least) one subsample")
        }
        if(min(selected) <= 0){
            stop("The provided feature selection method fitfun returned a vector of selected features containing a non-positive index on (at least) one subsample")
        }
    }
    if("try-error" %in% class(selected) |
        "error" %in% class(selected) | "simpleError" %in% class(selected) |
        "condition" %in% class(selected)){
        stop("The provided feature selection method fitfun returned an error on (at least) one subsample")
    }
    if(!all(selected %in% feats_on_subsamp)){
        stop("The provided feature selection method somehow selected features that were not provided for it to consider.")
    }
}
```

Tests for `checkCssLoopOutput()`:


```r
testthat::test_that("checkCssLoopOutput works", {
  testthat::expect_null(checkCssLoopOutput(selected=1:5, p=6,
                                           feats_on_subsamp=1:6))
  
  testthat::expect_error(checkCssLoopOutput(selected=1:5, p=4,
                                            feats_on_subsamp=1:6),
                         "The provided feature selection method fitfun returned a vector of selected features longer than p on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=1:5, p=7,
                                            feats_on_subsamp=1:4),
                         "The provided feature selection method somehow selected features that were not provided for it to consider.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=c(1, 2, 3, 4.4, 5), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun failed to return a vector of valid (integer) indices on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=rep(1, 3), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun returned a vector of selected features containing repeated indices on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=c(-1, 5), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun returned a vector of selected features containing a non-positive index on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=c(0, 5), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun returned a vector of selected features containing a non-positive index on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=as.integer(NA), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun returned a vector containing NA values on (at least) one subsample",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLoopOutput(selected=c("1", "2", "3"), p=7,
                                            feats_on_subsamp=1:7),
                         "The provided feature selection method fitfun failed to return an integer or numeric vector on (at least) one subsample",
                         fixed=TRUE)

})
```

```
## Test passed 🌈
```




`checkCssLassoInputs()`:


```r
#' Helper function to confirm that the inputs to `cssLasso()` are as expected. 
#'
#' @param X A design matrix containing the predictors. (Note that we don't need
#' to check X very much, because X will have already been checked by the
#' function `checkCssInputs()` when it was provided to `css()`.)
#' @param y A numeric vector containing the response.
#' @param lambda Numeric; a nonnegative number for the lasso penalty to use
#' on each subsample. (For now, only one lambda value can be provided to
#' `cssLasso()`; in the future, we plan to allow for multiple lambda values to be
#' provided to `cssLasso()`, as described in Faletto and Bien 2022.)
#' @author Gregory Faletto, Jacob Bien
checkCssLassoInputs <- function(X, y, lambda){

    n <- nrow(X)
    p <- ncol(X)

    if(!is.numeric(y)){
        stop("For method cssLasso, y must be a numeric vector.")
    }
    if(is.matrix(y)){
        stop("For method cssLasso, y must be a numeric vector (inputted y was a matrix).")
    }
    if(n != length(y)){
        stop("For method cssLasso, y must be a vector of length equal to nrow(X).")
    }
    if(length(unique(y)) <= 1){
        stop("Subsample with only one unique value of y detected--for method cssLasso, all subsamples of y of size floor(n/2) must have more than one unique value.")
    }
    if(!is.numeric(lambda) & !is.integer(lambda)){
        stop("For method cssLasso, lambda must be a numeric.")
    }
    if(any(is.na(lambda))){
        stop("NA detected in provided lambda input to cssLasso")
    }
    if(length(lambda) != 1){
        stop("For method cssLasso, lambda must be a numeric of length 1.")
    }
    if(lambda < 0){
        stop("For method cssLasso, lambda must be nonnegative.")
    }
}
```

Tests for `checkCssLassoInputs()`:


```r
testthat::test_that("checkCssLassoInputs works", {
  set.seed(761)
  
  x <- matrix(stats::rnorm(15*4), nrow=15, ncol=4)
  y <- stats::rnorm(15)
  
  testthat::expect_null(checkCssLassoInputs(X=x, y=y, lambda=0.01))
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=logical(15), lambda=0.05),
                         "For method cssLasso, y must be a numeric vector.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x[1:13, ], y=y, lambda=0.01),
                         "For method cssLasso, y must be a vector of length equal to nrow(X).",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=rep(1.2, 15), lambda=0.05),
                         "Subsample with only one unique value of y detected--for method cssLasso, all subsamples of y of size floor(n/2) must have more than one unique value.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=TRUE),
                         "For method cssLasso, lambda must be a numeric.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=as.numeric(NA)),
                         "NA detected in provided lambda input to cssLasso",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=-0.01),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)

  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=x),
                         "For method cssLasso, lambda must be a numeric of length 1.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=numeric()),
                         "For method cssLasso, lambda must be a numeric of length 1.",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=-0.01),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)
  
})
```

```
## Test passed 🌈
```


Tests for `cssLoop()` (had to define `checkCssLassoInputs()` first):


```r
testthat::test_that("cssLoop works", {
  set.seed(89134)

  x <- matrix(stats::rnorm(9*8), nrow=9, ncol=8)
  y <- stats::rnorm(9)

  output <- cssLoop(input=1L:4L, x=x, y=y, lambda=0.05,
                    fitfun=cssLasso)

  testthat::expect_true(is.integer(output))

  testthat::expect_equal(length(output), length(unique(output)))

  testthat::expect_true(length(output) <= 8)

  testthat::expect_true(all(output >= 1))

  testthat::expect_true(all(output <= 8))

  testthat::expect_error(cssLoop(input=1L:6L, x=x, y=y, lambda=0.05,
                                 fitfun=cssLasso),
                         "floor(n/2) == length(subsample) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=1L:4L, x=x, y=y[1:8],
                                 lambda=0.05, fitfun=cssLasso),
                         "length(y) == n is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=1L:4L, x=x, y=logical(9),
                                 lambda=0.05, fitfun=cssLasso),
                         "For method cssLasso, y must be a numeric vector.",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=1L:4L, x=x, y=y,
                                 lambda=x, fitfun=cssLasso),
                         "For method cssLasso, lambda must be a numeric of length 1.",
                         fixed=TRUE)

  # Test other input format

  alt_input <- list("subsample"=2:5, "feats_to_keep"=c(FALSE, rep(TRUE, 4),
                                                       rep(FALSE, 2), TRUE))

  output2 <- cssLoop(input=alt_input, x=x, y=y, lambda=0.08, fitfun=cssLasso)

  testthat::expect_true(is.integer(output2))

  testthat::expect_equal(length(output2), length(unique(output2)))

  testthat::expect_true(length(output2) <= 8)

  testthat::expect_true(all(output2 %in% c(2, 3, 4, 5, 8)))

  testthat::expect_error(cssLoop(input= list("subsample"=2:5,
                                             "feats_to_keep"=c(FALSE,
                                                               rep(TRUE, 4),
                                                               rep(FALSE, 2))),
                                 x=x, y=y, lambda=0.08, fitfun=cssLasso),
                         "length(feats_to_keep) == p is not TRUE",
                         fixed=TRUE)

  # Custom fitfun with nonsense lambda (which will be ignored by fitfun, and
  # shouldn't throw any error, because the acceptable input for lambda should be
  # enforced only by fitfun) and nonsense y

  testFitfun <- function(X, y, lambda){
    p <- ncol(X)
    stopifnot(p >= 2)
    # Choose p/2 features randomly
    selected <- sample.int(p, size=floor(p/2))
    return(selected)
  }

  testthat::expect_true(is.integer(cssLoop(input=1L:4L, x=x, y=y,
                                           lambda=TRUE, fitfun=testFitfun)))

  testthat::expect_true(is.integer(cssLoop(input=1L:4L, x=x,
                                           y=character(9), lambda=.05,
                                           fitfun=testFitfun)))

})
```

```
## Test passed 🎊
```


`checkGetClusterSelMatrixInput()`:


```r
#' Helper function to check inputs to getClusterSelMatrix function
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of formatClusters.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @param res A binary integer matrix. res[i, j] = 1 if feature j was selected
#' on subsample i and equals 0 otherwise, as in the output of getSelMatrix.
#' (That is, each row is a selected set.)
#' @return The parameter B, corresponding to half of the subsamples for 
#' sampling_type "SS".
#' @author Gregory Faletto, Jacob Bien
checkGetClusterSelMatrixInput <- function(clusters, res){
    stopifnot(is.matrix(res))
    stopifnot(all(res %in% c(0, 1)))
    p <- ncol(res)
    stopifnot(nrow(res) > 0)
    checkClusters(clusters, p)
}
```


Tests for `checkGetClusterSelMatrixInput()`:


```r
testthat::test_that("checkGetClusterSelMatrixInput works", {
  
  good_clusters <- list(happy=1L:8L, sad=9L:10L, med=11L)
  
  res <- matrix(sample(c(0, 1), size=6*11, replace=TRUE), nrow=6, ncol=11)
  
  testthat::expect_null(checkGetClusterSelMatrixInput(good_clusters, res))
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(happy=1L:8L,
                                                            med=11L), res),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(good_clusters, 1:9),
                         "is.matrix(res) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(good_clusters, res + .3),
                         "all(res %in% c(0, 1)) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(good_clusters,
                                                       res[, 1:9]),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(1L:10L, res),
                         "is.list(clusters) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=1L:5L,
                                                            c2=6L:8L,
                                                            c3=9L,
                                                            c4=integer()), res),
                         "all(lengths(clusters) >= 1) is not TRUE",
                         fixed=TRUE)
                         
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=1L:5L,
                                            c2=6L:8L, c3=9L,
                                            c4=as.integer(NA)), res),
                         "all(!is.na(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=1L:5L,
                                            c2=6L:8L, c3=9L,
                                            c2=6L:8L), res),
                         "n_clusters == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=1L:5L,
                                            c2=6L:8L, c3=14L), res),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)
  
})
```

```
## Test passed 🎉
```

# Tests for main functions in `cssr`

Now that the helper functions have been defined, we move on to tests for the main functions in the package.

Tests for `createSubsamples()`:


```r
testthat::test_that("createSubsamples works", {
  res <- createSubsamples(n=20L, p=5L, B=11L, sampling_type="SS",
                          prop_feats_remove=0)
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2*11)
  testthat::expect_true(all(lengths(res) == 20/2))
  testthat::expect_equal(length(unique(res[[13]])), 20/2)

  set <- res[[4]]
  comp_set <- res[[4 + 11]]

  testthat::expect_equal(length(intersect(set, comp_set)), 0)
  testthat::expect_equal(length(union(set, comp_set)), length(c(set, comp_set)))
  testthat::expect_equal(20, length(c(set, comp_set)))

  # Try odd n

  res_odd <- createSubsamples(n=19L, p=23L, B=13L, sampling_type="SS",
                              prop_feats_remove=0)
  testthat::expect_true(is.list(res_odd))
  testthat::expect_equal(length(res_odd), 2*13)
  testthat::expect_true(all(lengths(res_odd) == floor(19/2)))
  testthat::expect_equal(length(unique(res_odd[[3]])), floor(19/2))

  set_odd <- res_odd[[2]]
  comp_set_odd <- res_odd[[2 + 13]]

  testthat::expect_equal(length(intersect(set_odd, comp_set_odd)), 0)
  testthat::expect_equal(length(union(set_odd, comp_set_odd)),
                         length(c(set_odd, comp_set_odd)))
  testthat::expect_equal(19 - 1, length(c(set_odd, comp_set_odd)))

  testthat::expect_error(createSubsamples(n=20L, p=5L, B=11L, sampling_type="MB",
                          prop_feats_remove=0),
                         "sampling_type MB is not yet supported (and isn't recommended anyway)",
                         fixed=TRUE)
  # misspecified sampling_type (not specifying error because contains quotation
  # marks)
  testthat::expect_error(createSubsamples(n=20L, p=5L, B=11L, sampling_type="S",
                          prop_feats_remove=0))
  testthat::expect_error(createSubsamples(n=11.1, p=5L, B=11L, sampling_type="SS",
                          prop_feats_remove=0),
                         "n == round(n) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(createSubsamples(n=-20L, p=5L, B=11L, sampling_type="SS",
                          prop_feats_remove=0),
                         "n > 0 is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(createSubsamples(n=20L, p=5L, B=25.6, sampling_type="SS",
                          prop_feats_remove=0),
                         "length(subsamples) == B is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 😸
```

Tests for `getSubsamps()`:


```r
testthat::test_that("getSubsamps works", {
  res <- getSubsamps(n=18L, B=21L, sampling_type="SS")
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2*21)
  testthat::expect_true(all(lengths(res) == 18/2))
  testthat::expect_equal(length(unique(res[[7]])), 18/2)

  set <- res[[3]]
  comp_set <- res[[3 + 21]]

  testthat::expect_equal(length(intersect(set, comp_set)), 0)
  testthat::expect_equal(length(union(set, comp_set)), length(c(set, comp_set)))
  testthat::expect_equal(18, length(c(set, comp_set)))
})
```

```
## Test passed 😸
```

Tests for `getSelMatrix()`:


```r
testthat::test_that("getSelMatrix works", {
  set.seed(98623)
  x <- matrix(stats::rnorm(25*6), nrow=25, ncol=6)
  y <- stats::rnorm(25)
  subsamps_object <- createSubsamples(n=25, p=6, B=12, sampling_type="SS",
                                      prop_feats_remove=0)
  res <- getSelMatrix(x=x, y=y, lambda=0.01, B=12, sampling_type="SS",
                      subsamps_object=subsamps_object, num_cores=1,
                      fitfun=cssLasso)

  testthat::expect_true(is.matrix(res))
  testthat::expect_equal(nrow(res), 2*12)
  testthat::expect_equal(ncol(res), 6)
  testthat::expect_true(all(res %in% c(0, 1)))
  testthat::expect_true(all(is.integer(res)))

  # Try a different fitfun

  testFitfun <- function(X, y, lambda){
    p <- ncol(X)
    stopifnot(p >= 2)
    # Choose p/2 features randomly
    selected <- sample.int(p, size=floor(p/2))
    return(selected)
  }
  
  # Note that value of lambda doesn't matter
  res2 <- getSelMatrix(x=x, y=y, lambda="foo", B=12, sampling_type="SS",
                      subsamps_object=subsamps_object, num_cores=1,
                      fitfun=testFitfun)

  testthat::expect_true(is.matrix(res2))
  testthat::expect_equal(nrow(res2), 2*12)
  testthat::expect_equal(ncol(res2), 6)
  testthat::expect_true(all(res2 %in% c(0, 1)))
  testthat::expect_true(all(is.integer(res2)))
  
  testthat::expect_error(getSelMatrix(x=x, y=y, lambda="0.02", B=12, sampling_type="SS",
                      subsamps_object="subsamps_object", num_cores=1,
                      fitfun=testFitfun),
                      "is.integer(subsample) is not TRUE",
                      fixed=TRUE)
  testthat::expect_error(getSelMatrix(x=x[1:8, ], y=y, lambda="foo", B=12, sampling_type="SS",
                      subsamps_object=subsamps_object, num_cores=1,
                      fitfun=testFitfun),
                      "length(y) == n is not TRUE",
                      fixed=TRUE)
  testthat::expect_error(getSelMatrix(x=x, y=y, lambda=-0.02, B=12, sampling_type="SS",
                      subsamps_object=subsamps_object, num_cores=1,
                      fitfun=cssLasso),
                      "For method cssLasso, lambda must be nonnegative.",
                      fixed=TRUE)
  # Wrong B
  testthat::expect_error(getSelMatrix(x=x, y=y, lambda=0.02, B=37, sampling_type="SS",
                      subsamps_object=subsamps_object, num_cores=1,
                      fitfun=cssLasso),
                      "length(res_list) == nrow(res) is not TRUE",
                      fixed=TRUE)
})
```

```
## Test passed 🎉
```

Tests for `cssLasso()`:


```r
testthat::test_that("cssLasso works", {
  set.seed(24509)
  x <- matrix(stats::rnorm(15*4), nrow=15, ncol=4)
  y <- stats::rnorm(15)
  
  res <- cssLasso(X=x, y=y, lambda=0.01)
  
  testthat::expect_true(is.integer(res))
  testthat::expect_true(length(res) <= 4)
  testthat::expect_true(length(res) >= 0)
  testthat::expect_true(length(res) == length(unique(res)))
  testthat::expect_true(all(res <= 4))
  testthat::expect_true(all(res >= 1))
  
  testthat::expect_error(cssLasso(X=x[1:13, ], y=y, lambda=0.01),
                         "For method cssLasso, y must be a vector of length equal to nrow(X).",
                         fixed=TRUE)
  testthat::expect_error(cssLasso(X=x, y=y, lambda=-0.01),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)

})
```

```
## Test passed 😀
```

Tests for `getClusterSelMatrix()`:


```r
testthat::test_that("getClusterSelMatrix works", {
  good_clusters <- list(red_cluster=1L:5L,
                        green_cluster=6L:8L, blue_clust=9L)
  
  B <- 14
  p <- 9
  
  res_entries <- as.integer(sample(c(0, 1), size=2*B*p, replace=TRUE))
  
  good_res <- matrix(res_entries, nrow=2*B, ncol=p)
  
  res <- getClusterSelMatrix(good_clusters, good_res)
  
  testthat::expect_true(is.matrix(res))
  testthat::expect_equal(nrow(res), 2*B)
  # 3 clusters
  testthat::expect_equal(ncol(res), 3)
  testthat::expect_identical(colnames(res), c("red_cluster", "green_cluster",
                                              "blue_clust"))
  testthat::expect_true(all(is.integer(res)))
  testthat::expect_true(all(res %in% c(0, 1)))

  clust_2 <- good_clusters[[2]]
  
  any_one <- rowSums(good_res[, clust_2]) > 0
  if(any(any_one)){
    testthat::expect_true(all(res[any_one, 2] == 1))
  }

  all_zeros <- rowSums(good_res[, clust_2]) == 0
  if(any(all_zeros)){
    testthat::expect_true(all(res[all_zeros, 2] == 0))
  }

  # Not all features in a cluster
  bad_clusters <- list(red_cluster=1L:5L, green_cluster=6L:7L, blue_clust=9L)

  testthat::expect_error(getClusterSelMatrix(bad_clusters, good_res),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)

  bad_res_entries <- as.integer(sample(c(0, 1, 2), size=2*B*p, replace=TRUE))

  bad_res <- matrix(bad_res_entries, nrow=2*B, ncol=p)

  testthat::expect_error(getClusterSelMatrix(good_clusters, bad_res),
                         "all(res %in% c(0, 1)) is not TRUE", fixed=TRUE)
})
```

```
## Test passed 🌈
```

Finally, tests for `css()` itself!


```r
testthat::test_that("css works", {
  set.seed(8712)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:5L,
                        green_cluster=6L:8L,
                        c4=10:11)
  
  res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, fitfun = cssLasso,
    sampling_type = "SS", B = 13,
    prop_feats_remove = 0, train_inds = integer(), num_cores = 1L)
  
  # Basic output
  testthat::expect_true(is.list(res))
  testthat::expect_identical(class(res), "cssr")
  testthat::expect_identical(names(res), c("feat_sel_mat", "clus_sel_mat", "X",
                                           "y", "clusters", "train_inds"))
  
  # feat_sel mat
  testthat::expect_true(is.integer(res$feat_sel_mat))
  testthat::expect_true(is.matrix(res$feat_sel_mat))
  testthat::expect_true(all(res$feat_sel_mat %in% c(0, 1)))
  testthat::expect_equal(ncol(res$feat_sel_mat), 11)
  testthat::expect_null(colnames(res$feat_sel_mat))

  # clus_sel_mat
  testthat::expect_true(is.integer(res$clus_sel_mat))
  testthat::expect_true(is.matrix(res$clus_sel_mat))
  testthat::expect_true(all(res$clus_sel_mat %in% c(0, 1)))
  # 4 clusters
  testthat::expect_equal(ncol(res$clus_sel_mat), 4)
  testthat::expect_identical(colnames(res$clus_sel_mat), names(res$clusters))
  testthat::expect_equal(length(colnames(res$clus_sel_mat)), 4)
  testthat::expect_equal(length(unique(colnames(res$clus_sel_mat))), 4)
  testthat::expect_true(all(!is.na(colnames(res$clus_sel_mat))))
  testthat::expect_true(all(!is.null(colnames(res$clus_sel_mat))))

  # X
  testthat::expect_true(is.matrix(res$X))
  testthat::expect_true(all(!is.na(res$X)))
  testthat::expect_true(is.numeric(res$X))
  testthat::expect_equal(ncol(res$X), 11)
  testthat::expect_equal(nrow(res$X), 15)

  # y
  testthat::expect_true(is.numeric(res$y))
  testthat::expect_equal(length(res$y), 15)

  # clusters
  testthat::expect_true(is.list(res$clusters))
  testthat::expect_equal(length(res$clusters), length(names(res$clusters)))
  testthat::expect_equal(length(res$clusters),
                         length(unique(names(res$clusters))))
  testthat::expect_true(all(!is.na(names(res$clusters))))
  testthat::expect_true(all(!is.null(names(res$clusters))))
  clust_feats <- integer()
  for(i in 1:length(res$clusters)){
    clust_feats <- c(clust_feats, res$clusters[[i]])
  }
  testthat::expect_equal(length(clust_feats), length(unique(clust_feats)))
  testthat::expect_equal(length(clust_feats), length(intersect(clust_feats,
                                                               1:11)))
  # train_inds
  testthat::expect_identical(res$train_inds, integer())

  ## Trying other inputs
  
  # X as a data.frame
  X_df <- datasets::mtcars
  res_fitfun <- css(X=X_df, y=stats::rnorm(nrow(X_df)), lambda=0.01, B = 10)
  testthat::expect_identical(class(res_fitfun), "cssr")
  
  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  res_fitfun <- css(X=df2, y=stats::rnorm(nrow(X_df)), lambda=0.01, B = 10)
  testthat::expect_identical(class(res_fitfun), "cssr")
  
  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]
  res_names <- css(X=x2, y=y, lambda=0.01, clusters=good_clusters, B = 13)
  testthat::expect_identical(class(res_names), "cssr")
  testthat::expect_identical(colnames(x2), colnames(res_names$X))
  testthat::expect_identical(colnames(x2), colnames(res_names$feat_sel_mat))

  # Custom fitfun with nonsense lambda (which will be ignored by fitfun, and
  # shouldn't throw any error, because the acceptable input for lambda should be
  # enforced only by fitfun)

  testFitfun <- function(X, y, lambda){
    p <- ncol(X)
    stopifnot(p >= 2)
    # Choose p/2 features randomly
    selected <- sample.int(p, size=floor(p/2))
    return(selected)
  }

  res_fitfun <- css(X=x, y=y, lambda=c("foo", as.character(NA), "bar"),
                    clusters=1:3, B = 10, fitfun=testFitfun)
  testthat::expect_identical(class(res_fitfun), "cssr")

  # Bad lambda
  testthat::expect_error(css(X=x, y=y, lambda=-0.01, B = 10),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)

  testthat::expect_error(css(X=x, y=y, lambda="foo", B = 10),
                         "For method cssLasso, lambda must be a numeric.",
                         fixed=TRUE)

  # Single cluster
  res_sing_clust <- css(X=x, y=y, lambda=0.01, clusters=1:3, B = 10)
  testthat::expect_identical(class(res_sing_clust), "cssr")
  testthat::expect_equal(length(res_sing_clust$clusters), 11 - 3 + 1)
  testthat::expect_true(length(unique(names(res_sing_clust$clusters))) == 11 -
                          3 + 1)
  testthat::expect_true(all(!is.na(names(res_sing_clust$clusters))))
  testthat::expect_true(all(!is.null(names(res_sing_clust$clusters))))

  # No cluster
  testthat::expect_identical(class(css(X=x, y=y, lambda=0.01, B = 10)), "cssr")
  
  # All clusters named
  testthat::expect_identical(class(css(X=x, y=y, clusters=list("a"=1:5,
                                                               "b"=6:10,
                                                               "c"=11),
                                       lambda=0.01, B=10)), "cssr")

  # Other sampling types
  testthat::expect_error(css(X=x, y=y, lambda=1, sampling_type="MB"),
                         "sampling_type MB is not yet supported (and isn't recommended anyway)",
                         fixed=TRUE)

  # Error has quotation marks in it
  testthat::expect_error(css(X=x, y=y, lambda=1, sampling_type="S"))

  testthat::expect_error(css(X=x, y=y, lambda=1, sampling_type=1),
                         "is.character(sampling_type) is not TRUE",
                         fixed=TRUE)

  # B
  testthat::expect_warning(css(X=x, y=y, lambda=1, B=5),
                           "Small values of B may lead to poor results.",
                           fixed=TRUE)

  testthat::expect_error(css(X=x, y=y, lambda=1, B=list(10)),
                           "is.numeric(B) | is.integer(B) is not TRUE",
                           fixed=TRUE)
  
  # Clusters
  testthat::expect_error(css(X=x, y=y, lambda=1, clusters="red"),
                           "is.numeric(clusters) | is.integer(clusters) is not TRUE",
                           fixed=TRUE)

  # prop_feats_remove
  testthat::expect_identical(class(css(X=x, y=y, lambda=0.01, B = 10,
                                       prop_feats_remove=0.3)), "cssr")
  # Weirdly high, but still valid, value of prop_feats_remove
  testthat::expect_identical(class(css(X=x, y=y, lambda=0.01, B = 10,
                                       prop_feats_remove=0.9999999999)), "cssr")
  
  # Use train_inds argument
  res_train <- css(X=x, y=y, lambda=0.01, B = 10, train_inds=11:15)
  testthat::expect_equal(res_train$train_inds, 11:15)

})
```

```
## Test passed 🥳
```

# Selection and prediction functions {#sel-and-pred}

Now that `css()` has been defined and tested, we write functions to work with the output of `css()`. The output of these functions will be of more direct interest for most end users than the output of `css()`. These functions are defined separately from `css()` because the most computationally intensive steps happen within `css()`. `css()` can be called only once on a data set, and then the functions that follow can be explored relatively quickly (one can try different parameters, etc.).

* `getCssSelections()` takes in the results of `css()` along with user-defined parameters on how to select clusters (a minimum or maximum number of clusters to select, along with a cutoff for cluster selection proportions) and selects clusters as well as features from those clusters.
* `getCssDesign()` takes in the same inputs as `getCssSelections()` along with an unlabeled test matrix of features X (containing the same features as the X matrix provided originally to `css()`). It uses the results from `css()` to select clusters like `getCssSelections()`, then it uses a user-selected weighting scheme to compute weighted averages of the cluster members. It returns a test matrix of cluster representatives, which can be used for downstream predictive tasks.
* Finally, `getCssPreds()` has the same inputs as `getCssPreds()` *FIX THIS TYPO*, except it also accepts a set of labeled training data (where the response must be real-valued). `getCssPreds()` selects clusters, forms matrices of cluster representatives on the training and test data, uses the training matrix of cluster representatives (along with the vector of responses for the training data) to estimate a linear model via ordinary least squares, and finally generates predictions on the test data using this linear model.

As in the previous section, we first define each function and then define the helper functions called by that function. Tests are written for each function as soon as all of its dependencies have been defined.

* `getCssSelections()`
  - Calls `checkCutoff()`, which checks that the specified cutoff input is as expected
  - `checkWeighting()` verifies the weighting input to `getCssSelections()`
  - `checkMinNumClusts()` confirms that the `min_num_clusts` parameter provided to `getCssSelections()` is as expected
  - Likewise for `checkMaxNumClusts()`
  - `getSelectedClusters()` is the workhorse function of `getCssSelections()`, doing most of the work to get the selected clusters (and the selected features from those clusters) with the verified inputs
    * `checkSelectedClusters()` checks internally that the selected clusters are as expected
    * `getAllClustWeights()` gets the weights for each of the members of the selected clusters
      - `getClustWeights()` gets the weights for a single cluster
    * `checkGetSelectedClustersOutput()` verifies the output of `getSelectedClusters()`
* `getCssDesign()`
  - `checkNewXProvided()` confirms that the design matrix provided to `getCssDesign()` matches the characteristics of the matrix that was provided to `css()`. 
    * `checkXInputResults()` also verifies these inputs (and is also used by `getCssPreds()` on both the training and test data)
  - `formCssDesign()` is the workhorse function, generating a matrix of cluster representatives with the verified inputs to `getCssDesign()`
    * `checkFormCssDesignInputs()` verifies the inputs to `formCssDesign()` (somewhat redundantly here, but `formCssDesign()` is called by more than one function, so this verifies the inputs to `formCssDesign()` regardless of where it is called).
* `getCssPreds()`
  - `checkGetCssPredsInputs()` verifies the inputs to `getCssPreds()`

`getCssSelections()`: 


```r
#' Obtain a selected set of clusters and features
#'
#' Generate sets of selected clusters and features from cluster stability
#' selection.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details. Default is "sparse".
#' @param cutoff Numeric; getCssSelections will select and return only of those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1. Default is 0 (in which case either all clusters are selected, or
#' max_num_clusts are selected, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @return A named list with two items. \item{selected_clusts}{A named list of
#' integer vectors; each vector contains the indices of the features in one of
#' the selected clusters.} \item{selected_feats}{A named integer vector; the
#' indices of the features with nonzero weights from all of the selected
#' clusters.}
#' @author Gregory Faletto, Jacob Bien
#' @references 
<<faletto2022>>
#' @export
getCssSelections <- function(css_results, weighting="sparse", cutoff=0,
    min_num_clusts=1, max_num_clusts=NA){
    # Check inputs
    stopifnot(class(css_results) == "cssr")
    checkCutoff(cutoff)
    checkWeighting(weighting)

    p <- ncol(css_results$feat_sel_mat)

    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    sel_results <- getSelectedClusters(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts)

    # sel_results$selected_clusts is guaranteed to have length at least 1 by
    # getSelectedClusters
    sel_clust_names <- names(sel_results$selected_clusts)

    stopifnot(length(sel_clust_names) >= 1)
    stopifnot(all(sel_clust_names %in% names(css_results$clusters)))

    sel_clusts <- list()
    for(i in 1:length(sel_clust_names)){
        sel_clusts[[i]] <- css_results$clusters[[sel_clust_names[i]]]
        names(sel_clusts)[i] <- sel_clust_names[i]
    }

    stopifnot(is.list(sel_clusts))
    stopifnot(length(sel_clusts) == length(sel_clust_names))

    # sel_results$selected_feats is guaranteed to have length at least as long
    # as sel_results$selected_clusts by getSelectedClusters
    return(list(selected_clusts=sel_clusts,
        selected_feats=sel_results$selected_feats))
}
```

`checkCutoff()`:


```r
#' Helper function to confirm that the argument cutoff to several functions is
#' as expected
#'
#' @param cutoff Numeric; only those clusters with selection proportions equal
#' to at least cutoff will be selected by cluster stability selection. Must be
#' between 0 and 1.
#' @author Gregory Faletto, Jacob Bien
checkCutoff <- function(cutoff){
    stopifnot(is.numeric(cutoff) | is.integer(cutoff))
    stopifnot(length(cutoff) == 1)
    stopifnot(!is.na(cutoff))
    stopifnot(cutoff >= 0)
    stopifnot(cutoff <= 1)
}
```

Tests for `checkCutoff()`:


```r
testthat::test_that("checkCutoff works", {
  testthat::expect_null(checkCutoff(0))
  testthat::expect_null(checkCutoff(0.2))
  testthat::expect_null(checkCutoff(1))
  
  testthat::expect_error(checkCutoff(-.2), "cutoff >= 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkCutoff(2), "cutoff <= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkCutoff(".3"),
                        "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)
  testthat::expect_error(checkCutoff(matrix(1:12, nrow=4, ncol=3)),
                         "length(cutoff) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkCutoff(numeric()),
                         "length(cutoff) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkCutoff(as.numeric(NA)),
                         "!is.na(cutoff) is not TRUE", fixed=TRUE)

})
```

```
## Test passed 😀
```

`checkWeighting()`:


```r
#' Helper function to confirm that the argument weighting to several 
#' functions is as expected
#'
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'.
#' @author Gregory Faletto, Jacob Bien
checkWeighting <- function(weighting){
    stopifnot(length(weighting)==1)
    stopifnot(!is.na(weighting))
    if(!is.character(weighting)){
        stop("Weighting must be a character")
    }
    if(!(weighting %in% c("sparse", "simple_avg", "weighted_avg"))){
        stop("Weighting must be a character and one of sparse, simple_avg, or weighted_avg")
    }
}
```

Tests for `checkWeighting()`:


```r
testthat::test_that("checkWeighting works", {
  testthat::expect_null(checkWeighting("sparse"))
  testthat::expect_null(checkWeighting("simple_avg"))
  testthat::expect_null(checkWeighting("weighted_avg"))
  
  testthat::expect_error(checkWeighting(c("sparse", "simple_avg")),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkWeighting(NA), "!is.na(weighting) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkWeighting(1), "Weighting must be a character",
                         fixed=TRUE)
  testthat::expect_error(checkWeighting("spasre"),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)
})
```

```
## Test passed 🌈
```

`checkMinNumClusts()`:


```r
#' Helper function to confirm that the argument min_num_clusts to several 
#' functions is as expected
#'
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param p The number of features; since this is an upper bound on the number
#' of clusters of features, it is also an upper bound on min_num_clusts.
#' @param n_clusters The number of clusters; note that this is an upper bound
#' on min_num_clusts
#' @author Gregory Faletto, Jacob Bien
checkMinNumClusts <- function(min_num_clusts, p, n_clusters){
    stopifnot(length(min_num_clusts) == 1)
    stopifnot(is.numeric(min_num_clusts) | is.integer(min_num_clusts))
    stopifnot(!is.na(min_num_clusts))
    stopifnot(min_num_clusts == round(min_num_clusts))
    stopifnot(min_num_clusts >= 1)
    stopifnot(min_num_clusts <= p)
    stopifnot(min_num_clusts <= n_clusters)
}
```

Tests for `checkMinNumClusts()`:


```r
testthat::test_that("checkMinNumClusts works", {
  testthat::expect_null(checkMinNumClusts(1, 5, 4))
  testthat::expect_null(checkMinNumClusts(6, 6, 6))
  testthat::expect_null(checkMinNumClusts(3, 1932, 3))
  
  testthat::expect_error(checkMinNumClusts(c(2, 4), 5, 4),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts("3", "1932", "3"),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(NA, NA, NA),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(as.numeric(NA), as.numeric(NA),
                                           as.numeric(NA)),
                         "!is.na(min_num_clusts) is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(0, 13, 7),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(-1, 9, 8),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(6, 5, 5),
                         "min_num_clusts <= p is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(6, 7, 5),
                         "min_num_clusts <= n_clusters is not TRUE", fixed=TRUE)
})
```

```
## Test passed 🥇
```

`checkMaxNumClusts()`:


```r
#' Helper function to confirm that the argument max_num_clusts to several 
#' functions is as expected
#'
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Can be NA, in which case
#' max_num_clusts will be ignored.
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) max_num_clusts must be at least as
#' large as min_num_clusts.
#' @param p The number of features; since this is an upper bound on the number
#' of clusters of features, it is also an upper bound on max_num_clusts.
#' @param n_clusters The number of clusters; note that this is an upper bound
#' on max_num_clusts
#' @return The provided max_num_clusts, coerced to an integer if needed, and
#' coerced to be less than or equal to the total number of clusters.
#' @author Gregory Faletto, Jacob Bien
checkMaxNumClusts <- function(max_num_clusts, min_num_clusts, p, n_clusters){
    stopifnot(length(max_num_clusts) == 1)
    if(!is.na(max_num_clusts)){
        stopifnot(is.numeric(max_num_clusts) | is.integer(max_num_clusts))
        stopifnot(max_num_clusts == round(max_num_clusts))
        stopifnot(max_num_clusts >= 1)
        stopifnot(max_num_clusts <= p)
        max_num_clusts <- as.integer(min(n_clusters, max_num_clusts))
        stopifnot(max_num_clusts >= min_num_clusts)
    }
    return(max_num_clusts)
}
```

Tests for `checkMaxNumClusts()`:


```r
testthat::test_that("checkMaxNumClusts works", {
  testthat::expect_equal(checkMaxNumClusts(max_num_clusts=4, min_num_clusts=1,
                                           p=5, n_clusters=4), 4)
  testthat::expect_equal(checkMaxNumClusts(max_num_clusts=5, min_num_clusts=1,
                                           p=5, n_clusters=4), 4)
  testthat::expect_true(is.na(checkMaxNumClusts(max_num_clusts=NA,
                                                min_num_clusts=3, p=5,
                                                n_clusters=4)))
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts="4", min_num_clusts=1,
                                           p=5, n_clusters=4),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=3.2, min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=1, min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=c(3, 4),
                                           min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "length(max_num_clusts) == 1 is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts="4",
                                           min_num_clusts="2",
                                           p="5", n_clusters="4"),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=-1, min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "max_num_clusts >= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=6, min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkMaxNumClusts(max_num_clusts=1, min_num_clusts=2,
                                           p=5, n_clusters=4),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 😀
```

`getSelectedClusters()`:


```r
#' From css output, obtain names of selected clusters and selection proportions,
#' indices of all selected features, and weights of individual cluster members
#'
#' If cutoff is too high for at least min_num_clusts clusters to be selected,
#' then it will be lowered until min_num_clusts can be selected. After that, if
#' the cutoff is too low such that more than max_num_clusts are selected, then
#' the cutoff will be increased until no more than max_num_clusts are selected.
#' Note that because clusters can have tied selection proportions, it is
#' possible that the number of selected clusters will be strictly lower than
#' max_num_clusts or strictly greater than min_num_clusts. In fact, it is
#' possible that both cutoffs won't be able to be satisfied simulteaneously,
#' even if there is a strictly positive difference between max_num_clusts and
#' min_num_clusts. If this occurs, max_num_clusts will take precedence over
#' min_num_clusts. getSelectedClusters will throw an error if the provided
#' inputs don't allow it to select any clusters. 
#' 
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details.
#' @param cutoff Numeric; getCssSelections will select and return only of those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1.
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) If NA, max_num_clusts is ignored.
#' @return A named list with the following elements: \item{selected_clusts}{A
#' named numeric vector containing the selection proportions for the selected
#' clusters. The name of each entry is the name of the corresponding cluster.}
#' \item{selected_feats}{A named integer vector; the indices of the features
#' with nonzero weights from all of the selected clusters.} \item{weights}{A
#' named list of the same length as the number of selected clusters. Each list
#' element weights[[j]] is a numeric vector of the weights to use for the jth
#' selected cluster, and it has the same name as the cluster it corresponds
#' to.}
#' @author Gregory Faletto, Jacob Bien
getSelectedClusters <- function(css_results, weighting, cutoff, min_num_clusts,
    max_num_clusts){
    # Check input
    stopifnot(class(css_results) == "cssr")

    # Eliminate clusters with selection proportions below cutoff
    clus_sel_props <- colMeans(css_results$clus_sel_mat)

    # Get selected clusters
    selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
    B <- nrow(css_results$feat_sel_mat)

    # Check that selected_clusts has length at least min_num_clusts
    while(length(selected_clusts) < min_num_clusts){
        cutoff <- cutoff - 1/B
        selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
    }

    # Check that selected_clusts has length at most max_num_clusts
    if(!is.na(max_num_clusts)){
        n_clusters <- ncol(css_results$clus_sel_mat)
        while(length(selected_clusts) > max_num_clusts){
            cutoff <- cutoff + 1/B
            if(cutoff > 1){
                break
            }
            # Make sure we don't reduce to a selected set of size 0
            if(any(clus_sel_props >= cutoff)){
                selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
            } else{
                break
            }
        }
    }

    stopifnot(length(selected_clusts) >= 1)

    clust_names <- names(selected_clusts)

    n_sel_clusts <- length(selected_clusts)

    # Check that n_sel_clusts is as expected, and throw warnings or an error if
    # not
    checkSelectedClusters(n_sel_clusts, min_num_clusts, max_num_clusts,
        max(clus_sel_props))
    
    ### Get selected features from selected clusters
    clusters <- css_results$clusters
    stopifnot(all(clust_names %in% names(clusters)))

    # Get a list of weights for all of the selected clusters
    weights <- getAllClustWeights(css_results, selected_clusts, weighting)

    # Get selected features from each cluster (those features with nonzero
    # weights)
    selected_feats <- integer()
    for(i in 1:n_sel_clusts){
        clus_i_name <- clust_names[i]
        clust_i <- clusters[[clus_i_name]]
        weights_i <- weights[[i]]
        selected_feats <- c(selected_feats, clust_i[weights_i != 0])
    }

    feat_names <- colnames(css_results$feat_sel_mat)

    names(selected_feats) <- feat_names[selected_feats]

    # Check output (already checked weights wihin getAllClustWeights)

    checkGetSelectedClustersOutput(selected_clusts, selected_feats,
        weights, n_clusters=length(clusters), p=ncol(css_results$feat_sel_mat))

    return(list(selected_clusts=selected_clusts,
        selected_feats=selected_feats, weights=weights))
}
```

`checkSelectedClusters()`:


```r
#' Helper function to check operations within getSelectedClusters function
#'
#' @param n_sel_clusts The number of selected clusters; should be constrained
#' by min_num_clusts and max_num_clusts (though it may not be possible to
#' satisfy both constraints simulteneously, in which case a warning will be
#' thrown).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) If NA, max_num_clusts is ignored.
#' @param max_sel_prop Numeric; the maximum selection proportion observed for 
#' any cluster.
#' @author Gregory Faletto, Jacob Bien
checkSelectedClusters <- function(n_sel_clusts, min_num_clusts, max_num_clusts,
    max_sel_prop){
    if(n_sel_clusts == 0){
        err <- paste("No clusters selected with this cutoff (try a cutoff below the maximum cluster selection proportion, ",
            max_sel_prop, ")", sep="")
        stop(err)
    }

    stopifnot(n_sel_clusts >= 1)

    # It may be impossible to get at least min_num_clusts or at most
    # max_num_clusts; if so, give a warning
    if(n_sel_clusts < min_num_clusts){
        warn <- paste("Returning fewer than min_num_clusts = ", min_num_clusts,
            " clusters because decreasing the cutoff any further would require returning more than max_num_clusts = ",
            max_num_clusts, " clusters", sep="")
        warning(warn)
    }
    if(!is.na(max_num_clusts)){
        if(n_sel_clusts > max_num_clusts){
            warn <- paste("Returning more than max_num_clusts = ",
                max_num_clusts,
                " clusters because increasing the cutoff any further would require returning 0 clusters",
                sep="")
            warning(warn)
        }
    }
}
```

Test for `checkSelectedClusters()`:


```r
testthat::test_that("checkSelectedClusters works", {
  testthat::expect_null(checkSelectedClusters(n_sel_clusts=5, min_num_clusts=1,
                                              max_num_clusts=NA, max_sel_prop=.8))
  testthat::expect_null(checkSelectedClusters(n_sel_clusts=5, min_num_clusts=2,
                                              max_num_clusts=5, max_sel_prop=.3))
  testthat::expect_null(checkSelectedClusters(n_sel_clusts=2, min_num_clusts=2,
                                              max_num_clusts=5, max_sel_prop=.3))
  

  testthat::expect_error(checkSelectedClusters(n_sel_clusts=0, min_num_clusts=2,
                                               max_num_clusts=5,
                                               max_sel_prop=.6),
                         "No clusters selected with this cutoff (try a cutoff below the maximum cluster selection proportion, 0.6)",
                         fixed=TRUE)
  
  testthat::expect_warning(checkSelectedClusters(n_sel_clusts=1,
                                                 min_num_clusts=2,
                                                 max_num_clusts=5,
                                                 max_sel_prop=.6),
                         "Returning fewer than min_num_clusts = 2 clusters because decreasing the cutoff any further would require returning more than max_num_clusts = 5 clusters",
                         fixed=TRUE)
  testthat::expect_warning(checkSelectedClusters(n_sel_clusts=6,
                                                 min_num_clusts=2,
                                                 max_num_clusts=5,
                                                 max_sel_prop=.6),
                         "Returning more than max_num_clusts = 5 clusters because increasing the cutoff any further would require returning 0 clusters",
                         fixed=TRUE)
  
})
```

```
## Test passed 🥇
```

`getAllClustWeights()`:


```r
#' Calculate weights for each cluster member of all of the selected clusters.
#' 
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param sel_clusters A named numeric vector containing the selection
#' proportions for the selected clusters. The name of each entry is the name
#' of the corresponding cluster.
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details.
#' @return A named list of the same length as sel_clusters of numeric vectors.
#' weights[[j]] is the weights to use for the jth selected cluster, and it has
#' the same name as the cluster it corresponds to.
#' @author Gregory Faletto, Jacob Bien
getAllClustWeights <- function(css_results, sel_clusters, weighting){

    # Check inputs
    stopifnot(class(css_results) == "cssr")

    stopifnot(is.numeric(sel_clusters))
    p_ret <- length(sel_clusters)
    stopifnot(length(unique(names(sel_clusters))) == p_ret)
    stopifnot(p_ret > 0)

    checkWeighting(weighting)

    # Get selection proportions and clusters
    feat_sel_props <- colMeans(css_results$feat_sel_mat)

    p <- length(feat_sel_props)
    stopifnot(p >= p_ret)

    clusters <- css_results$clusters
    stopifnot(all(names(sel_clusters) %in% names(clusters)))

    # Identify weights
    weights <- list()

    for(j in 1:p_ret){
        # Find the members of the cluster feature j is a member of
        cluster_j <- clusters[[names(sel_clusters)[j]]]
        # Get the weights for this cluster and add them to the list
        weights[[j]] <- getClustWeights(cluster_j, weighting, feat_sel_props)
    }

    # Add names to weights
    names(weights) <- names(sel_clusters)

    # Check output

    stopifnot(length(weights) == p_ret)
    stopifnot(is.list(weights))

    for(i in 1:p_ret){
        stopifnot(length(clusters[[names(sel_clusters)[i]]]) ==
            length(weights[[i]]))
        stopifnot(all(weights[[i]] >= 0))
        stopifnot(all(weights[[i]] <= 1))
        stopifnot(abs(sum(weights[[i]]) - 1) < 10^(-6))
    }
    return(weights)
}
```

`getClustWeights()`:


```r
#' Calculate weights for members of a cluster using selection proportions
#'
#' Given a cluster of features, the selection proportions for each cluster
#' member, and a specified weighting scheme, calculate the appropriate weights
#' for the cluster.
#' @param cluster_i An integer vector containing the indices of the members
#' of a cluster.
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details.
#' @param feat_sel_props A numeric vector of selection proportions corresponding
#' to each of the p features.
#' @return A numeric vector of the same length as cluster_i containing the
#' weights corresponding to each of the features in cluster_i. The weights
#' will all be nonnegative and sum to 1.
#' @author Gregory Faletto, Jacob Bien
getClustWeights <- function(cluster_i, weighting, feat_sel_props){

    stopifnot(is.integer(cluster_i) | is.numeric(cluster_i))
    stopifnot(all(cluster_i == round(cluster_i)))
    n_weights <- length(cluster_i)
    stopifnot(length(unique(cluster_i)) == n_weights)

    p <- length(feat_sel_props)
    stopifnot(all(cluster_i %in% 1:p))

    # Get the selection proportions of each cluster member
    sel_props <- feat_sel_props[cluster_i]

    stopifnot(all(sel_props >= 0))
    stopifnot(all(sel_props <= 1))

    weights_i <- rep(as.numeric(NA), n_weights)

    # Weighted or simple average?
    if(weighting == "sparse"){
        # Sparse cluster stability selection: All features in cluster with
        # selection proportion equal to the max
        # for the cluster get equal weight; rest of cluster gets 0 weight
        if(sum(sel_props) == 0){
            weights_i <- rep(1/n_weights, n_weights)
        } else{
            maxes <- sel_props==max(sel_props)

            stopifnot(sum(maxes) > 0)
            stopifnot(sum(maxes) <= n_weights)

            weights_i <- rep(0, n_weights)
            weights_i[maxes] <- 1/sum(maxes)
        }
    } else if(weighting == "weighted_avg"){
        # Get weights for weighted average
        if(sum(sel_props) == 0){
            weights_i <- rep(1/n_weights, n_weights)
        } else{
            weights_i <- sel_props/sum(sel_props)
        }
    } else if(weighting == "simple_avg"){
        weights_i <- rep(1/n_weights, n_weights)
    } else{
        stop("weighting must be one of sparse, simple_avg, or weighted_avg")
    }

    stopifnot(abs(sum(weights_i) - 1) < 10^(-6))
    stopifnot(length(weights_i) == n_weights)
    stopifnot(length(weights_i) >= 1)
    stopifnot(all(weights_i >= 0))
    stopifnot(all(weights_i <= 1))

    return(weights_i)
}
```


Tests for `getClustWeights()`:


```r
testthat::test_that("getClustWeights works", {
  sel_props <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  
  # sparse
  testthat::expect_identical(getClustWeights(cluster_i=c(3L, 4L, 5L),
                                             weighting="sparse",
                                             feat_sel_props=sel_props),
                             c(0, 0, 1))
  
  # weighted_avg
  cluster=c(1L, 3L, 5L)
  true_weights <- sel_props[cluster]/sum(sel_props[cluster])
  
  testthat::expect_identical(getClustWeights(cluster_i=cluster,
                                             weighting="weighted_avg",
                                             feat_sel_props=sel_props),
                             true_weights)
  
  # simple_avg
  testthat::expect_identical(getClustWeights(cluster_i=c(2L, 3L, 4L, 5L),
                                             weighting="simple_avg",
                                             feat_sel_props=sel_props),
                             rep(0.25, 4))
})
```

```
## Test passed 🥳
```


Tests for `getAllClustWeights()`:


```r
testthat::test_that("getAllClustWeights works", {
  
  set.seed(1872)
  
  x <- matrix(stats::rnorm(10*5), nrow=10, ncol=5)
  y <- stats::rnorm(10)
  
  clust_names <- letters[1:3]
  
  good_clusters <- list(1:2, 3:4, 5)
  
  names(good_clusters) <- clust_names
  
  res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, fitfun = cssLasso,
    sampling_type = "SS", B = 10, prop_feats_remove = 0, train_inds = integer(),
    num_cores = 1L)
  
  sel_props <- colMeans(res$feat_sel_mat)
  
  sel_clusts <- list(1L:2L, 3L:4L)
  
  names(sel_clusts) <- clust_names[1:2]
  
  # sparse
  true_weights <- list()
  
  for(i in 1:2){
    weights_i <- sel_props[sel_clusts[[i]]]/sum(sel_props[sel_clusts[[i]]])
    true_weights[[i]] <- rep(0, length(weights_i))
    true_weights[[i]][weights_i == max(weights_i)] <- 1
  }
  
  names(true_weights) <- clust_names[1:2]
  
  testthat::expect_identical(getAllClustWeights(res,
                                                colMeans(res$clus_sel_mat[, 1:2]),
                                                "sparse"), true_weights)

  # weighted_avg
  true_weights <- list()

  for(i in 1:2){
    true_weights[[i]] <- sel_props[sel_clusts[[i]]]/sum(sel_props[sel_clusts[[i]]])
  }
  
  names(true_weights) <- clust_names[1:2]

  testthat::expect_identical(getAllClustWeights(res,
                                                colMeans(res$clus_sel_mat[, 1:2]),
                                                "weighted_avg"), true_weights)

  # simple_avg
  true_weights <- list()

  for(i in 1:2){
    n_weights_i <- length(sel_clusts[[i]])
    true_weights[[i]] <- rep(1/n_weights_i, n_weights_i)
  }
  
  names(true_weights) <- clust_names[1:2]

  testthat::expect_identical(getAllClustWeights(res,
                                                colMeans(res$clus_sel_mat[, 1:2]),
                                                "simple_avg"), true_weights)

  # Errors

  # css_results not correct (error has quotation marks)
  testthat::expect_error(getAllClustWeights(1:4, colMeans(res$clus_sel_mat[,
                                                                           1:2]),
                                            "simple_avg"))

  bad_sel_clusts <- colMeans(res$clus_sel_mat[, 1:2])
  names(bad_sel_clusts) <- c("apple", "banana")
  testthat::expect_error(getAllClustWeights(res, bad_sel_clusts, "sparse"),
                         "all(names(sel_clusters) %in% names(clusters)) is not TRUE",
                         fixed=TRUE)


  testthat::expect_error(getAllClustWeights(res, colMeans(res$clus_sel_mat[,
                                                                           1:2]),
                                            c("sparse", "simple_avg")),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getAllClustWeights(res, colMeans(res$clus_sel_mat[,
                                                                           1:2]),
                                            NA),
                         "!is.na(weighting) is not TRUE", fixed=TRUE)

  testthat::expect_error(getAllClustWeights(res, colMeans(res$clus_sel_mat[,
                                                                           1:2]),
                                            1),
                         "Weighting must be a character", fixed=TRUE)

  testthat::expect_error(getAllClustWeights(res, colMeans(res$clus_sel_mat[,
                                                                           1:2]),
                                            "spasre"),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)

})
```

```
## Test passed 🥇
```

`checkGetSelectedClustersOutput()`:


```r
#' Helper function to check that output of getSelectedClusters is as expected
#'
#' @param selected_clusts A named numeric vector containing the selection
#' proportions for the selected clusters. The name of each entry is the name of
#' the corresponding cluster.
#' @param selected_feats A named integer vector; the indices of the features
#' with nonzero weights from all of the selected clusters.
#' @param weights A named list of the same length as the number of selected
#' clusters. Each list element weights[[j]] is a numeric vector of the weights
#' to use for the jth selected cluster, and it has the same name as the cluster
#' it corresponds to.
#' @param n_clusters Integer; the number of clusters in the data (upper bound
#' for the length of selected_clusts)
#' @param p Integer; number of features in the data (all selected_feats should
#' be in 1:p)
#' @author Gregory Faletto, Jacob Bien
checkGetSelectedClustersOutput <- function(selected_clusts, selected_feats,
    weights, n_clusters, p){
    stopifnot(is.numeric(selected_clusts))
    stopifnot(all(selected_clusts >= 0))
    stopifnot(all(selected_clusts <= 1))
    stopifnot(length(selected_clusts) >= 1)
    stopifnot(length(selected_clusts) <= n_clusters)
    stopifnot(length(names(selected_clusts)) ==
        length(unique(names(selected_clusts))))
    stopifnot(!is.null(names(selected_clusts)))
    stopifnot(all(!is.na(names(selected_clusts)) &
        names(selected_clusts) != ""))
    stopifnot(length(names(selected_clusts)) == length(selected_clusts))
    stopifnot(is.integer(selected_feats))
    stopifnot(length(selected_feats) == length(unique(selected_feats)))
    stopifnot(all(selected_feats %in% 1:p))
    stopifnot(length(selected_clusts) <= length(selected_feats))
    stopifnot(identical(names(weights), names(selected_clusts)))
    stopifnot(length(weights) == length(selected_clusts)) 
}
```

Tests for `checkGetSelectedClustersOutput()`:


```r
testthat::test_that("checkGetSelectedClustersOutput works", {
  
  sel_clusts <- 0.1*(1:9)
  names(sel_clusts) <- letters[1:9]
  
  weights <- list()
  
  for(i in 1:8){
    weights[[i]] <- c(0.2, 0.3)
  }
  weights[[9]] <- 0.4
  names(weights) <- letters[1:9]
  
  sel_feats <- 10:26
  names(sel_feats) <- LETTERS[10:26]
  
  testthat::expect_null(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30))
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=letters[1:4],
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "is.numeric(selected_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=-sel_clusts,
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "all(selected_clusts >= 0) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=10*sel_clusts,
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "all(selected_clusts <= 1) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=numeric(),
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "length(selected_clusts) >= 1 is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats,
                               weights=weights,
                               n_clusters=8, p=30),
                         "length(selected_clusts) <= n_clusters is not TRUE",
                         fixed=TRUE)
  
  bad_clusts <- sel_clusts
  names(bad_clusts) <- rep("a", length(bad_clusts))
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats,
                               weights=weights,
                               n_clusters=10, p=30),
                         "length(names(selected_clusts)) == length(unique(names(selected_clusts))) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=unname(sel_clusts),
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "!is.null(names(selected_clusts)) is not TRUE",
                         fixed=TRUE)
  
  bad_clusts <- sel_clusts
  names(bad_clusts)[1] <- ""
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats, weights=weights,
                               n_clusters=10, p=30),
                         "all(!is.na(names(selected_clusts)) & names(selected_clusts) !=  .... is not TRUE",
                         fixed=TRUE)
  
  names(bad_clusts)[1] <- as.character(NA)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats, weights=weights,
                               n_clusters=10, p=30),
                         "all(!is.na(names(selected_clusts)) & names(selected_clusts) !=  .... is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=0.1,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "is.integer(selected_feats) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=c(1L,
                                                                        rep(2L,
                                                                            2)),
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "length(selected_feats) == length(unique(selected_feats)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats, weights=weights,
                               n_clusters=10, p=25),
                         "all(selected_feats %in% 1:p) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats[1:8], weights=weights,
                               n_clusters=10, p=25),
                         "length(selected_clusts) <= length(selected_feats) is not TRUE",
                         fixed=TRUE)
  
})
```

```
## Test passed 🥇
```

Tests for `getSelectedClusters()`


```r
testthat::test_that("getSelectedClusters works", {
  set.seed(26717)
  
  x <- matrix(stats::rnorm(10*5), nrow=10, ncol=5)
  y <- stats::rnorm(10)
  
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  res <- getSelectedClusters(css_res, weighting="sparse", cutoff=0.05,
                             min_num_clusts=1, max_num_clusts=NA)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))

  testthat::expect_true(is.numeric(res$selected_clusts))
  testthat::expect_true(length(res$selected_clusts) >= 1)
  testthat::expect_equal(length(names(res$selected_clusts)),
                         length(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                         length(unique(names(res$selected_clusts))))
  testthat::expect_true(all(res$selected_clusts >= 0))
  testthat::expect_true(all(res$selected_clusts <= 1))

  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 5))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))

  testthat::expect_equal(length(res$selected_clusts), length(res$weights))
  for(i in 1:length(res$weights)){
    weights_i <- res$weights[[i]]
    num_nonzero_weights <- sum(weights_i > 0)
    # For "sparse" weighting, tither there should only be one nonzero weight and
    # it should equal 1 (if there were no ties in selection proportions among
    # cluster members) or the nonzero weights should all be
    # 1/num_nonzero_weights
    testthat::expect_true(all(weights_i[weights_i > 0] == 1/num_nonzero_weights))
  }

  # weighted_avg
  res_weighted <- getSelectedClusters(css_res, weighting="weighted_avg",
                                      cutoff=0.05, min_num_clusts=1,
                                      max_num_clusts=NA)

  testthat::expect_equal(length(res_weighted$selected_clusts),
                         length(res_weighted$weights))
  for(i in 1:length(res_weighted$weights)){
    weights_i <- res_weighted$weights[[i]]
    testthat::expect_true(all(weights_i >= 0))
    testthat::expect_true(all(weights_i <= 1))
  }

  # simple_avg
  res_simple <- getSelectedClusters(css_res, weighting="simple_avg",
                                    cutoff=0.05, min_num_clusts=1,
                                    max_num_clusts=NA)

  testthat::expect_equal(length(res_simple$selected_clusts),
                         length(res_simple$weights))
  for(i in 1:length(res_simple$weights)){
    weights_i <- res_simple$weights[[i]]
    testthat::expect_equal(length(unique(weights_i)), 1)
    testthat::expect_equal(length(weights_i), sum(weights_i > 0))
  }

  # Test min_num_clusts
  res2 <- getSelectedClusters(css_res, weighting="weighted_avg", cutoff=1,
                             min_num_clusts=3, max_num_clusts=NA)
  testthat::expect_true(is.list(res2))
  testthat::expect_equal(length(res2$selected_clusts), 3)

  res3 <- getSelectedClusters(css_res, weighting="sparse", cutoff=1,
                             min_num_clusts=2, max_num_clusts=NA)
  testthat::expect_true(length(res3$selected_clusts) >= 2)

  # Test max_num_clusts
  # Ensure there is at least one relevant feature
  x2 <- x
  x2[, 5] <- y
  css_res2 <- css(X=x2, y=y, lambda=0.01, clusters=good_clusters, B = 10)
  res4 <- getSelectedClusters(css_res2, weighting="simple_avg", cutoff=0,
                             min_num_clusts=1, max_num_clusts=1)
  testthat::expect_true(is.list(res4))
  testthat::expect_equal(length(res4$selected_clusts), 1)

  res5 <- getSelectedClusters(css_res, weighting="weighted_avg", cutoff=0,
                             min_num_clusts=1, max_num_clusts=2)
  testthat::expect_true(length(res5$selected_clusts) <= 2)
  
  # Name features
  colnames(x) <- LETTERS[1:ncol(x)]
  css_res3 <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)
  res <- getSelectedClusters(css_res3, weighting="sparse", cutoff=0.05,
                             min_num_clusts=1, max_num_clusts=NA)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))

  testthat::expect_true(is.numeric(res$selected_clusts))
  testthat::expect_true(length(res$selected_clusts) >= 1)
  testthat::expect_equal(length(names(res$selected_clusts)),
                         length(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                         length(unique(names(res$selected_clusts))))
  testthat::expect_true(all(res$selected_clusts >= 0))
  testthat::expect_true(all(res$selected_clusts <= 1))

  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 5))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(res$selected_feats))
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
})
```

```
## Test passed 🎊
```

Finally, tests for `getCssSelections()`


```r
testthat::test_that("getCssSelections works", {

  set.seed(26717)
  
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  res <- getCssSelections(css_res)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:11))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= 11)
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:11))

  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 7))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))

  # Test min_num_clusts (should be 5 clusters--3 named ones, plus last two get
  # put in their own unnamed clusters automatically by css)
  res2 <- getCssSelections(css_res, weighting="weighted_avg", cutoff=1,
                             min_num_clusts=5, max_num_clusts=NA)
  testthat::expect_true(is.list(res2))
  testthat::expect_equal(length(res2$selected_clusts), 5)

  res3 <- getCssSelections(css_res, weighting="sparse", cutoff=1,
                             min_num_clusts=3, max_num_clusts=NA)
  testthat::expect_true(length(res3$selected_clusts) >= 3)

  # Test max_num_clusts
  # Ensure there is at least one relevant feature
  x2 <- x
  x2[, 5] <- y
  css_res2 <- css(X=x2, y=y, lambda=0.01, clusters=good_clusters, B = 10)
  res4 <- getCssSelections(css_res2, weighting="simple_avg", cutoff=0,
                             min_num_clusts=1, max_num_clusts=1)
  testthat::expect_true(is.list(res4))
  testthat::expect_equal(length(res4$selected_clusts), 1)

  res5 <- getCssSelections(css_res, weighting="weighted_avg", cutoff=0,
                             min_num_clusts=1, max_num_clusts=2)
  testthat::expect_true(length(res5$selected_clusts) <= 2)

  # Name features
  colnames(x) <- LETTERS[1:ncol(x)]
  css_res3 <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)
  res <- getCssSelections(css_res3, weighting="sparse", cutoff=0.05,
                             min_num_clusts=1, max_num_clusts=NA)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))

  testthat::expect_equal(length(names(res$selected_feats)),
                         length(res$selected_feats))
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))

  # Bad inputs
  # Error has quotation marks in it
  testthat::expect_error(getCssSelections("css_results"))
  testthat::expect_error(getCssSelections(css_res, weighting="spasre"),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, cutoff=-.5),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, min_num_clusts=0),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, min_num_clusts=0),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, max_num_clusts=50),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, max_num_clusts=4.5),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 🥳
```

`getCssDesign()`


```r
#' Obtain a design matrix of cluster representatives
#'
#' Takes a matrix of observations from the original feature space and returns
#' a matrix of representatives from the selected clusters based on the results
#' of cluster stability selection.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param newX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newX are labeled, the names must match the variable names provided to css.
#' newX may be omitted if train_inds were provided to css to set aside
#' observations for model estimation. If this is the case, then when newX is
#' omitted getCssDesign will return a design matrix of cluster representatives
#' formed from the train_inds observations from the matrix X provided to css.
#' (If no train_inds were provided to css, newX must be provided to
#' getCssDesign.) Default is NA.
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'. For "sparse", all the weight is put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", the weight used for each cluster member is calculated in
#' proportion to the individual selection proportions of each feature. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, the cluster representative
#' is just a simple average of all the cluster members). See Faletto and Bien
#' (2022) for details. Default is "weighted_avg".
#' @param cutoff Numeric; getCssDesign will only include those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is 0 (in which case either all clusters are used, or max_num_clusts
#' are used, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @return A design matrix with either nrow(newX) (or length(train_inds), if
#' train_inds was provided to css and newX was not provided to getCssDesign)
#' observations and number of columns equal to the number of selected clusters,
#' containing the cluster representatives for each cluster.
#' @author Gregory Faletto, Jacob Bien
#' @export
getCssDesign <- function(css_results, newX=NA, weighting="weighted_avg",
    cutoff=0, min_num_clusts=1, max_num_clusts=NA){
    # Check inputs
    stopifnot(class(css_results) == "cssr")

    check_results <- checkNewXProvided(newX, css_results)

    newX <- check_results$newX
    newXProvided <- check_results$newXProvided

    rm(check_results)

    n_train <- nrow(newX)

    results <- checkXInputResults(newX, css_results$X)

    newX <- results$newx
    feat_names <- results$feat_names

    rm(results)

    n <- nrow(newX)
    p <- ncol(newX)

    checkCutoff(cutoff)
    checkWeighting(weighting)
    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    # Take provided training design matrix and testX and turn them into
    # matrices of cluster representatives using information from css_results
    if(newXProvided){
        newX_clusters <- formCssDesign(css_results, weighting, cutoff,
            min_num_clusts, max_num_clusts, newx=newX)
    } else{
        newX_clusters <- formCssDesign(css_results, weighting, cutoff,
            min_num_clusts, max_num_clusts)
    }

    return(newX_clusters)
}
```

`checkNewXProvided()`


```r
#' Helper function to confirm that the new X matrix provided to getCssDesign or
#' getCssPreds matches the characteristics of the X that was provided to css.
#'
#' @param trainX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix). Must contain
#' the same features (in the same number of columns) as the X matrix provided to
#' css, and if the columns of trainX are labeled, the names must match the
#' variable names provided to css. trainX may be omitted if train_inds were
#' provided to css to set aside observations.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @return A named list with the following elements: \item{newX}{If trainX was
#' provided, this is the provided trainX matrix, coerced from a data.frame to a
#' matrix if the provided trainX was a data.frame. If trainX was not provided,
#' this is a matrix made up of the training indices provided to css in the
#' train_inds argument.} \item{newXProvided}{Logical; indicates whether a valid
#' trainX input was provided.}
#' @author Gregory Faletto, Jacob Bien
checkNewXProvided <- function(trainX, css_results){
    newXProvided <- FALSE

    if(all(!is.na(trainX)) & length(trainX) > 1){
        newXProvided <- TRUE
        trainX <- checkXInputResults(trainX, css_results$X)$newx
        
        n_train <- nrow(trainX)
        stopifnot(n_train > 1)
    } else{
        if(length(css_results$train_inds) == 0){
            stop("css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix")
        }
        trainX <- css_results$X[css_results$train_inds, ]
    } 
    stopifnot(is.matrix(trainX))
    stopifnot(is.numeric(trainX) | is.integer(trainX))
    stopifnot(all(!is.na(trainX)))
    stopifnot(ncol(trainX) >= 2)

    return(list(newX=trainX, newXProvided=newXProvided))
}
```


`checkXInputResults()`


```r
#' Helper function to confirm that inputs to several functions are as expected,
#' and modify inputs if needed
#'
#' @param newx A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newX are labeled, the names must match the variable names provided to css.
#' @param css_X The X matrix provided to css, as in the output of the css
#' function (after having been coerced from a data.frame to a matrix by css if
#' needed).
#' @return A named list with the following elements. \item{feat_names}{A 
#' character vector containing the column names of newx (if the provided newx
#' had column names). If the provided newx did not have column names, feat_names
#' will be NA.} \item{newx}{The provided newx matrix, coerced from a data.frame
#' to a matrix if the provided newx was a data.frame.}
#' @author Gregory Faletto, Jacob Bien
checkXInputResults <- function(newx, css_X){

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(newx)){
        newx <- stats::model.matrix(~ ., newx)
        newx <- newx[, colnames(newx) != "(Intercept)"]
    }

    feat_names <- as.character(NA)
    if(!is.null(colnames(newx))){
        feat_names <- colnames(newx)
        stopifnot(identical(feat_names, colnames(css_X)))
    } else{
        # In this case, newx has no column names, so same better be true of
        # css_X
        if(!is.null(colnames(css_X))){
            warning("New X provided had no variable names (column names) even though the X provided to css did.")
        }
    }

    stopifnot(is.matrix(newx))
    stopifnot(all(!is.na(newx)))

    n <- nrow(newx)
    p <- ncol(newx)
    stopifnot(p >= 2)
    if(length(feat_names) > 1){
        stopifnot(length(feat_names) == p)
        stopifnot(!("(Intercept)" %in% feat_names))
    } else{
        stopifnot(is.na(feat_names))
    }

    colnames(newx) <- character()

    # Confirm that newx matches css_results$X
    if(p != ncol(css_X)){
        err <- paste("Number of columns in newx must match number of columns from matrix provided to css. Number of columns in new provided X: ",
            p, ". Number of columns in matrix provided to css: ", ncol(css_X),
            ".", sep="")
        stop(err)
    }
    if(length(feat_names) != 1 & all(!is.na(feat_names))){
        if(!identical(feat_names, colnames(css_X))){
            stop("Provided feature names for newx do not match feature names provided to css")
        }
    }

    return(list(feat_names=feat_names, newx=newx))
}
```

Tests for `checkXInputResults()`


```r
testthat::test_that("checkXInputResults works", {
  set.seed(72617)

  x_select <- matrix(stats::rnorm(10*5), nrow=10, ncol=5)
  x_new <- matrix(stats::rnorm(8*5), nrow=8, ncol=5)
  y_select <- stats::rnorm(10)
  y_new <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)
  
  res <- checkXInputResults(x_new, css_res$X)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("feat_names", "newx"))
  
  testthat::expect_true(is.character(res$feat_names))
  testthat::expect_true(is.na(res$feat_names))
  
  testthat::expect_true(is.numeric(res$newx))
  testthat::expect_true(is.matrix(res$newx))
  testthat::expect_equal(nrow(res$newx), 8)
  testthat::expect_equal(ncol(res$newx), 5)
  testthat::expect_null(colnames(res$newx))
  
  # Try naming variables
  
  colnames(x_select) <- LETTERS[1:5]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)
  
  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(checkXInputResults(x_new, css_res_named$X),
                           "New X provided had no variable names (column names) even though the X provided to css did.",
                           fixed=TRUE)

  # Try mismatching variable names
  colnames(x_new) <- LETTERS[2:6]
  testthat::expect_error(checkXInputResults(x_new, css_res_named$X),
                           "identical(feat_names, colnames(css_X)) is not TRUE",
                           fixed=TRUE)
  
  colnames(x_new) <- LETTERS[1:5]
  
  res_named <- checkXInputResults(x_new, css_res_named$X)
  
  testthat::expect_true(is.list(res_named))
  testthat::expect_identical(names(res_named), c("feat_names", "newx"))
  
  testthat::expect_true(is.character(res_named$feat_names))
  testthat::expect_identical(res_named$feat_names, LETTERS[1:5])
  
  # Try data.frame input to css and checkXInputResults
  
  X_df <- datasets::mtcars
  
  n <- nrow(X_df)
  y <- stats::rnorm(n)
  
  selec_inds <- 1:round(n/2)
  fit_inds <- setdiff(1:n, selec_inds)
  
  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkXInputResults(X_df[fit_inds, ], css_res_df$X)
  
  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("feat_names", "newx"))
  
  testthat::expect_true(is.character(res_df$feat_names))
  testthat::expect_identical(res_df$feat_names, colnames(css_res_df$X))
  testthat::expect_identical(res_df$feat_names, colnames(X_df))

  testthat::expect_true(is.numeric(res_df$newx))
  testthat::expect_true(is.matrix(res_df$newx))
  testthat::expect_null(colnames(res_df$newx))
  testthat::expect_equal(ncol(res_df$newx), ncol(css_res_df$X))
  
  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)
  
  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkXInputResults(X_df[fit_inds, ], css_res_df$X)

  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("feat_names", "newx"))

  testthat::expect_true(is.character(res_df$feat_names))
  testthat::expect_identical(res_df$feat_names, colnames(css_res_df$X))
  
  mat <- model.matrix( ~., X_df)
  mat <- mat[, colnames(mat) != "(Intercept)"]
  
  testthat::expect_identical(res_df$feat_names, colnames(mat))

  testthat::expect_true(is.numeric(res_df$newx))
  testthat::expect_true(is.matrix(res_df$newx))
  testthat::expect_null(colnames(res_df$newx))
  testthat::expect_equal(ncol(res_df$newx), ncol(css_res_df$X))
})
```

```
## Test passed 🌈
```

Tests for `checkNewXProvided()`


```r
testthat::test_that("checkNewXProvided works", {
  set.seed(2673)

  x_select <- matrix(stats::rnorm(10*5), nrow=10, ncol=5)
  x_new <- matrix(stats::rnorm(8*5), nrow=8, ncol=5)
  y_select <- stats::rnorm(10)
  y_new <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)
  
  res <- checkNewXProvided(x_new, css_res)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newX", "newXProvided"))
  
  testthat::expect_true(is.numeric(res$newX))
  testthat::expect_true(is.matrix(res$newX))
  testthat::expect_equal(nrow(res$newX), 8)
  testthat::expect_equal(ncol(res$newX), 5)
  testthat::expect_null(colnames(res$newX))
  
  testthat::expect_true(is.logical(res$newXProvided))
  testthat::expect_equal(length(res$newXProvided), 1)
  testthat::expect_true(!is.na(res$newXProvided))
  testthat::expect_true(res$newXProvided)
  
  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10, train_inds=6:10)
  
  # Training indices should be ignored if new x is provided
  
  res <- checkNewXProvided(x_new, css_res_train)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newX", "newXProvided"))
  
  testthat::expect_true(all(abs(x_new - res$newX) < 10^(-9)))
  testthat::expect_true(res$newXProvided)
  
  # Things should still work if new x is not provided
  
  res <- checkNewXProvided(NA, css_res_train)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newX", "newXProvided"))
  
  testthat::expect_true(is.numeric(res$newX))
  testthat::expect_true(is.matrix(res$newX))
  testthat::expect_equal(nrow(res$newX), 5)
  testthat::expect_equal(ncol(res$newX), 5)
  testthat::expect_null(colnames(res$newX))
  
  testthat::expect_false(res$newXProvided)
  
  # Try not providing training indices and omitting newx--should get error
  testthat::expect_error(checkNewXProvided(NA, css_res),
                         "css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix", fixed=TRUE)
  
  # Try naming variables

  colnames(x_select) <- LETTERS[1:5]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(checkNewXProvided(x_new, css_res_named),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_new) <- LETTERS[2:6]
  testthat::expect_error(checkNewXProvided(x_new, css_res_named),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_new) <- LETTERS[1:5]

  res_named <- checkNewXProvided(x_new, css_res_named)

  testthat::expect_true(is.list(res_named))
  testthat::expect_identical(names(res_named), c("newX", "newXProvided"))
  
  testthat::expect_true(all(abs(x_new - res_named$newX) < 10^(-9)))
  testthat::expect_true(res_named$newXProvided)

  # Try data.frame input to css and checkNewXProvided

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/2)
  fit_inds <- setdiff(1:n, selec_inds)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkNewXProvided(X_df[fit_inds, ], css_res_df)

  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("newX", "newXProvided"))
  
  testthat::expect_true(is.numeric(res_df$newX))
  testthat::expect_true(is.matrix(res_df$newX))
  testthat::expect_equal(nrow(res_df$newX), length(fit_inds))
  testthat::expect_equal(ncol(res_df$newX), ncol(css_res_df$X))
  testthat::expect_null(colnames(res_df$newX))
  
  testthat::expect_true(is.logical(res_df$newXProvided))
  testthat::expect_equal(length(res_df$newXProvided), 1)
  testthat::expect_true(!is.na(res_df$newXProvided))
  testthat::expect_true(res_df$newXProvided)
  
  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkNewXProvided(X_df[fit_inds, ], css_res_df)

  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("newX", "newXProvided"))
  
  testthat::expect_true(is.numeric(res_df$newX))
  testthat::expect_true(is.matrix(res_df$newX))
  testthat::expect_equal(nrow(res_df$newX), length(fit_inds))
  testthat::expect_equal(ncol(res_df$newX), ncol(css_res_df$X))
  testthat::expect_null(colnames(res_df$newX))
  
  testthat::expect_true(is.logical(res_df$newXProvided))
  testthat::expect_equal(length(res_df$newXProvided), 1)
  testthat::expect_true(!is.na(res_df$newXProvided))
  testthat::expect_true(res_df$newXProvided)
  
})
```

```
## Test passed 😀
```

`formCssDesign()`:


```r
#' Create design matrix of cluster representatives from matrix of raw features
#' using results of css function
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'. For "sparse", all the weight is put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", the weight used for each cluster member is calculated in
#' proportion to the individual selection proportions of each feature. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, the cluster representative
#' is just a simple average of all the cluster members). See Faletto and Bien
#' (2022) for details. Default is "weighted_avg".
#' @param cutoff Numeric; css will return only those clusters with selection
#' proportions equal to at least cutoff. Must be between 0 and 1. Default is 0
#' (in which case all clusters are returned in decreasing order of selection
#' proportion).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param newx A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newx are labeled, the names must match the variable names provided to css.
#' newx may be omitted if train_inds were provided to css to set aside
#' observations for model estimation. If this is the case, then when newx is
#' omitted formCssDesign will return a design matrix of cluster representatives
#' formed from the train_inds observations from the matrix X provided to css.
#' (If no train_inds were provided to css, newX must be provided to
#' formCssDesign.) Default is NA.
#' @return A design matrix with the same number of rows as newx (or the 
#' train_inds provided to css) where the columns are the constructed cluster
#' representatives.
#' @author Gregory Faletto, Jacob Bien
#' @references
<<faletto2022>>
formCssDesign <- function(css_results, weighting="weighted_avg", cutoff=0,
    min_num_clusts=1, max_num_clusts=NA, newx=NA){

    # Check inputs
    ret <- checkFormCssDesignInputs(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts, newx)

    newx <- ret$newx
    max_num_clusts <- ret$max_num_clusts

    rm(ret)

    n <- nrow(newx)
    p <- ncol(newx)

    # Get the names of the selected clusters and the weights for the features
    # within each cluster, according to the provided weighting rule
    weights <- getSelectedClusters(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts)$weights

    n_sel_clusts <- length(weights)

    # Form matrix of cluster representatives of selected clusters
    X_clus_reps <- matrix(rep(as.numeric(NA), n*n_sel_clusts), nrow=n,
        ncol=n_sel_clusts)
    colnames(X_clus_reps) <- rep(as.character(NA), n_sel_clusts)

    for(i in 1:n_sel_clusts){
        clust_i_name <- names(weights)[i]

        stopifnot(length(clust_i_name) == 1)
        stopifnot(clust_i_name %in% names(weights))

        colnames(X_clus_reps)[i] <- clust_i_name

        clust_i <- css_results$clusters[[clust_i_name]]

        stopifnot(length(clust_i) >= 1)
        stopifnot(all(clust_i) %in% 1:p)

        weights_i <- weights[[clust_i_name]]

        stopifnot(length(clust_i) == length(weights_i))

        if(length(weights_i) > 1){
            X_clus_reps[, i] <- newx[, clust_i] %*% weights_i
        } else{
            X_clus_reps[, i] <- newx[, clust_i]*weights_i
        }
    }

    # Check output
    stopifnot(all(!is.na(X_clus_reps)))
    stopifnot(ncol(X_clus_reps) == n_sel_clusts)
    stopifnot(nrow(X_clus_reps) == n)

    return(X_clus_reps)
}

```

`checkFormCssDesignInputs()`:


```r
#' Helper function to check that the inputs to formCssDesign are as expected
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'.
#' @param cutoff Numeric; css will return only those clusters with selection
#' proportions equal to at least cutoff. Must be between 0 and 1.
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.)
#' @param newx A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate the design matrix of cluster
#' representatives. Must contain the same features (in the same
#' number of columns) as the X matrix provided to css, and if the columns of
#' newx are labeled, the names must match the variable names provided to css.
#' newx may be omitted if train_inds were provided to css to set aside
#' observations for model estimation. If this is the case, then when newx is
#' omitted formCssDesign will return a design matrix of cluster representatives
#' formed from the train_inds observations from the matrix X provided to css.
#' (If no train_inds were provided to css, newX must be provided to
#' formCssDesign.)
#' @return A named list with the following elements: \item{newx}{If newx was
#' provided, the provided newx matrix, coerced from a data.frame to a matrix if
#' needed. If newx was not provided, a matrix formed by the train_inds set
#' aside in the original function call to css.} \item{max_num_clusts}{The
#' provided max_num_clusts, coerced to an integer if needed, and coerced to be
#' less than or equal to the total number of clusters.}
#' @author Gregory Faletto, Jacob Bien
checkFormCssDesignInputs <- function(css_results, weighting, cutoff,
    min_num_clusts, max_num_clusts, newx){    
    stopifnot(class(css_results) == "cssr")

    if(length(newx) == 1){
        if(is.na(newx)){
            if(length(css_results$train_inds) == 0){
                stop("If css was not provided with indices to set aside for model training, then newx must be provided to formCssDesign")
            }
            newx <- css_results$X[css_results$train_inds, ]
            # feat_names <- colnames(newx)
        } else{
            results <- checkXInputResults(newx, css_results$X)

            newx <- results$newx
            # feat_names <- results$feat_names

            rm(results)
        }
    } else{
        results <- checkXInputResults(newx, css_results$X)

        newx <- results$newx
        # feat_names <- results$feat_names

        rm(results)
    }

    p <- ncol(newx)

    checkCutoff(cutoff)
    checkWeighting(weighting)
    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))
    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    return(list(newx=newx, max_num_clusts=max_num_clusts))
}
```

Tests for `checkFormCssDesignInputs()`


```r
testthat::test_that("checkFormCssDesignInputs works", {
  set.seed(72617)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  x_new <- matrix(stats::rnorm(8*6), nrow=8, ncol=6)
  y_select <- stats::rnorm(10)
  y_new <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)
  
  res <- checkFormCssDesignInputs(css_results=css_res, weighting="sparse",
                                  cutoff=0.5, min_num_clusts=1,
                                  max_num_clusts=NA, newx=x_new)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newx", "max_num_clusts"))
  
  testthat::expect_true(is.numeric(res$newx))
  testthat::expect_true(is.matrix(res$newx))
  testthat::expect_equal(nrow(res$newx), 8)
  testthat::expect_equal(ncol(res$newx), 6)
  testthat::expect_null(colnames(res$newx))
  testthat::expect_true(all(abs(x_new - res$newX) < 10^(-9)))
  
  testthat::expect_equal(length(res$max_num_clusts), 1)
  testthat::expect_true(is.na(res$max_num_clusts))
  
  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B=10, train_inds=6:10)
  
  # Training indices should be ignored if new x is provided
  
  res <- checkFormCssDesignInputs(css_results=css_res_train,
                                  weighting="weighted_avg", cutoff=0,
                                  min_num_clusts=2, max_num_clusts=NA,
                                  newx=x_new)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newx", "max_num_clusts"))
  
  testthat::expect_true(is.numeric(res$newx))
  testthat::expect_true(is.matrix(res$newx))
  testthat::expect_equal(nrow(res$newx), 8)
  testthat::expect_equal(ncol(res$newx), 6)
  testthat::expect_null(colnames(res$newx))
  testthat::expect_true(all(abs(x_new - res$newX) < 10^(-9)))
  
  # Things should still work if new x is not provided

  res <- checkFormCssDesignInputs(css_results=css_res_train, weighting="sparse",
                                  cutoff=1, min_num_clusts=3,
                                  max_num_clusts=NA, newx=NA)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("newx", "max_num_clusts"))

  testthat::expect_true(is.numeric(res$newx))
  testthat::expect_true(is.matrix(res$newx))
  testthat::expect_equal(nrow(res$newx), length(6:10))
  testthat::expect_equal(ncol(res$newx), 6)
  testthat::expect_null(colnames(res$newx))
  testthat::expect_true(all(abs(x_select[1:5, ] - res$newX) < 10^(-9)))
  
  # Try not providing training indices and omitting newx--should get error
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="sparse",
                                                  cutoff=0.5, min_num_clusts=1,
                                                  max_num_clusts=5, newx=NA),
                         "If css was not provided with indices to set aside for model training, then newx must be provided to formCssDesign", fixed=TRUE)
  
  # Try naming variables

  colnames(x_select) <- LETTERS[1:6]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(checkFormCssDesignInputs(css_results=css_res_named,
                                                    weighting="simple_avg",
                                                    cutoff=0.9,
                                                    min_num_clusts=1,
                                                    max_num_clusts=3,
                                                    newx=x_new),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_new) <- LETTERS[2:7]
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res_named,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2, min_num_clusts=1,
                                                  max_num_clusts=1,
                                                  newx=x_new),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_new) <- LETTERS[1:6]

  res_named <- checkFormCssDesignInputs(css_results=css_res_named,
                                        weighting="sparse", cutoff=0.5,
                                        min_num_clusts=2, max_num_clusts=NA,
                                        newx=x_new)

  testthat::expect_true(is.list(res_named))
  testthat::expect_identical(names(res_named), c("newx", "max_num_clusts"))

  testthat::expect_true(is.numeric(res_named$newx))
  testthat::expect_true(is.matrix(res_named$newx))
  testthat::expect_equal(nrow(res_named$newx), 8)
  testthat::expect_equal(ncol(res_named$newx), 6)
  testthat::expect_null(colnames(res_named$newx))
  testthat::expect_identical(colnames(css_res_named$X), LETTERS[1:6])
  testthat::expect_true(all(abs(x_new - res_named$newX) < 10^(-9)))

  # Try data.frame input to css and checkFormCssDesignInputs

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/2)
  fit_inds <- setdiff(1:n, selec_inds)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkFormCssDesignInputs(css_results=css_res_df,
                                     weighting="simple_avg", cutoff=0.7,
                                     min_num_clusts=3, max_num_clusts=NA,
                                     newx=X_df[fit_inds, ])

  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("newx", "max_num_clusts"))
  
  testthat::expect_true(is.numeric(res_df$newx))
  testthat::expect_true(is.matrix(res_df$newx))
  testthat::expect_null(colnames(res_df$newx))
  testthat::expect_equal(nrow(res_df$newx), length(fit_inds))
  testthat::expect_equal(ncol(res_df$newx), ncol(css_res_df$X))

  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkFormCssDesignInputs(css_results=css_res_df,
                                     weighting="weighted_avg", cutoff=0.3,
                                     min_num_clusts=1, max_num_clusts=4,
                                     newx=X_df[fit_inds, ])

  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("newx", "max_num_clusts"))
  
  testthat::expect_true(is.numeric(res_df$newx))
  testthat::expect_true(is.matrix(res_df$newx))
  testthat::expect_null(colnames(res_df$newx))
  testthat::expect_equal(nrow(res_df$newx), length(fit_inds))
  testthat::expect_equal(ncol(res_df$newx), ncol(css_res_df$X))
  
  ##### Try other bad inputs
  
  colnames(x_new) <- NULL
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=-0.3, min_num_clusts=1,
                                                  max_num_clusts=4,
                                                  newx=x_new),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="sparse",
                                                  cutoff="0.5",
                                                  min_num_clusts=1,
                                                  max_num_clusts=NA, newx=x_new),
                        "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="sparse",
                                                  cutoff=as.numeric(NA),
                                                  min_num_clusts=1,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                        "!is.na(cutoff) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting=c("sparse",
                                                              "simple_avg"),
                                                  cutoff=0.2,
                                                  min_num_clusts=1,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting=1,
                                                  cutoff=0.2,
                                                  min_num_clusts=1,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "Weighting must be a character", fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="spasre",
                                                  cutoff=0.2,
                                                  min_num_clusts=1,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=c(1, 2),
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts="3",
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=0,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=6,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "min_num_clusts <= n_clusters is not TRUE", fixed=TRUE)
  
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=1,
                                                  max_num_clusts="4",
                                                  newx=x_new),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=1,
                                                  max_num_clusts=3.5,
                                                  newx=x_new),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=2,
                                                  max_num_clusts=1,
                                                  newx=x_new),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkFormCssDesignInputs(css_results=css_res,
                                                  weighting="weighted_avg",
                                                  cutoff=0.2,
                                                  min_num_clusts=2,
                                                  max_num_clusts=8,
                                                  newx=x_new),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)

  
})
```

```
## Test passed 🥇
```

Tests for `formCssDesign()`



```r
testthat::test_that("formCssDesign works", {
  set.seed(17230)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  x_new <- matrix(stats::rnorm(8*6), nrow=8, ncol=6)
  y_select <- stats::rnorm(10)
  y_new <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)
  
  res <- formCssDesign(css_res, newx=x_new)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 8)
  testthat::expect_equal(ncol(res), length(css_res$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res$clusters)))
  testthat::expect_true(all(names(css_res$clusters) %in% colnames(res)))
  
  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B=10, train_inds=6:10)

  # Training indices should be ignored if new x is provided

  res <- formCssDesign(css_results=css_res_train, weighting="weighted_avg",
                       cutoff=0, min_num_clusts=2, max_num_clusts=NA,
                       newx=x_new)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 8)
  testthat::expect_equal(ncol(res), length(css_res_train$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res_train$clusters)))
  testthat::expect_true(all(names(css_res_train$clusters) %in% colnames(res)))

  # Things should still work if new x is not provided

  res <- formCssDesign(css_results=css_res_train, weighting="weighted_avg",
                       cutoff=0, min_num_clusts=2, max_num_clusts=NA)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 5)
  testthat::expect_equal(ncol(res), length(css_res_train$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res_train$clusters)))
  testthat::expect_true(all(names(css_res_train$clusters) %in% colnames(res)))

  # Try not providing training indices and omitting newx--should get error
  testthat::expect_error(formCssDesign(css_results=css_res, weighting="sparse",
                                       cutoff=0.5, min_num_clusts=1,
                                       max_num_clusts=5, newx=NA),
                         "If css was not provided with indices to set aside for model training, then newx must be provided to formCssDesign", fixed=TRUE)

  # Try naming variables

  colnames(x_select) <- LETTERS[1:6]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(formCssDesign(css_results=css_res_named,
                                         weighting="simple_avg", cutoff=0.9,
                                         min_num_clusts=1, max_num_clusts=3,
                                         newx=x_new),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_new) <- LETTERS[2:7]
  testthat::expect_error(formCssDesign(css_results=css_res_named,
                                       weighting="weighted_avg", cutoff=0.2,
                                       min_num_clusts=1, max_num_clusts=1,
                                       newx=x_new),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_new) <- LETTERS[1:6]

  res_named <- formCssDesign(css_results=css_res_named,
                                        weighting="sparse", cutoff=0.5,
                                        min_num_clusts=2, max_num_clusts=NA,
                                        newx=x_new)
  
  testthat::expect_true(is.matrix(res_named))
  testthat::expect_true(is.numeric(res_named))
  testthat::expect_equal(nrow(res_named), 8)
  testthat::expect_true(ncol(res_named) <= length(css_res_named$clusters))
  testthat::expect_true(all(colnames(res_named) %in% names(css_res_named$clusters)))

  # Try data.frame input to css and formCssDesign

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/2)
  fit_inds <- setdiff(1:n, selec_inds)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- formCssDesign(css_results=css_res_df, weighting="simple_avg",
                          cutoff=0.7, min_num_clusts=3, max_num_clusts=NA,
                          newx=X_df[fit_inds, ])

  testthat::expect_true(is.matrix(res_df))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(nrow(res_df), length(fit_inds))
  testthat::expect_true(ncol(res_df) <= length(css_res_df$clusters))
  testthat::expect_true(all(colnames(res_df) %in% names(css_res_df$clusters)))

  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- formCssDesign(css_results=css_res_df, weighting="weighted_avg",
                          cutoff=0.3, min_num_clusts=1, max_num_clusts=4,
                          newx=X_df[fit_inds, ])

  testthat::expect_true(is.matrix(res_df))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(nrow(res_df), length(fit_inds))
  testthat::expect_true(ncol(res_df) <= length(css_res_df$clusters))
  testthat::expect_true(all(colnames(res_df) %in% names(css_res_df$clusters)))

  ##### Try other bad inputs

  colnames(x_new) <- NULL

  testthat::expect_error(formCssDesign(css_results=css_res, cutoff=-0.3,
                                       newx=x_new), "cutoff >= 0 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, cutoff="0.5",
                                       newx=x_new),
                         "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res,
                                       cutoff=as.numeric(NA), newx=x_new),
                        "!is.na(cutoff) is not TRUE", fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res,
                                       weighting=c("sparse", "simple_avg"),
                                       newx=x_new),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, weighting=1,
                                       newx=x_new),
                         "Weighting must be a character", fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, weighting="spasre",
                                       newx=x_new),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res,
                                       min_num_clusts=c(1, 2), newx=x_new),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, min_num_clusts="3",
                                       newx=x_new),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, min_num_clusts=0,
                                       newx=x_new),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, min_num_clusts=6,
                                       newx=x_new),
                         "min_num_clusts <= n_clusters is not TRUE", fixed=TRUE)


  testthat::expect_error(formCssDesign(css_results=css_res, max_num_clusts="4",
                                       newx=x_new),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, max_num_clusts=3.5,
                                       newx=x_new),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, min_num_clusts=2,
                                       max_num_clusts=1, newx=x_new),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(formCssDesign(css_results=css_res, max_num_clusts=8,
                                       newx=x_new),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)

  
})
```

```
## Test passed 🥇
```

Finally, tests for `getCssDesign()`




```r
testthat::test_that("getCssDesign works", {
  set.seed(23170)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  x_new <- matrix(stats::rnorm(8*6), nrow=8, ncol=6)
  y_select <- stats::rnorm(10)
  y_new <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)

  res <- getCssDesign(css_res, newX=x_new)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 8)
  testthat::expect_equal(ncol(res), length(css_res$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res$clusters)))
  testthat::expect_true(all(names(css_res$clusters) %in% colnames(res)))
  
  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B=10, train_inds=6:10)

  # Training indices should be ignored if new x is provided

  res <- getCssDesign(css_results=css_res_train, weighting="weighted_avg",
                      min_num_clusts=2, newX=x_new)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 8)
  testthat::expect_equal(ncol(res), length(css_res_train$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res_train$clusters)))
  testthat::expect_true(all(names(css_res_train$clusters) %in% colnames(res)))

  # Things should still work if new x is not provided

  res <- getCssDesign(css_results=css_res_train, min_num_clusts=2)

  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(nrow(res), 5)
  testthat::expect_equal(ncol(res), length(css_res_train$clusters))
  testthat::expect_true(all(colnames(res) %in% names(css_res_train$clusters)))
  testthat::expect_true(all(names(css_res_train$clusters) %in% colnames(res)))

  # Try not providing training indices and omitting newX--should get error
  testthat::expect_error(getCssDesign(css_results=css_res, weighting="sparse",
                                       cutoff=0.5, min_num_clusts=1,
                                       max_num_clusts=5, newX=NA),
                         "css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix", fixed=TRUE)

  # Try naming variables

  colnames(x_select) <- LETTERS[1:6]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(getCssDesign(css_results=css_res_named,
                                         weighting="simple_avg", cutoff=0.9,
                                         min_num_clusts=1, max_num_clusts=3,
                                         newX=x_new),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_new) <- LETTERS[2:7]
  testthat::expect_error(getCssDesign(css_results=css_res_named,
                                      weighting="weighted_avg", cutoff=0.2,
                                      min_num_clusts=1, max_num_clusts=1,
                                      newX=x_new),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_new) <- LETTERS[1:6]

  res_named <- getCssDesign(css_results=css_res_named, weighting="sparse",
                            cutoff=0.5, min_num_clusts=2, max_num_clusts=NA,
                            newX=x_new)

  testthat::expect_true(is.matrix(res_named))
  testthat::expect_true(is.numeric(res_named))
  testthat::expect_equal(nrow(res_named), 8)
  testthat::expect_true(ncol(res_named) <= length(css_res_named$clusters))
  testthat::expect_true(all(colnames(res_named) %in% names(css_res_named$clusters)))

  # Try data.frame input to css and getCssDesign

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/2)
  fit_inds <- setdiff(1:n, selec_inds)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- getCssDesign(css_results=css_res_df, weighting="simple_avg",
                          cutoff=0.7, min_num_clusts=3, max_num_clusts=NA,
                          newX=X_df[fit_inds, ])

  testthat::expect_true(is.matrix(res_df))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(nrow(res_df), length(fit_inds))
  testthat::expect_true(ncol(res_df) <= length(css_res_df$clusters))
  testthat::expect_true(all(colnames(res_df) %in% names(css_res_df$clusters)))

  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- getCssDesign(css_results=css_res_df, weighting="weighted_avg",
                         cutoff=0.3, min_num_clusts=1, max_num_clusts=4,
                         newX=X_df[fit_inds, ])

  testthat::expect_true(is.matrix(res_df))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(nrow(res_df), length(fit_inds))
  testthat::expect_true(ncol(res_df) <= length(css_res_df$clusters))
  testthat::expect_true(all(colnames(res_df) %in% names(css_res_df$clusters)))

  ##### Try other bad inputs

  colnames(x_new) <- NULL

  testthat::expect_error(getCssDesign(css_results=css_res, cutoff=-0.3,
                                       newX=x_new), "cutoff >= 0 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, cutoff="0.5",
                                       newX=x_new),
                         "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res,
                                       cutoff=as.numeric(NA), newX=x_new),
                        "!is.na(cutoff) is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res,
                                       weighting=c("sparse", "simple_avg"),
                                       newX=x_new),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, weighting=1,
                                       newX=x_new),
                         "Weighting must be a character", fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, weighting="spasre",
                                       newX=x_new),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res,
                                       min_num_clusts=c(1, 2), newX=x_new),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, min_num_clusts="3",
                                       newX=x_new),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, min_num_clusts=0,
                                       newX=x_new),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, min_num_clusts=6,
                                       newX=x_new),
                         "min_num_clusts <= n_clusters is not TRUE", fixed=TRUE)


  testthat::expect_error(getCssDesign(css_results=css_res, max_num_clusts="4",
                                       newX=x_new),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, max_num_clusts=3.5,
                                       newX=x_new),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, min_num_clusts=2,
                                       max_num_clusts=1, newX=x_new),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssDesign(css_results=css_res, max_num_clusts=8,
                                       newX=x_new),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)

  
})
```

```
## ── Warning ('<text>:63'): getCssDesign works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. testthat::expect_warning(...)
##  7. litr (local) getCssDesign(...)
##  8. litr (local) checkXInputResults(newX, css_results$X)
## 
## ── Warning ('<text>:63'): getCssDesign works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##   1. testthat::expect_warning(...)
##   7. litr (local) getCssDesign(...)
##   8. litr (local) formCssDesign(...)
##   9. litr (local) checkFormCssDesignInputs(...)
##  10. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:80'): getCssDesign works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) checkXInputResults(newX, css_results$X)
## 
## ── Warning ('<text>:80'): getCssDesign works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) formCssDesign(...)
##  3. litr (local) checkFormCssDesignInputs(...)
##  4. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:101'): getCssDesign works ──────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) checkXInputResults(newX, css_results$X)
## 
## ── Warning ('<text>:101'): getCssDesign works ──────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) formCssDesign(...)
##  3. litr (local) checkFormCssDesignInputs(...)
##  4. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:121'): getCssDesign works ──────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) checkXInputResults(newX, css_results$X)
## 
## ── Warning ('<text>:121'): getCssDesign works ──────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssDesign(...)
##  2. litr (local) formCssDesign(...)
##  3. litr (local) checkFormCssDesignInputs(...)
##  4. litr (local) checkXInputResults(newx, css_results$X)
```

`getCssPreds()`


```r
#' Fit model and generate predictions from new data
#'
#' Generate predictions on test data using cluster stability-selected model.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param testX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate predictions. Must contain the same
#' features (in the same number of columns) as the matrix provided to css, and
#' if the columns of testX are labeled, the names must match the variable names
#' provided to css.
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'. For "sparse", all the weight is put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", the weight used for each cluster member is calculated in
#' proportion to the individual selection proportions of each feature. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, the cluster representative
#' is just a simple average of all the cluster members). See Faletto and Bien
#' (2022) for details. Default is "weighted_avg".
#' @param cutoff Numeric; getCssPreds will make use only of those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is 0 (in which case either all clusters are used, or max_num_clusts
#' are used, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param trainX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to estimate the linear model from the selected
#' clusters. trainX is only necessary to provide if no train_inds were
#' designated in the css function call to set aside observations for model
#' estimation (though even if train_inds was provided, trainX and trianY will be
#' used for model estimation if they are both provided to getCssPreds). Must 
#' contain the same features (in the same number of columns) as the matrix 
#' provided to css, and if the columns of trainX are labeled, the names must
#' match the variable names provided to css. Default is NA (in which case
#' getCssPreds uses the observations from the train_inds that were provided to
#' css to estimate a linear model).
#' @param trainY The response corresponding to trainX. Must be a real-valued
#' response (unlike in the general css setup) because predictions will be
#' generated by an ordinary least squares model. Must have the same length as
#' the number of rows of trainX. Like trainX, only needs to be provided if no
#' observations were set aside for model estimation by the parameter train_inds
#' in the css function call. Default is NA (in which case getCssPreds uses the
#' observations from the train_inds that were provided to css).
#' @return A vector of predictions corresponding to the observations from testX.
#' @author Gregory Faletto, Jacob Bien
#' @references 
<<faletto2022>>
#' @export
getCssPreds <- function(css_results, testX, weighting="weighted_avg", cutoff=0,
    min_num_clusts=1, max_num_clusts=NA, trainX=NA, trainY=NA){
    # TODO(gregfaletto) Consider adding an argument for a user-provided prediction
    # function in order to allow for more general kinds of predictions than
    # OLS.

    # Check inputs
    
    check_list <- checkGetCssPredsInputs(css_results, testX, weighting, cutoff,
        min_num_clusts, max_num_clusts, trainX, trainY)

    trainXProvided <- check_list$trainXProvided
    trainX <- check_list$trainX
    testX <- check_list$testX
    feat_names <- check_list$feat_names
    max_num_clusts <- check_list$max_num_clusts

    rm(check_list)

    n_train <- nrow(trainX)
    n <- nrow(testX)
    p <- ncol(testX)

    # Take provided training design matrix and testX and turn them into
    # matrices of cluster representatives using information from css_results
    if(trainXProvided){
        train_X_clusters <- formCssDesign(css_results, weighting, cutoff,
            min_num_clusts, max_num_clusts, newx=trainX)
        if(!is.numeric(trainY) & !is.integer(trainY)){
            stop("The provided trainY must be real-valued, because predictions will be generated by ordinary least squares regression.")
        }
        y_train <- trainY
    } else{
        train_X_clusters <- formCssDesign(css_results, weighting, cutoff,
            min_num_clusts, max_num_clusts)
        y_train <- css_results$y[css_results$train_inds]
        if(!is.numeric(y_train) & !is.integer(y_train)){
            stop("Can't generated predictions from the data that was provided to css because the provided y was not real-valued (getCssPreds generated predictions using ordinary least squares regression).")
        }
    }

    stopifnot(length(y_train) == nrow(train_X_clusters))

    testX_clusters <- formCssDesign(css_results, weighting, cutoff,
        min_num_clusts, max_num_clusts, newx=testX)

    stopifnot(ncol(testX_clusters) == ncol(train_X_clusters))

    # Get names for clusters
    clust_X_names <- paste("c_fit_", 1:ncol(testX_clusters), sep="")
    if(!is.null(colnames(train_X_clusters))){
        stopifnot(identical(colnames(train_X_clusters), colnames(testX_clusters)))
        clust_X_names <- colnames(train_X_clusters)
    }

    # Fit linear model on training data via OLS
    if(nrow(train_X_clusters) < ncol(train_X_clusters)){
        err_mess <- paste("css not provided with enough indices to fit OLS model for predictions (number of training indices: ",
            nrow(train_X_clusters), ", number of clusters: ",
            ncol(train_X_clusters),
            "). Try reducing number of clusters by increasing cutoff, or re-run css with a larger number of training indices.",
            sep="")
        stop(err_mess)
    }

    df <- data.frame(y=y_train, train_X_clusters)
    colnames(df)[2:ncol(df)] <- clust_X_names
    model <- stats::lm(y ~., data=df)

    # Use fitted model to generate predictions on testX
    df_test <- data.frame(testX_clusters)
    colnames(df_test) <- clust_X_names
    predictions <- stats::predict.lm(model, newdata=df_test)
    names(predictions) <- NULL

    # Check output
    stopifnot(is.numeric(predictions) | is.integer(predictions))
    stopifnot(length(predictions) == n)
    stopifnot(all(!is.na(predictions)))

    return(predictions)
}
```


`checkGetCssPredsInputs()`:


```r
#' Helper function to confirm that inputs to the function getCssPreds are as
#' expected, and modify inputs if needed.
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param testX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate predictions. Must contain the same
#' features (in the same number of columns) as the matrix provided to css.
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'. For "sparse", all the weight is put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", the weight used for each cluster member is calculated in
#' proportion to the individual selection proportions of each feature. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, the cluster representative
#' is just a simple average of all the cluster members). See Faletto and Bien
#' (2022) for details. Default is "weighted_avg".
#' @param cutoff Numeric; getCssPreds will make use only of those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is 0 (in which case either all clusters are used, or max_num_clusts
#' are used, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param trainX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to estimate the linear model from the selected
#' clusters. trainX is only necessary to provide if no train_inds were
#' designated in the css function call to set aside observations for model
#' estimation (though even if train_inds was provided, trainX and trianY will be
#' used for model estimation if they are both provided to getCssPreds). Must 
#' contain the same features (in the same number of columns) as the matrix 
#' provided to css, and if the columns of trainX are labeled, the names must
#' match the variable names provided to css. Default is NA (in which case
#' getCssPreds uses the observations from the train_inds that were provided to
#' css to estimate a linear model).
#' @param trainY The response corresponding to trainX. Must be a real-valued
#' response (unlike in the general css setup) because predictions will be
#' generated by an ordinary least squares model. Must have the same length as
#' the number of rows of trainX. Like trainX, only needs to be provided if no
#' observations were set aside for model estimation by the parameter train_inds
#' in the css function call. Default is NA (in which case getCssPreds uses the
#' observations from the train_inds that were provided to css).
#' @return A named list with the following elements: \item{trainXProvided}{
#' Logical; indicates whether a valid trainX input was provided.} \item{trainX}{
#' The provided trainX matrix, coerced from a data.frame to a matrix if the
#' provided trainX was a data.frame. (If a valid trainX was not provided, this
#' output simply passes whatever was provided as trainX.)} \item{testX}{The
#' provided testX matrix, coerced from a data.frame to a matrix if the provided
#' testX was a data.frame.} \item{feat_names}{A character vector containing the
#' column names of testX (if the provided testX had column names). If the
#' provided testX did not have column names, feat_names will be NA.}
#' \item{max_num_clusts}{The provided max_num_clusts, coerced to an integer if
#' needed, and coerced to be less than or equal to the total number of clusters
#' from the output of css_results.}
#' @author Gregory Faletto, Jacob Bien
checkGetCssPredsInputs <- function(css_results, testX, weighting, cutoff,
    min_num_clusts, max_num_clusts, trainX, trainY){
    # Check inputs
    stopifnot(class(css_results) == "cssr")

    check_results <- checkNewXProvided(trainX, css_results)

    trainX <- check_results$newX
    trainXProvided <- check_results$newXProvided

    rm(check_results)

    n_train <- nrow(trainX)

    if(trainXProvided){
        if(all(!is.na(trainY)) & length(trainY) > 1){
            stopifnot(is.numeric(trainY))
            stopifnot(n_train == length(trainY))
        } else{
            if(length(css_results$train_inds) == 0){
                stop("css was not provided with indices to set aside for model training (train_inds), so must provide both trainX and trainY in order to generate predictions")
            }
            trainXProvided <- FALSE
            warning("trainX provided but no trainY provided; instead, training model using the train_inds observations provided to css to set aside for model training.")
        }
    } else{
        if(length(css_results$train_inds) == 0){
            stop("css was not provided with indices to set aside for model training (train_inds), so must provide both trainX and trainY in order to generate predictions")
        }
        if(all(!is.na(trainY)) & length(trainY) > 1){
            warning("trainY provided but no trainX provided; instead, training model using the train_inds observations provided to css to set aside for model training.")
        }
    }

    results <- checkXInputResults(testX, css_results$X)

    testX <- results$newx
    feat_names <- results$feat_names

    if(all(!is.na(feat_names))){
        stopifnot(length(feat_names) == ncol(testX))
        stopifnot(!("(Intercept)" %in% feat_names))
        colnames(testX) <- feat_names
    }

    rm(results)

    n <- nrow(testX)
    p <- ncol(testX)

    stopifnot(n >= 1)
    stopifnot(p == ncol(trainX))
    if(!is.null(colnames(trainX)) & is.null(colnames(testX))){
        warning("Column names were provided for trainX but not for testX (are you sure they both contain identical features in the same order?)")
    }
    if(is.null(colnames(trainX)) & !is.null(colnames(testX))){
        warning("Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)")
    }
    if(!is.null(colnames(trainX)) & !is.null(colnames(testX))){
        stopifnot(all(colnames(trainX) == colnames(testX)))
    }

    checkCutoff(cutoff)
    checkWeighting(weighting)
    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))
    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    return(list(trainXProvided=trainXProvided, trainX=trainX, testX=testX,
        feat_names=feat_names, max_num_clusts=max_num_clusts))

}
```

Tests for `checkGetCssPredsInputs()`



```r
testthat::test_that("checkGetCssPredsInputs works", {
  set.seed(17081)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  x_train <- matrix(stats::rnorm(8*6), nrow=8, ncol=6)
  x_pred <- matrix(stats::rnorm(7*6), nrow=7, ncol=6)
  y_select <- stats::rnorm(10)
  y_train <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)

  res <- checkGetCssPredsInputs(css_res, testX=x_pred, weighting="simple_avg",
                                cutoff=0.05, min_num_clusts=1,
                                max_num_clusts=NA, trainX=x_train,
                                trainY=y_train)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("trainXProvided", "trainX", "testX",
                                           "feat_names", "max_num_clusts"))
  
  testthat::expect_true(!is.na(res$trainXProvided))
  testthat::expect_equal(length(res$trainXProvided), 1)
  testthat::expect_true(is.logical(res$trainXProvided))
  testthat::expect_true(res$trainXProvided)
  
  testthat::expect_true(all(!is.na(res$trainX)))
  testthat::expect_true(is.matrix(res$trainX))
  testthat::expect_true(is.numeric(res$trainX))
  testthat::expect_equal(nrow(res$trainX), 8)
  testthat::expect_equal(ncol(res$trainX), 6)
  testthat::expect_true(all(abs(x_train - res$trainX) < 10^(-9)))
  
  testthat::expect_true(all(!is.na(res$testX)))
  testthat::expect_true(is.matrix(res$testX))
  testthat::expect_true(is.numeric(res$testX))
  testthat::expect_equal(nrow(res$testX), 7)
  testthat::expect_equal(ncol(res$testX), 6)
  testthat::expect_true(all(abs(x_pred - res$testX) < 10^(-9)))
  
  testthat::expect_true(is.character(res$feat_names))
  testthat::expect_true(is.na(res$feat_names))
  
  testthat::expect_true(is.na(res$max_num_clusts))
  testthat::expect_true(length(res$max_num_clusts) == 1)
  
  ##### Try other bad inputs
  
  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="weighted_avg",
                                                cutoff=-0.5, min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse",
                                                cutoff="0.3", min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse",
                                                cutoff=as.numeric(NA),
                                                min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                        "!is.na(cutoff) is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting=c("sparse",
                                                            "simple_avg"),
                                                cutoff=0.1,
                                                min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting=2, cutoff=0.1,
                                                min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "Weighting must be a character", fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="spasre", cutoff=0.1,
                                                min_num_clusts=1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse", cutoff=0.1,
                                                min_num_clusts=c(1, 2),
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="weighted_avg",
                                                cutoff=0.1, min_num_clusts="2",
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="simple_avg",
                                                cutoff=0.1, min_num_clusts=0,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="weighted_avg",
                                                cutoff=0.1, min_num_clusts=10,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "min_num_clusts <= p is not TRUE", fixed=TRUE)


  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="simple_avg",
                                                cutoff=0.1, min_num_clusts=1,
                                                max_num_clusts="5",
                                                trainX=x_train, trainY=y_train),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse",
                                                cutoff=0.1, min_num_clusts=1,
                                                max_num_clusts=4.5,
                                                trainX=x_train, trainY=y_train),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse",
                                                cutoff=0.1, min_num_clusts=3,
                                                max_num_clusts=2,
                                                trainX=x_train, trainY=y_train),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                                weighting="sparse",
                                                cutoff=0.1, min_num_clusts=1,
                                                max_num_clusts=10,
                                                trainX=x_train, trainY=y_train),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)

  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B=10, train_inds=6:10)

  # Training indices should be ignored if new x is provided

  res <- checkGetCssPredsInputs(css_res_train, testX=x_pred,
                                weighting="weighted_avg",
                                cutoff=0, min_num_clusts=1,
                                max_num_clusts=NA, trainX=x_train,
                                trainY=y_train)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("trainXProvided", "trainX", "testX",
                                           "feat_names", "max_num_clusts"))

  testthat::expect_true(!is.na(res$trainXProvided))
  testthat::expect_equal(length(res$trainXProvided), 1)
  testthat::expect_true(is.logical(res$trainXProvided))
  testthat::expect_true(res$trainXProvided)

  testthat::expect_true(all(!is.na(res$trainX)))
  testthat::expect_true(is.matrix(res$trainX))
  testthat::expect_true(is.numeric(res$trainX))
  testthat::expect_equal(nrow(res$trainX), 8)
  testthat::expect_equal(ncol(res$trainX), 6)
  testthat::expect_true(all(abs(x_train - res$trainX) < 10^(-9)))

  testthat::expect_true(all(!is.na(res$testX)))
  testthat::expect_true(is.matrix(res$testX))
  testthat::expect_true(is.numeric(res$testX))
  testthat::expect_equal(nrow(res$testX), 7)
  testthat::expect_equal(ncol(res$testX), 6)
  testthat::expect_true(all(abs(x_pred - res$testX) < 10^(-9)))

  testthat::expect_true(is.character(res$feat_names))
  testthat::expect_true(is.na(res$feat_names))

  testthat::expect_true(is.na(res$max_num_clusts))
  testthat::expect_true(length(res$max_num_clusts) == 1)

  # Things should still work if new x is not provided
  
  res <- checkGetCssPredsInputs(css_res_train, testX=x_pred,
                                weighting="weighted_avg",
                                cutoff=0, min_num_clusts=1,
                                max_num_clusts=NA, trainX=NA, trainY=NA)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("trainXProvided", "trainX", "testX",
                                           "feat_names", "max_num_clusts"))

  testthat::expect_true(!is.na(res$trainXProvided))
  testthat::expect_equal(length(res$trainXProvided), 1)
  testthat::expect_true(is.logical(res$trainXProvided))
  testthat::expect_true(!res$trainXProvided)

  testthat::expect_true(all(!is.na(res$trainX)))
  testthat::expect_true(is.matrix(res$trainX))
  testthat::expect_true(is.numeric(res$trainX))
  testthat::expect_equal(nrow(res$trainX), 5)
  testthat::expect_equal(ncol(res$trainX), 6)
  testthat::expect_true(all(abs(x_select[6:10, ] - res$trainX) < 10^(-9)))

  testthat::expect_true(all(!is.na(res$testX)))
  testthat::expect_true(is.matrix(res$testX))
  testthat::expect_true(is.numeric(res$testX))
  testthat::expect_equal(nrow(res$testX), 7)
  testthat::expect_equal(ncol(res$testX), 6)
  testthat::expect_true(all(abs(x_pred - res$testX) < 10^(-9)))

  testthat::expect_true(is.character(res$feat_names))
  testthat::expect_true(is.na(res$feat_names))

  testthat::expect_true(is.na(res$max_num_clusts))
  testthat::expect_true(length(res$max_num_clusts) == 1)


  # Try not providing training indices and omitting newX--should get error
  testthat::expect_error(checkGetCssPredsInputs(css_res, testX=x_pred,
                                weighting="sparse",
                                cutoff=0, min_num_clusts=1,
                                max_num_clusts=NA, trainX=NA, trainY=NA),
                         "css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix", fixed=TRUE)

  # Try naming variables

  colnames(x_select) <- LETTERS[1:6]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(checkGetCssPredsInputs(css_res_named, testX=x_pred,
                                weighting="simple_avg", cutoff=0,
                                min_num_clusts=1, max_num_clusts=NA,
                                trainX=x_train, trainY=y_train),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_train) <- LETTERS[2:7]
  colnames(x_pred) <- LETTERS[1:6]
  testthat::expect_error(checkGetCssPredsInputs(css_res_named, testX=x_pred,
                                weighting="weighted_avg", cutoff=0,
                                min_num_clusts=1, max_num_clusts=NA,
                                trainX=x_train, trainY=y_train),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_train) <- LETTERS[1:6]
  colnames(x_pred) <- LETTERS[2:7]
  testthat::expect_error(checkGetCssPredsInputs(css_res_named, testX=x_pred,
                                weighting="sparse", cutoff=0,
                                min_num_clusts=1, max_num_clusts=NA,
                                trainX=x_train, trainY=y_train),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_pred) <- LETTERS[1:6]

  res_named <- checkGetCssPredsInputs(css_res_named, testX=x_pred,
                                      weighting="simple_avg", cutoff=0,
                                      min_num_clusts=1, max_num_clusts=NA,
                                      trainX=x_train, trainY=y_train)

  testthat::expect_true(is.list(res_named))
  testthat::expect_identical(names(res_named), c("trainXProvided", "trainX", "testX",
                                           "feat_names", "max_num_clusts"))

  testthat::expect_true(all(!is.na(res_named$trainX)))
  testthat::expect_true(is.matrix(res_named$trainX))
  testthat::expect_true(is.numeric(res_named$trainX))
  testthat::expect_equal(nrow(res_named$trainX), 8)
  testthat::expect_equal(ncol(res_named$trainX), 6)
  testthat::expect_true(all(abs(x_train - res_named$trainX) < 10^(-9)))
  
  testthat::expect_true(is.character(res_named$feat_names))
  testthat::expect_identical(res_named$feat_names, LETTERS[1:6])

  # Try data.frame input to css and checkGetCssPredsInputs

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/3)
  train_inds <- (max(selec_inds) + 1):(2*round(n/3))
  test_inds <- setdiff(1:n, c(selec_inds, train_inds))

  css_res_df <- css(X=X_df[c(selec_inds, train_inds), ],
                    y=y[c(selec_inds, train_inds)], lambda=0.01, B = 10,
                    train_inds=train_inds)
  
  res_df <- checkGetCssPredsInputs(css_res_df, testX=X_df[test_inds, ],
                                   weighting="sparse", cutoff=0,
                                   min_num_clusts=1, max_num_clusts=NA,
                                   trainX=NA, trainY=NA)
  
  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("trainXProvided", "trainX",
                                              "testX","feat_names",
                                              "max_num_clusts"))
  
  testthat::expect_true(all(!is.na(res_df$trainX)))
  testthat::expect_true(is.matrix(res_df$trainX))
  testthat::expect_true(is.numeric(res_df$trainX))
  testthat::expect_equal(nrow(res_df$trainX), length(train_inds))
  
  stopifnot(nrow(css_res_df$X) >= max(train_inds))
  train_mat <- css_res_df$X[train_inds, ]

  testthat::expect_equal(ncol(res_df$trainX), ncol(train_mat))
  testthat::expect_true(all(abs(train_mat - res_df$trainX) < 10^(-9)))
  testthat::expect_identical(colnames(res_df$trainX), colnames(train_mat))

  testthat::expect_true(all(!is.na(res_df$testX)))
  testthat::expect_true(is.matrix(res_df$testX))
  testthat::expect_true(is.numeric(res_df$testX))
  testthat::expect_equal(nrow(res_df$testX), length(test_inds))
  
  test_mat <- stats::model.matrix(~ ., X_df[test_inds, ])
  test_mat <- test_mat[, colnames(test_mat) != "(Intercept)"]
  
  testthat::expect_equal(ncol(res_df$testX), ncol(test_mat))
  testthat::expect_true(all(abs(test_mat - res_df$testX) < 10^(-9)))
  testthat::expect_identical(colnames(res_df$testX), colnames(test_mat))
  testthat::expect_identical(colnames(res_df$testX), colnames(res_df$trainX))

  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- checkGetCssPredsInputs(css_res_df, testX=X_df[test_inds, ],
                                   weighting="simple_avg", cutoff=0.3,
                                   min_num_clusts=1, max_num_clusts=4,
                                   trainX=X_df[train_inds, ],
                                   trainY=y[train_inds])
  
  testthat::expect_true(is.list(res_df))
  testthat::expect_identical(names(res_df), c("trainXProvided", "trainX",
                                              "testX","feat_names",
                                              "max_num_clusts"))
  
  testthat::expect_true(all(!is.na(res_df$trainX)))
  testthat::expect_true(is.matrix(res_df$trainX))
  testthat::expect_true(is.numeric(res_df$trainX))
  testthat::expect_equal(nrow(res_df$trainX), length(train_inds))
  
  train_mat <- stats::model.matrix(~ ., X_df[train_inds, ])
  train_mat <- train_mat[, colnames(train_mat) != "(Intercept)"]

  testthat::expect_equal(ncol(res_df$trainX), ncol(train_mat))
  testthat::expect_true(all(abs(train_mat - res_df$trainX) < 10^(-9)))

  testthat::expect_true(all(!is.na(res_df$testX)))
  testthat::expect_true(is.matrix(res_df$testX))
  testthat::expect_true(is.numeric(res_df$testX))
  testthat::expect_equal(nrow(res_df$testX), length(test_inds))
  
  test_mat <- stats::model.matrix(~ ., X_df[test_inds, ])
  test_mat <- test_mat[, colnames(test_mat) != "(Intercept)"]
  
  testthat::expect_equal(ncol(res_df$testX), ncol(test_mat))
  testthat::expect_true(all(abs(test_mat - res_df$testX) < 10^(-9)))

  
})
```

```
## ── Warning ('<text>:251'): checkGetCssPredsInputs works ────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. testthat::expect_warning(...)
##  7. litr (local) checkGetCssPredsInputs(...)
##  8. litr (local) checkXInputResults(testX, css_results$X)
## 
## ── Warning ('<text>:278'): checkGetCssPredsInputs works ────────────────────────
## Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)
## Backtrace:
##  1. litr (local) checkGetCssPredsInputs(...)
## 
## ── Warning ('<text>:357'): checkGetCssPredsInputs works ────────────────────────
## Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)
## Backtrace:
##  1. litr (local) checkGetCssPredsInputs(...)
```

Finally, tests for `getCssPreds()`


```r
testthat::test_that("getCssPreds works", {
  set.seed(70811)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  x_train <- matrix(stats::rnorm(8*6), nrow=8, ncol=6)
  x_pred <- matrix(stats::rnorm(7*6), nrow=7, ncol=6)
  y_select <- stats::rnorm(10)
  y_train <- stats::rnorm(8)

  good_clusters <- list("red"=1:2, "blue"=3:4, "green"=5)

  css_res <- css(X=x_select, y=y_select, lambda=0.01, clusters=good_clusters,
                 B = 10)

  res <- getCssPreds(css_res, testX=x_pred, trainX=x_train, trainY=y_train)

  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 7)
  
  ##### Try other bad inputs

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, cutoff=-0.5,
                                     trainX=x_train, trainY=y_train),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, cutoff="0.3",
                                     trainX=x_train, trainY=y_train),
                         "is.numeric(cutoff) | is.integer(cutoff) is not TRUE",
                        fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred,
                                     cutoff=as.numeric(NA), trainX=x_train,
                                     trainY=y_train),
                        "!is.na(cutoff) is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred,
                                     weighting=c("sparse", "simple_avg"),
                                     trainX=x_train, trainY=y_train),
                         "length(weighting) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, weighting=2,
                                     trainX=x_train, trainY=y_train),
                         "Weighting must be a character", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, weighting="spasre",
                                     trainX=x_train, trainY=y_train),
                         "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
                         fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred,
                                     min_num_clusts=c(1, 2), trainX=x_train,
                                     trainY=y_train),
                         "length(min_num_clusts) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, min_num_clusts="2",
                                     trainX=x_train, trainY=y_train),
                         "is.numeric(min_num_clusts) | is.integer(min_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, min_num_clusts=0,
                                     trainX=x_train, trainY=y_train),
                         "min_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, min_num_clusts=10,
                                     trainX=x_train, trainY=y_train),
                         "min_num_clusts <= p is not TRUE", fixed=TRUE)


  testthat::expect_error(getCssPreds(css_res, testX=x_pred, max_num_clusts="5",
                                     trainX=x_train, trainY=y_train),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, max_num_clusts=4.5,
                                     trainX=x_train, trainY=y_train),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, min_num_clusts=3,
                                     max_num_clusts=2, trainX=x_train,
                                     trainY=y_train),
                         "max_num_clusts >= min_num_clusts is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, max_num_clusts=10,
                                     trainX=x_train, trainY=y_train),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)

  # Add training indices
  css_res_train <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B=10, train_inds=6:10)

  # Training indices should be ignored if new x is provided

  res <- getCssPreds(css_res_train, testX=x_pred, trainX=x_train,
                     trainY=y_train)

  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 7)
  
  # Things should still work if new x is not provided

  res <- getCssPreds(css_res_train, testX=x_pred)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 7)

  # Try not providing training indices and omitting newX--should get error
  testthat::expect_error(getCssPreds(css_res, testX=x_pred),
                         "css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix", fixed=TRUE)

  # Try naming variables

  colnames(x_select) <- LETTERS[1:6]
  css_res_named <- css(X=x_select, y=y_select, lambda=0.01,
                       clusters=good_clusters, B = 10)

  # Named variables for css matrix but not new one--should get a warning
  testthat::expect_warning(getCssPreds(css_res_named, testX=x_pred,
                                       trainX=x_train, trainY=y_train),
                           "New X provided had no variable names (column names) even though the X provided to css did.", fixed=TRUE)

  # Try mismatching variable names
  colnames(x_train) <- LETTERS[2:7]
  colnames(x_pred) <- LETTERS[1:6]
  testthat::expect_error(getCssPreds(css_res_named, testX=x_pred,
                                     trainX=x_train, trainY=y_train),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_train) <- LETTERS[1:6]
  colnames(x_pred) <- LETTERS[2:7]
  testthat::expect_error(getCssPreds(css_res_named, testX=x_pred,
                                     trainX=x_train, trainY=y_train),
                         "identical(feat_names, colnames(css_X)) is not TRUE",
                         fixed=TRUE)

  colnames(x_pred) <- LETTERS[1:6]

  res_named <- getCssPreds(css_res_named, testX=x_pred, trainX=x_train,
                           trainY=y_train)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 7)

  # Try data.frame input to css and getCssPreds

  X_df <- datasets::mtcars

  n <- nrow(X_df)
  y <- stats::rnorm(n)

  selec_inds <- 1:round(n/3)
  train_inds <- (max(selec_inds) + 1):(max(selec_inds) + 17)
  test_inds <- setdiff(1:n, c(selec_inds, train_inds))

  css_res_df <- css(X=X_df[c(selec_inds, train_inds), ],
                    y=y[c(selec_inds, train_inds)], lambda=0.01, B = 10,
                    train_inds=train_inds)

  res_df <- getCssPreds(css_res_df, testX=X_df[test_inds, ])

  testthat::expect_true(all(!is.na(res_df)))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(length(res_df), length(test_inds))
  
  # Try again with X as a dataframe with factors (number of columns of final
  # design matrix after one-hot encoding factors won't match number of columns
  # of X_df)
  X_df$cyl <- as.factor(X_df$cyl)
  X_df$vs <- as.factor(X_df$vs)
  X_df$am <- as.factor(X_df$am)
  X_df$gear <- as.factor(X_df$gear)
  X_df$carb <- as.factor(X_df$carb)

  css_res_df <- css(X=X_df[selec_inds, ], y=y[selec_inds], lambda=0.01, B = 10)
  res_df <- getCssPreds(css_res_df, testX=X_df[test_inds, ],
                        trainX=X_df[train_inds, ], trainY=y[train_inds])
  
  # TODO(gregfaletto): known issue--the above code produces the following
  # undesired warnings:
  # 1: In checkGetCssPredsInputs(css_results, testX, weighting, cutoff,  :
  # Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)
  # 2: In checkXInputResults(newx, css_results$X) :
  # New X provided had no variable names (column names) even though the X provided to css did.

  testthat::expect_true(all(!is.na(res_df)))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(length(res_df), length(test_inds))

})
```

```
## ── Warning ('<text>:122'): getCssPreds works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. testthat::expect_warning(...)
##  7. litr (local) getCssPreds(...)
##  8. litr (local) checkGetCssPredsInputs(...)
##  9. litr (local) checkXInputResults(testX, css_results$X)
## 
## ── Warning ('<text>:122'): getCssPreds works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##   1. testthat::expect_warning(...)
##   7. litr (local) getCssPreds(...)
##   8. litr (local) formCssDesign(...)
##   9. litr (local) checkFormCssDesignInputs(...)
##  10. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:122'): getCssPreds works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##   1. testthat::expect_warning(...)
##   7. litr (local) getCssPreds(...)
##   8. litr (local) formCssDesign(...)
##   9. litr (local) checkFormCssDesignInputs(...)
##  10. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:143'): getCssPreds works ───────────────────────────────────
## Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)
## Backtrace:
##  1. litr (local) getCssPreds(...)
##  2. litr (local) checkGetCssPredsInputs(...)
## 
## ── Warning ('<text>:143'): getCssPreds works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssPreds(...)
##  2. litr (local) formCssDesign(...)
##  3. litr (local) checkFormCssDesignInputs(...)
##  4. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:181'): getCssPreds works ───────────────────────────────────
## Column names were provided for testX but not for trainX (are you sure they both contain identical features in the same order?)
## Backtrace:
##  1. litr (local) getCssPreds(...)
##  2. litr (local) checkGetCssPredsInputs(...)
## 
## ── Warning ('<text>:181'): getCssPreds works ───────────────────────────────────
## New X provided had no variable names (column names) even though the X provided to css did.
## Backtrace:
##  1. litr (local) getCssPreds(...)
##  2. litr (local) formCssDesign(...)
##  3. litr (local) checkFormCssDesignInputs(...)
##  4. litr (local) checkXInputResults(newx, css_results$X)
## 
## ── Warning ('<text>:181'): getCssPreds works ───────────────────────────────────
## prediction from a rank-deficient fit may be misleading
## Backtrace:
##  1. litr (local) getCssPreds(...)
##  2. stats::predict.lm(model, newdata = df_test)
```

# Other useful functions {#other-useful}

The following are some other functions that are useful for specific tasks.

* `genClusteredData()` generates jointly Gaussian data sets that include clusters of features that are correlated with a latent feature (the latent features themselves are also provided).
* `getLassoLambda()` uses a data set to choose a good value of lambda for subsamples of size n/2 (as in cluster stability selection) by cross-validation.
* `getModelSize()` estimates the best size for a lasso model on a provided data set.
* `printCssDf()` converts the output of the `css()` function (an object of class cssr) to a data.frame that is ready to be printed. We provide end-user access to `printCssDf()` because this data.frame itself may be useful as an object that summarizes the output from `css()`.
* `print.cssr()` is the print function for objects of class cssr.

* `genClusteredData()`
  - `checkGenClusteredDataInputs()` checks that the inputs to `genClusteredData()` are as expected.
  - `makeCovarianceMatrix()` generates a covariance matrix for a jointly Gaussian random variable with distribution matching the provided inputs.
  - `makeCoefficients()` generates a coefficient vector for the response according to the provided inputs.
  - Finally, `genMuXZSd()` draws a random Z (matrix of latent variables), X (observed matrix, containing, in general, features correlated with latent features, directly observed features that are associated with y, and noise features), conditional means mu, and observed responses y.
* `getLassoLambda()`
* `getModelSize()`
* `printCssDf()`
  - `getSelectionPrototypes()` identifies the prototypes from selected clusters (the feature in each cluster that is most correlated with the response)
* `print.cssr()`

`genClusteredData()`


```r
#' Generate randomly sampled data including noisy observations of latent
#' variables
#'
#' TODO(gregfaletto) change cluster_size into a vector of sizes (maybe also
#' deprecate n_clusters as an input, since this would be inferred by the length
#' of cluster_sizes?)
#' Generate a data set including latent features Z, observed features X (which
#' may include noisy or noiseless observations of the latent features in Z),
#' an observed response y which is a linear model of features from Z and X as
#' well as independent mean zero noise, and mu (the responses from y without
#' the added noise). Data is generated in the same way as in the simulations
#' from Faletto and Bien (2022).
#' @param n Integer or numeric; the number of observations to generate. (The
#' generated X and Z will have n rows, and the generated y and mu will have
#' length n.)
#' @param p Integer or numeric; the number of features to generate. The
#' generated X will have p columns.
#' @param k_unclustered Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_unclustered.
#' @param cluster_size Integer or numeric; for each of the n_clusters latent
#' variables, X will contain cluster_size noisy proxies that are correlated with
#' the latent variable.
#' @param n_clusters Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param sig_clusters Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). Must be less than or equal to
#' n_clusters. Default is 1.
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. Can't equal 0. Default is 0.9.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). Can't equal 0. Default is 1.
#' @param beta_latent Integer or numeric; the coefficient used for all
#' sig_clusters latent variables that have nonzero coefficients in the true
#' model for y. Can't equal 0. Default is 1.5.
#' @param beta_unclustered Integer or numeric; the maximum coefficient in the
#' model for y among the k_unclustered features in X not generated from the
#' latent variables. The coefficients of the features will be
#' beta_unclustered/sqrt(1:k_unclustered). Can't equal 0. Default is 1.
#' @param snr Integer or numeric; the signal-to-noise ratio of the response
#' y. If sigma_eps_sq is not specified, the variance of the noise in y will be
#' calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
#' snr and sigma_eps_sq must be specified. Default is NA.
#' @param sigma_eps_sq Integer or numeric; the variance on the noise added
#' to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.
#' @return A list of the following elements. \item{X}{An n x p numeric matrix of
#' n observations from a p-dimensional multivariate normal distribution
#' generated using the specified parameters. The first n_clusters times
#' cluster_size features will be the clusters of features correlated with the
#' n_clusters latent variables. The next k_unclustered features will be the
#' "weak signal" features, and the remaining p - n_clusters*cluster_size -
#' k_unclustered features will be the unclustered noise features.} \item{y}{A
#' length n numeric vector; the response generated from X, the latent features
#' from Z, and the coefficient vector, along with additive noise.} \item{Z}{The
#' latent features; either a numeric vector (if n_clusters > 1) or a numeric
#' matrix (if n_clusters > 1). Note that (X, Z) is multivariate Gaussian.}
#' item{mu}{A length `n` numeric vector; the expected response given X, Z, and
#' the true coefficient vector (equal to y minus the added noise).}
#' @author Gregory Faletto, Jacob Bien
#' @references
<<faletto2022>>
#' @export
genClusteredData <- function(n, p, k_unclustered, cluster_size, n_clusters=1,
    sig_clusters=1, rho=0.9, var=1, beta_latent=1.5, beta_unclustered=1,
    snr=as.numeric(NA), sigma_eps_sq=as.numeric(NA)){

    # Check inputs
    checkGenClusteredDataInputs(p, k_unclustered, cluster_size, n_clusters,
        sig_clusters, rho, var, beta_latent, beta_unclustered, snr,
        sigma_eps_sq)

    # Generate covariance matrix (latent features are mixed in matrix, so each
    # cluster will be of size cluster_size + 1)
    Sigma <- makeCovarianceMatrix(p=p + n_clusters, nblocks=n_clusters,
        block_size=cluster_size + 1, rho=rho, var=var)

    # Generate coefficients
    # Note that beta has length p + sig_clusters
    coefs <- makeCoefficients(p=p + n_clusters, k_unblocked=k_unclustered,
        beta_low=beta_unclustered, beta_high=beta_latent, nblocks=n_clusters,
        sig_blocks=sig_clusters, block_size=cluster_size + 1)

    # Generate mu, X, z, sd, y
    gen_mu_x_z_sd_res <- genMuXZSd(n=n, p=p, beta=coefs$beta, Sigma=Sigma,
        blocked_dgp_vars=coefs$blocked_dgp_vars, latent_vars=coefs$latent_vars, 
        block_size=cluster_size, n_blocks=n_clusters, snr=snr,
        sigma_eps_sq=sigma_eps_sq)

    mu <- gen_mu_x_z_sd_res$mu
    sd <- gen_mu_x_z_sd_res$sd

    y <- mu + sd * stats::rnorm(n)

    return(list(X=gen_mu_x_z_sd_res$X, y=y, Z=gen_mu_x_z_sd_res$z, mu=mu))
}
```

`checkGenClusteredDataInputs()` ## NEED TO START WITH ROXYGEN FORMATTING


```r
#' TODO(gregfaletto) fix this description!!!
#' Check inputs to genClusteredData
#'
#' Generate a data set including latent features Z, observed features X (which
#' may include noisy or noiseless observations of the latent features in Z),
#' an observed response y which is a linear model of features from Z and X as
#' well as independent mean zero noise, and mu (the responses from y without
#' the added noise). Data is generated in the same way as in the simulations
#' from Faletto and Bien (2022).
#' @param p Integer or numeric; the number of features to generate. The
#' generated X will have p columns.
#' @param k_unclustered Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_unclustered.
#' @param cluster_size Integer or numeric; for each of the n_clusters latent
#' variables, X will contain cluster_size noisy proxies that are correlated with
#' the latent variable.
#' @param n_clusters Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param sig_clusters Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). Must be less than or equal to
#' n_clusters. Default is 1.
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. Can't equal 0. Default is 0.9.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). Can't equal 0. Default is 1.
#' @param beta_latent Integer or numeric; the coefficient used for all
#' sig_clusters latent variables that have nonzero coefficients in the true
#' model for y. Can't equal 0. Default is 1.5.
#' @param beta_unclustered Integer or numeric; the maximum coefficient in the
#' model for y among the k_unclustered features in X not generated from the
#' latent variables. The coefficients of the features will be
#' beta_unclustered/sqrt(1:k_unclustered). Can't equal 0. Default is 1.
#' @param snr Integer or numeric; the signal-to-noise ratio of the response
#' y. If sigma_eps_sq is not specified, the variance of the noise in y will be
#' calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
#' snr and sigma_eps_sq must be specified. Default is NA.
#' @param sigma_eps_sq Integer or numeric; the variance on the noise added
#' to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.
#' @return A list of the following elements. \item{X}{An n x p numeric matrix of
#' n observations from a p-dimensional multivariate normal distribution
#' generated using the specified parameters. The first n_clusters times
#' cluster_size features will be the clusters of features correlated with the
#' n_clusters latent variables. The next k_unclustered features will be the
#' "weak signal" features, and the remaining p - n_clusters*cluster_size -
#' k_unclustered features will be the unclustered noise features.} \item{y}{A
#' length n numeric vector; the response generated from X, the latent features
#' from Z, and the coefficient vector, along with additive noise.} \item{Z}{The
#' latent features; either a numeric vector (if n_clusters > 1) or a numeric
#' matrix (if n_clusters > 1). Note that (X, Z) is multivariate Gaussian.}
#' item{mu}{A length `n` numeric vector; the expected response given X, Z, and
#' the true coefficient vector (equal to y minus the added noise).}
#' @author Gregory Faletto, Jacob Bien
checkGenClusteredDataInputs <- function(p, k_unclustered, cluster_size,
    n_clusters, sig_clusters, rho, var, beta_latent, beta_unclustered, snr,
    sigma_eps_sq){

    stopifnot(is.numeric(sig_clusters) | is.integer(sig_clusters))
    stopifnot(sig_clusters <= n_clusters)
    stopifnot(sig_clusters >= 0)
    stopifnot(sig_clusters == round(sig_clusters))
    
    stopifnot(is.numeric(n_clusters) | is.integer(n_clusters))
    stopifnot(n_clusters == round(n_clusters))
    # TODO(gregfaletto): is it easy to remove the requirement that n_clusters is
    # at least 1 (so that it's possible to generate data with no latent 
    # features)? If so, should only check that cluster_size >= 1 if n_clusters
    # >= 1, and in makeCovarianceMatrix function only need block_size >= 1
    # rather than 2.
    stopifnot(n_clusters >= 1)

    stopifnot(cluster_size >= 1)

    stopifnot(abs(rho) <= abs(var))
    stopifnot(rho != 0)
    stopifnot(var > 0)

    stopifnot(beta_latent != 0)
    stopifnot(beta_unclustered != 0)

    stopifnot(is.numeric(k_unclustered) | is.integer(k_unclustered))
    stopifnot(k_unclustered >= 0)
    stopifnot(k_unclustered == round(k_unclustered))
    
    stopifnot(p >= n_clusters*cluster_size + k_unclustered)

    # Same as make_sparse_blocked_linear_model_random, but ith coefficient
    # of weak signal features is beta_unclustered/sqrt(i) in order to have
    # a definitive ranking of weak signal features.
    if(is.na(snr) & is.na(sigma_eps_sq)){
        stop("Must specify one of snr or sigma_eps_sq")
    }
    
    if(!is.na(snr)){
        stopifnot(snr > 0)
    }
    if(!is.na(sigma_eps_sq)){
        stopifnot(sigma_eps_sq > 0)
    }
}
```

Tests for `checkGenClusteredDataInputs()`


```r
testthat::test_that("checkGenClusteredDataInputs works", {
  set.seed(7612)

  # Should get no error
  checkGenClusteredDataInputs(p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                           sig_clusters=2, rho=.8, var=1.1, beta_latent=1.5,
                           beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  
  checkGenClusteredDataInputs(p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                           sig_clusters=2, rho=.8, var=1.1, beta_latent=1.5,
                           beta_unclustered=-2, snr=1, sigma_eps_sq=NA)
  
  # sig_clusters
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters="2", rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(sig_clusters) | is.integer(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=4, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters <= n_clusters is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=-1, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=.6, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters == round(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  
  # n_clusters
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5,
                                                  n_clusters="3",
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(n_clusters) | is.integer(n_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3.2,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters == round(n_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=0,
                                                  sig_clusters=0, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters >= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=.3, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "cluster_size >= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=16, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "p >= n_clusters * cluster_size + k_unclustered is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "abs(rho) <= abs(var) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "rho != 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=-1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "var > 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=0,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_latent != 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=0, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_unclustered != 0 is not TRUE", fixed=TRUE)
  
  # k_unclustered
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered="2",
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(k_unclustered) | is.integer(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=-2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=.2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered == round(k_unclustered) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=NA),
                         "Must specify one of snr or sigma_eps_sq", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=-.2,
                                                  sigma_eps_sq=NA),
                         "snr > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=-.3),
                         "sigma_eps_sq > 0 is not TRUE", fixed=TRUE)
  
})
```

```
## Test passed 😀
```

`makeCovarianceMatrix()`


```r
#' Generate covariance matrix for simulated clustered data
#'
#' @param p Integer or numeric; the total number of features in the covariance
#' matrix to be created, including latent features, the associated noisy proxies
#' with each latent feature, and other (weak signal and noise) features.
#' @param n_blocks Integer or numeric; the number of latent variables in the
#' data, each of which is associated with an observed cluster in X. Must be at
#' least 1.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, the covariance matrix will include the original latent feature
#' plus block_size - 1 noisy proxies that are correlated with the latent
#' variable.
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. rho cannot equal 0.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). var cannot equal 0.
#' @return A `p` x `p` numeric matrix representing the covariance matrix for
#' the latent features, the associated proxies, and the remaining features. All
#' features not in a block will be independent from each other and the blocks
#' and have variance var.
#' @author Gregory Faletto, Jacob Bien
makeCovarianceMatrix <- function(p, nblocks, block_size, rho, var) {
    # Check inputs

    stopifnot(nblocks >= 1)
    stopifnot(rho != 0)
    stopifnot(var != 0)
    stopifnot(abs(rho) <= abs(var))
    stopifnot(block_size >= 2)
    stopifnot(p >= nblocks*block_size)

    # start with p x p identity matrix
    Sigma <- var*diag(p)

    # create matrix with nblocks rows, each containing a vector of
    # indices of highly correlated features
    block_feats <- matrix(seq(nblocks*block_size), nrow=nblocks, byrow=TRUE)

    stopifnot(length(unique(block_feats)) == length(block_feats))

    # add covariances of highly correlated features to sigma
    for(i in 1:nblocks){
        for(j in 1:(block_size - 1)){
            for(k in (j+1):block_size){
                feat_1 <- block_feats[i, j]
                feat_2 <- block_feats[i, k]
                Sigma[feat_1, feat_2] <- rho
                Sigma[feat_2, feat_1] <- rho
            }
        }
    }
    stopifnot(is.numeric(Sigma))
    stopifnot(is.matrix(Sigma))
    stopifnot(nrow(Sigma) == p & ncol(Sigma) == p)
    stopifnot(all(Sigma == t(Sigma)))

    return(Sigma)
}
```

Tests for `makeCovarianceMatrix()`


```r
testthat::test_that("makeCovarianceMatrix works", {
  set.seed(7612)

  ret <- makeCovarianceMatrix(p=8, nblocks=2, block_size=3, rho=0.8, var=2)
  
  testthat::expect_true(is.numeric(ret))
  testthat::expect_true(is.matrix(ret))
  testthat::expect_equal(nrow(ret), 8)
  testthat::expect_equal(ncol(ret), 8)
  testthat::expect_true(all(ret == t(ret)))
  # Test on-diagonal and off-diagonal entries to see if they are as expected
  # (only need to check top half of matrix; we already confirmed the matrix is
  # symmetric)
  testthat::expect_true(all(diag(ret) == 2))
  testthat::expect_equal(ret[1, 2], 0.8)
  testthat::expect_equal(ret[1, 3], 0.8)
  testthat::expect_equal(ret[2, 3], 0.8)
  testthat::expect_equal(ret[4, 5], 0.8)
  testthat::expect_equal(ret[4, 6], 0.8)
  testthat::expect_equal(ret[5, 6], 0.8)
  testthat::expect_true(all(ret[1:3, 4:8] == 0))
  testthat::expect_true(all(ret[4:6, c(1:3, 7:8)] == 0))
  testthat::expect_true(all(ret[7, c(1:6, 8)] == 0))
  testthat::expect_true(all(ret[8, 1:7] == 0))
  
  # Bad inputs
  testthat::expect_error(makeCovarianceMatrix(p=8, nblocks=2, block_size=3,
                                              rho=0.8, var=.2),
                         "abs(rho) <= abs(var) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCovarianceMatrix(p=8, nblocks=-2, block_size=3,
                                              rho=0.8, var=2),
                         "nblocks >= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCovarianceMatrix(p=8, nblocks=2, block_size=3,
                                              rho=0, var=2),
                         "rho != 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCovarianceMatrix(p=8, nblocks=2, block_size=3,
                                              rho=0.9, var=0),
                         "var != 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCovarianceMatrix(p=5, nblocks=2, block_size=3,
                                              rho=0.8, var=2),
                         "p >= nblocks * block_size is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 😀
```

`makeCoefficients()`:


```r
#' Generated coefficients for y in latent variable model
#'
#' @param p Integer or numeric; the number of features that will be observed in
#' x plus the number of latent variables (each corresponding to a cluster).
#' @param k_unblocked Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_low.
#' @param beta_low Integer or numeric; the maximum coefficient in the
#' model for y among the k_unblocked features in X not generated from the
#' latent variables. The coefficients of the features will be
#' beta_low/sqrt(1:k_unblocked).
#' @param beta_high Integer or numeric; the coefficient used for all
#' sig_blocks latent variables that have nonzero coefficients in the true
#' model for y.
#' @param nblocks Integer or numeric; the number of latent variables that were
#' generated, each of which will be associated with an observed cluster in X.
#' @param sig_blocks Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). In particular, the first sig_blocks
#' latent variables will have coefficient beta_latent, and the remaining nblocks
#' - sig_blocks features will have coefficient 0. Must be less than or equal to
#' n_clusters.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, the covariance matrix will include the original latent feature
#' plus block_size - 1 noisy proxies that are correlated with the latent
#' variable.
#' @return A named list with the following elements: \item{beta}{A vector of
#' length `p` containing the coefficients for the true model for y. All entries
#' will equal 0 except for the sig_blocks latent variables that will have
#' coefficient beta_high and the k_unblocked independent features with
#' coefficient determined by beta_low.} \item{blocked_dgp_vars}{An integer
#' vector of length sig_blocks containing the indices of the features
#' corresponding to the latent features that will have nonzero coefficient
#' beta_high in the true model for y.} \item{sig_unblocked_vars}{An integer
#' vector of length k_unblocked containing the indices of the observed features
#' that are independent of the blocked features and have coefficient beta_low in
#' the true model for y. If k_unblocked = 0, this will just be NA.}
#' \item{insig_blocked_vars}{An integer vector containing the indices of the
#' features corresponding to the latent features that will have coefficient 0 in
#' the true model for y. If nblocks=0, this will just be NA.}
#' \item{latent_vars}{An integer vector of length nblocks containing the indices
#' of all of the latent features.}
#' @author Gregory Faletto, Jacob Bien
#' @references
<<faletto2022>>
makeCoefficients <- function(p, k_unblocked, beta_low, beta_high, nblocks,
    sig_blocks, block_size){

    # Check inputs
    stopifnot(k_unblocked >= 0)
    stopifnot(sig_blocks <= nblocks)
    stopifnot(p >= nblocks*block_size + k_unblocked)
    stopifnot(sig_blocks >= 0)

    # Initialize beta
    beta <- numeric(p)

    # identify indices of first coefficient in each significant block (these
    # features will have coefficient beta_high)
    latent_vars <- NA
    if(nblocks >= 1){
        latent_vars <- as.integer(((0:(nblocks - 1))*block_size + 1))

        stopifnot(all(latent_vars) %in% 1:p)
        stopifnot(all(latent_vars) %in% 1:(block_size*nblocks))
        stopifnot(length(unique(latent_vars)) == nblocks)
        stopifnot(length(latent_vars) == nblocks)
    }

    blocked_dgp_vars <- latent_vars[1:sig_blocks]

    stopifnot(sig_blocks == length(blocked_dgp_vars))
    
    beta[blocked_dgp_vars] <- beta_high

    # identify remaining coefficients in blocks (which ought to be set to 0)
    insig_blocked_vars <- NA

    if(nblocks >= 1){
        insig_blocked_vars <- setdiff(1:(block_size*nblocks), blocked_dgp_vars)
        stopifnot(all(beta[insig_blocked_vars] == 0))
    }
    # find significant unblocked variables (if applicable) and fill in
    # coefficients
    sig_unblocked_vars <- NA

    if(k_unblocked > 0){
        # Range of weak signal coefficients
        beta_lows <- beta_low/sqrt(1:k_unblocked)
        sig_unblocked_vars <- (nblocks*block_size + 1):
            (nblocks*block_size + k_unblocked)
        sig_unblocked_vars <- as.integer(sig_unblocked_vars)

        stopifnot(length(sig_unblocked_vars) == k_unblocked)
        stopifnot(length(unique(sig_unblocked_vars)) == k_unblocked)
        stopifnot(all(sig_unblocked_vars) %in% 1:p)

        beta[sig_unblocked_vars] <- beta_lows
    }

    stopifnot(length(intersect(blocked_dgp_vars, sig_unblocked_vars)) == 0)
    stopifnot(length(intersect(sig_unblocked_vars, insig_blocked_vars)) == 0)
    stopifnot(length(intersect(blocked_dgp_vars, insig_blocked_vars)) == 0)

    stopifnot(length(insig_blocked_vars) + length(blocked_dgp_vars) ==
        nblocks*block_size)

    stopifnot(sig_blocks + length(insig_blocked_vars) + k_unblocked <= p)

    stopifnot(sum(beta != 0) == sig_blocks + k_unblocked)
    stopifnot(is.numeric(beta) | is.integer(beta))

    return(list(beta=beta, blocked_dgp_vars=blocked_dgp_vars,
        sig_unblocked_vars=sig_unblocked_vars,
        insig_blocked_vars=insig_blocked_vars, latent_vars=latent_vars))
}
```

Tests for `makeCoefficients()`


```r
testthat::test_that("makeCoefficients works", {
  set.seed(5722)

  ret <- makeCoefficients(p=9+2, k_unblocked=2, beta_low=.9, beta_high=-2,
                          nblocks=2, sig_blocks=1, block_size=3)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("beta", "blocked_dgp_vars",
                                           "sig_unblocked_vars",
                                           "insig_blocked_vars", "latent_vars"))
  
  testthat::expect_true(is.numeric(ret$beta))
  testthat::expect_equal(length(ret$beta), 9 + 2)
  testthat::expect_equal(sum(ret$beta == 0), 9 + 2 - 3)
  # Checking structure
  testthat::expect_equal(ret$beta[1], -2)
  testthat::expect_true(all(ret$beta[2:6] == 0))
  testthat::expect_true(all(ret$beta[7:8] == c(.9, .9/sqrt(2))))
  testthat::expect_true(all(ret$beta[9:11] == 0))
  
  testthat::expect_true(is.integer(ret$blocked_dgp_vars) | is.numeric(ret$blocked_dgp_vars))
  testthat::expect_equal(ret$blocked_dgp_vars, 1)
  
  testthat::expect_identical(ret$sig_unblocked_vars, 7:8)

  testthat::expect_identical(ret$insig_blocked_vars, 2:6)
  
  testthat::expect_identical(ret$latent_vars, c(1L, 4L))
  
  # Bad inputs
  testthat::expect_error(makeCoefficients(p=9+2, k_unblocked=-2, beta_low=.9,
                                          beta_high=-2, nblocks=2, sig_blocks=1,
                                          block_size=3),
                         "k_unblocked >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCoefficients(p=9+2, k_unblocked=2, beta_low=.9,
                                          beta_high=-2, nblocks=2, sig_blocks=3,
                                          block_size=3),
                         "sig_blocks <= nblocks is not TRUE", fixed=TRUE)
  
  testthat::expect_error(makeCoefficients(p=7, k_unblocked=2, beta_low=.9,
                                          beta_high=-2, nblocks=2, sig_blocks=1,
                                          block_size=3),
                         "p >= nblocks * block_size + k_unblocked is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(makeCoefficients(p=9+2, k_unblocked=2, beta_low=.9,
                                          beta_high=-2, nblocks=2,
                                          sig_blocks=-1, block_size=3),
                         "sig_blocks >= 0 is not TRUE", fixed=TRUE)

})
```

```
## Test passed 😸
```

`genMuXZSd()`


```r
#' Generate observed and latent variables along with conditional mean
#'
#' @param n Integer or numeric; the number of observations to generate. (The
#' generated X and Z will have n rows, and the generated y and mu will have
#' length n.)
#' @param p Integer or numeric; the number of observed features (the generated X
#' will have p columns).
#' @param beta A numeric or integer vector of length `p` + sig_blocks containing
#' the coefficients for the true model for y.
#' @param Sigma A (`p` + n_blocks) x (`p` + n_blocks) numeric matrix
#' representing the covariance matrix for the latent features, the associated
#' proxies, and the remaining features.
#' @param blocked_dgp_vars An integer vector of length sig_blocks containing the
#' indices of the features corresponding to the latent features that have
#' nonzero coefficient beta_high in the true model for y.
#' @param latent_vars An integer vector of length n_blocks containing the
#' indices of all of the latent features.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, X will contain block_size noisy proxies that are correlated with
#' the latent variable.
#' @param n_blocks Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param snr Integer or numeric; the signal-to-noise ratio of the response
#' y. If sigma_eps_sq is not specified, the variance of the noise in y will be
#' calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
#' snr and sigma_eps_sq must be specified. Default is NA.
#' @param sigma_eps_sq Integer or numeric; the variance on the noise added
#' to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.
#' @return A named list with the following elements: \item{X}{An `n` x `p`
#' numeric matrix containing the observed proxies for the latent variables as
#' well as the observed unblocked (iid) variables.} \item{mu}{A length `n`
#' numeric vector; the expected response given X, Z, and the true
#' coefficient vector (equal to y minus the added noise).} \item{z}{An `n` x
#' n_blocks numeric matrix containing the n_blocks latent variables. Note that
#' (X, z) is multivariate Gaussian.} \item{sd}{Numeric; the standard deviation
#' of the noise added to mu to get y (calculated either from snr or
#' sigma_eps_sq).}
#' @author Gregory Faletto, Jacob Bien
genMuXZSd <- function(n, p, beta, Sigma, blocked_dgp_vars,
    latent_vars, block_size, n_blocks=1, snr=NA, sigma_eps_sq=NA){
    # Check inputs

    stopifnot(length(blocked_dgp_vars) <= n_blocks)
    stopifnot(nrow(Sigma) == p + n_blocks)
    stopifnot(ncol(Sigma) == p + n_blocks)
    
    if(any(!is.na(sigma_eps_sq))){
        stopifnot(is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq))
        stopifnot(length(sigma_eps_sq) == 1)
        stopifnot(sigma_eps_sq >= 0)
    } else{
        if(any(is.na(snr))){
            stop("Must provide one of snr or sigma_eps_sq")
        }
        stopifnot(is.numeric(snr) | is.integer(snr))
        stopifnot(length(snr) == 1)
        stopifnot(snr > 0)
    }

    stopifnot(length(beta) == p + n_blocks)
    stopifnot(all(beta[blocked_dgp_vars] != 0))

    stopifnot(length(latent_vars) == n_blocks)

    x <- MASS::mvrnorm(n=n, mu=rep(0, p + n_blocks), Sigma=Sigma)

    stopifnot(length(beta) == ncol(x))

    mu <- as.numeric(x %*% beta)

    # Remove true blocked signal feature from each block from x now that I've
    # generated mu
    if(n_blocks > 0){
        z <- matrix(as.numeric(NA), nrow=n, ncol=n_blocks)
        stopifnot(length(latent_vars) > 0)
    } else{
        z <- NA
        stopifnot(length(latent_vars) == 0)
    }
    
    if(length(latent_vars) > 0){
        if(length(latent_vars) > 0){
        z[, 1:n_blocks] <- x[, latent_vars]
    }
    }
    
    x <- x[, setdiff(1:(p + n_blocks), latent_vars)]

    # If SNR is null, use sigma_eps_sq
    if(!is.na(sigma_eps_sq)){
        sd <- sqrt(sigma_eps_sq)
    }else{
        sd <- sqrt(sum(mu^2) / (n * snr)) # taking snr = ||mu||^2 /(n * sigma^2)
    }

    # Check output

    stopifnot(length(mu) == n)

    stopifnot(nrow(x) == n)
    stopifnot(ncol(x) == p)

    if(any(!is.na(z))){
        stopifnot(nrow(z) == n)
        stopifnot(ncol(z) == n_blocks)
    }

    stopifnot(is.numeric(sd) | is.integer(sd))
    stopifnot(length(sd) == 1)
    stopifnot(!is.na(sd))
    stopifnot(sd >= 0)

    return(list(X=x, mu=mu, z=z, sd=sd))
}
```

Tests for `genMuXZSd()`


```r
testthat::test_that("genMuXZSd works", {
  set.seed(61232)
  
  Sigma <- makeCovarianceMatrix(p=9+2, nblocks=2, block_size=3+1, rho=0.2499,
                                var=.25)

  coefs <- makeCoefficients(p=9+2, k_unblocked=2, beta_low=.9, beta_high=-2,
                            nblocks=2, sig_blocks=1, block_size=3+1)

  ret <- genMuXZSd(n=25, p=9, beta=coefs$beta, Sigma=Sigma,
                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                   latent_vars=coefs$latent_vars, n_blocks=2, block_size=3,
                   snr=NA, sigma_eps_sq=1.2)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "mu", "z", "sd"))
  
  testthat::expect_true(is.numeric(ret$X))
  testthat::expect_true(is.matrix(ret$X))
  testthat::expect_equal(nrow(ret$X), 25)
  testthat::expect_equal(ncol(ret$X), 9)
  # X is Gaussian with mean 0 and variance 1/4; expect all observations to lie
  # within 5 standard deviations of mean
  testthat::expect_true(all(abs(ret$X) < 5*sqrt(0.25)))
  
  # Test that clusters are correlated--within-cluster correlation should be
  # high, correlation with other features should be low
  testthat::expect_true(min(cor(ret$X[, 1:3])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 1:3], ret$X[, 4:9]))) < .6)

  testthat::expect_true(min(cor(ret$X[, 4:6])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 4:6],
                                    ret$X[, c(1:3, 7:9)]))) < .6)

  cor_indeps <- cor(ret$X[, 7:9])
  testthat::expect_true(max(abs(cor_indeps[lower.tri(cor_indeps)])) < .6)


  testthat::expect_true(is.numeric(ret$mu))
  testthat::expect_equal(length(ret$mu), 25)

  testthat::expect_true(is.numeric(ret$z))
  testthat::expect_true(is.matrix(ret$z))
  testthat::expect_equal(nrow(ret$z), 25)
  testthat::expect_equal(ncol(ret$z), 2)

  testthat::expect_true(is.numeric(ret$sd))
  testthat::expect_equal(ret$sd, sqrt(1.2))
  
  # Specify SNR instead of sd
  
  ret <- genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                   latent_vars=coefs$latent_vars, n_blocks=2, block_size=3,
                   snr=1, sigma_eps_sq=NA)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "mu", "z", "sd"))
  
  testthat::expect_true(is.numeric(ret$X))
  testthat::expect_true(is.matrix(ret$X))
  testthat::expect_equal(nrow(ret$X), 5)
  testthat::expect_equal(ncol(ret$X), 9)

  testthat::expect_true(is.numeric(ret$mu))
  testthat::expect_equal(length(ret$mu), 5)

  testthat::expect_true(is.numeric(ret$z))
  testthat::expect_true(is.matrix(ret$z))
  testthat::expect_equal(nrow(ret$z), 5)
  testthat::expect_equal(ncol(ret$z), 2)

  testthat::expect_true(is.numeric(ret$sd))
  testthat::expect_true(ret$sd > 0)

  # Try a single latent variable (z should be a one-column matrix)

  Sigma <- makeCovarianceMatrix(p=9+1, nblocks=1, block_size=3, rho=0.8, var=2)

  coefs <- makeCoefficients(p=9+1, k_unblocked=2, beta_low=.9, beta_high=-2,
                            nblocks=1, sig_blocks=1, block_size=3)

  ret <- genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                   latent_vars=coefs$latent_vars, n_blocks=1, block_size=3,
                   snr=NA, sigma_eps_sq=1.2)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "mu", "z", "sd"))

  testthat::expect_true(is.numeric(ret$z))
  testthat::expect_true(is.matrix(ret$z))
  testthat::expect_equal(nrow(ret$z), 5)
  testthat::expect_equal(ncol(ret$z), 1)
  
  # Bad inputs
  Sigma <- makeCovarianceMatrix(p=9+2, nblocks=2, block_size=3, rho=0.8, var=2)

  coefs <- makeCoefficients(p=9+2, k_unblocked=2, beta_low=.9, beta_high=-2,
                            nblocks=2, sig_blocks=1, block_size=3)

  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=0,
                                   block_size=3, snr=NA, sigma_eps_sq=1.2),
                         "length(blocked_dgp_vars) <= n_blocks is not TRUE", fixed=TRUE)

  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=1,
                                   block_size=3, snr=NA, sigma_eps_sq=1.2),
                         "nrow(Sigma) == p + n_blocks is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=NA, sigma_eps_sq="1.2"),
                         "is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=NA, sigma_eps_sq=1:2),
                         "length(sigma_eps_sq) == 1 is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=NA, sigma_eps_sq=-1),
                         "sigma_eps_sq >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=NA, sigma_eps_sq=NA),
                         "Must provide one of snr or sigma_eps_sq", fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr="1", sigma_eps_sq=NA),
                         "is.numeric(snr) | is.integer(snr) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=1:2, sigma_eps_sq=NA),
                         "length(snr) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=-1, sigma_eps_sq=NA),
                         "snr > 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=1:9, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=1, sigma_eps_sq=NA),
                         "length(beta) == p + n_blocks is not TRUE", fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=rep(0, 9 + 2), Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=coefs$latent_vars, n_blocks=2,
                                   block_size=3, snr=1, sigma_eps_sq=NA),
                         "all(beta[blocked_dgp_vars] != 0) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genMuXZSd(n=5, p=9, beta=coefs$beta, Sigma=Sigma,
                                   blocked_dgp_vars=coefs$blocked_dgp_vars,
                                   latent_vars=1:3, n_blocks=2,
                                   block_size=3, snr=1, sigma_eps_sq=NA),
                         "length(latent_vars) == n_blocks is not TRUE",
                         fixed=TRUE)

})
```

```
## Test passed 🥇
```

Finally, tests for `genClusteredData()`


```r
testthat::test_that("genClusteredData works", {
  set.seed(23478)

  ret <- genClusteredData(n=25, p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                sig_clusters=2, rho=3.99, var=4, beta_latent=1.5,
                beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  
  testthat::expect_true(is.numeric(ret$X))
  testthat::expect_true(is.matrix(ret$X))
  testthat::expect_equal(ncol(ret$X), 19)
  testthat::expect_equal(nrow(ret$X), 25)
  # X is Gaussian with mean 0 and variance 4; expect all observations to lie
  # within 5 standard deviations of mean
  testthat::expect_true(all(abs(ret$X) < 5*2))
  # Test that clusters are correlated--within-cluster correlation should be
  # high, correlation with other features should be low
  testthat::expect_true(min(cor(ret$X[, 1:5])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 1:5], ret$X[, 6:19]))) < .6)
  
  testthat::expect_true(min(cor(ret$X[, 6:10])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 6:10],
                                    ret$X[, c(1:5, 11:19)]))) < .6)
  
  testthat::expect_true(min(cor(ret$X[, 11:15])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 11:15],
                                    ret$X[, c(1:10, 16:19)]))) < .6)

  cor_indeps <- cor(ret$X[, 16:19])
  testthat::expect_true(max(abs(cor_indeps[lower.tri(cor_indeps)])) < .6)

  testthat::expect_true(is.numeric(ret$y))
  testthat::expect_equal(length(ret$y), 25)

  testthat::expect_true(is.numeric(ret$Z))
  testthat::expect_true(is.matrix(ret$Z))
  testthat::expect_equal(nrow(ret$Z), 25)
  testthat::expect_equal(ncol(ret$Z), 3)

  testthat::expect_true(is.numeric(ret$mu))
  testthat::expect_equal(length(ret$mu), 25)
  # Because y is Gaussian with mean mu and standard deviation .5 conditional on
  # mu, expect all observations to lie within 5 sds of mu
  testthat::expect_true(all(abs(ret$y - ret$mu) < 5*.5))

  # Specify SNR instead of sigma_eps_sq
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                       sig_clusters=2, rho=.8, var=1.1, beta_latent=1.5,
                       beta_unclustered=-2, snr=1, sigma_eps_sq=NA)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  
  # If sigma_eps_sq is specified, snr should be ignored. (Set an SNR that
  # implies a very large noise variance to test this)
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                sig_clusters=2, rho=.8, var=4, beta_latent=1.5,
                beta_unclustered=-2, snr=.01, sigma_eps_sq=.25)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  
  # Because y is Gaussian with mean mu and standard deviation .5 conditional on
  # mu, expect all observations to lie within 5 sds of mu
  testthat::expect_true(all(abs(ret$y - ret$mu) < 5*.25))
  
  # Try a single latent variable (z should be a one-column matrix)
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5, n_clusters=1,
                sig_clusters=1, rho=.8, var=4, beta_latent=1.5,
                beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))

  testthat::expect_true(is.numeric(ret$Z))
  testthat::expect_true(is.matrix(ret$Z))
  testthat::expect_equal(nrow(ret$Z), 5)
  testthat::expect_equal(ncol(ret$Z), 1)
  
  # Bad inputs
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                       cluster_size=5, n_clusters=3,
                                       sig_clusters="2", rho=.8, var=1.1,
                                       beta_latent=1.5, beta_unclustered=-2,
                                       snr=1, sigma_eps_sq=NA),
                         "is.numeric(sig_clusters) | is.integer(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=4, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters <= n_clusters is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=-1, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=.6, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters == round(sig_clusters) is not TRUE",
                         fixed=TRUE)


  # n_clusters
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5,
                                                  n_clusters="3",
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(n_clusters) | is.integer(n_clusters) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3.2,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters == round(n_clusters) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=0,
                                                  sig_clusters=0, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=.3, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "cluster_size >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(p=16, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "p >= n_clusters * cluster_size + k_unclustered is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "abs(rho) <= abs(var) is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "rho != 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=-1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "var > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=0,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_latent != 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=0, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_unclustered != 0 is not TRUE", fixed=TRUE)

  # k_unclustered
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered="2",
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(k_unclustered) | is.integer(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=-2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=.2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered == round(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=NA),
                         "Must specify one of snr or sigma_eps_sq", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=-.2,
                                                  sigma_eps_sq=NA),
                         "snr > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  var=1.1, beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=-.3),
                         "sigma_eps_sq > 0 is not TRUE", fixed=TRUE)

})
```

```
## Test passed 😀
```

`getLassoLambda()`:


```r
#' Get lambda value for lasso
#'
#' Chooses a lambda value for the lasso used on a subsample of size n/2 (as in
#' cluster stability selection) by cross-validation.
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors that will be used by cluster stability
#' selection.
#' @param y The response; an n-dimensional numeric or integer vector. (Unlike
#' in the more general css setup, this response must be real-valued since
#' lambda will be determined using the lasso with cross-validation.)
#' @param lambda_choice Character; either "min" or "1se". If "min", chooses
#' the lambda that minimizes the cross-validated error; if "1se", chooses the
#' largest lambda within one standard error of the minimum error lambda
#' (resulting in a smaller selected set, which may be desirable because the
#' model size corresponding to the minimum error lambda tends to be larger
#' than optimal. See, for example, Bühlmann and Meinshausen 2006, Prop. 1 and
#' Bühlmann and van de Geer 2011, Section 2.5.1.). Default is "1se".
#' @param nfolds Numeric or integer; the number of folds for cross-validation.
#' Must be at least 4 (as specified by cv.glmnet). Default is 10.
#' @return A numeric; the selected value of lambda.
#' @author Gregory Faletto, Jacob Bien
#' @references Bühlmann, P., & Meinshausen, N. (2006). High-Dimensional Graphs
#' and Variable Selection With the Lasso. \emph{The Annals of Statistics},
#' 34(3), 1436–1462. \url{https://doi.org/10.1214/009053606000000281}.
#' \cr Peter Bühlmann and Sara van de Geer. Statistics for High-Dimensional
#' Data. \emph{Springer Series in Statistics}. Springer, Heidelberg, 2011. ISBN
#' 978-3-642-20191-2. \url{http://dx.doi.org/10.1007/978-3-642-20192-9}. \cr
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
#' Paths for Generalized Linear Models via Coordinate Descent. \emph{Journal of
#' Statistical Software}, 33(1), 1-22. URL \url{https://www.jstatsoft.org/v33/i01/}.
#' @export
getLassoLambda <- function(X, y, lambda_choice="1se", nfolds=10){
    stopifnot(is.character(lambda_choice))
    stopifnot(length(lambda_choice) == 1)
    stopifnot(!is.na(lambda_choice))
    stopifnot(lambda_choice %in% c("min", "1se"))

    stopifnot(is.matrix(X))
    stopifnot(is.numeric(X) | is.integer(X))
    n <- nrow(X)

    stopifnot(is.numeric(nfolds) | is.integer(nfolds))
    stopifnot(length(nfolds) == 1)
    stopifnot(nfolds == round(nfolds))
    stopifnot(nfolds > 3)

    # Since we are using the lasso, we require y to be a real-valued response
    # (unlike for the general cluster stability selection procedure, where y
    # can have a more general format as long as a suitable feature selection
    # function is provided by the user)
    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(n == length(y))

    # Sample size to use: inflate n/2 by a factor of nfolds/(nfolds - 1),
    # so that each individual lasso fit is of size floor(n/2)

    n_sample <- min(round(n/2*nfolds/(nfolds - 1)), n)
    nfolds <- min(nfolds, n_sample)

    inds_size <- sample(1:n, n_sample)
    size_results <- glmnet::cv.glmnet(x=X[inds_size, ], y=y[inds_size],
        family="gaussian", nfolds=nfolds)

    lambda_ret <- size_results[[paste("lambda.", lambda_choice, sep="")]]

    # Check output
    stopifnot(length(lambda_ret) == 1)
    stopifnot(is.numeric(lambda_ret) | is.integer(lambda_ret))
    stopifnot(lambda_ret >= 0)

    return(lambda_ret)
}
```

Tests for `getLassoLambda()`:


```r
testthat::test_that("getLassoLambda works", {
  set.seed(7252)
  
  x <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  y <- stats::rnorm(10)

  ret <- getLassoLambda(X=x, y=y, lambda_choice="1se", nfolds=4)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_true(ret >= 0)
  
  ret <- getLassoLambda(X=x, y=y, lambda_choice="min", nfolds=5)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_true(ret >= 0)
  
  # Bad inputs
  testthat::expect_error(getLassoLambda(X="x", y=y), "is.matrix(X) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x[1:9, ], y=y),
                         "n == length(y) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=TRUE),
                         "is.numeric(y) | is.integer(y) is not TRUE",
                         fixed=TRUE)

  # Error has quotation marks in it
  testthat::expect_error(getLassoLambda(X=x, y=y, lambda_choice="mni"))

  testthat::expect_error(getLassoLambda(X=x, y=y,
                                        lambda_choice=c("min", "1se")),
                         "length(lambda_choice) == 1 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getLassoLambda(X=x, y=y, lambda_choice=1),
                         "is.character(lambda_choice) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds="5"),
                         "is.numeric(nfolds) | is.integer(nfolds) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds=1:4),
                         "length(nfolds) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds=3.2),
                         "nfolds == round(nfolds) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds=3),
                         "nfolds > 3 is not TRUE", fixed=TRUE)

})
```

```
## ── Warning ('<text>:7'): getLassoLambda works ──────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getLassoLambda(X = x, y = y, lambda_choice = "1se", nfolds = 4)
##  2. glmnet::cv.glmnet(...)
##  3. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:14'): getLassoLambda works ─────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getLassoLambda(X = x, y = y, lambda_choice = "min", nfolds = 5)
##  2. glmnet::cv.glmnet(...)
##  3. glmnet:::cv.glmnet.raw(...)
```

`getModelSize()`:


```r
#' Automated estimation of model size
#'
#' This function is uses the lasso with cross-validation to estimate the
#' model size. Before using the lasso, in each cluster all features will be
#' dropped from X except the one feature with the highest marginal correlation
#' with y, as in the protolasso (Reid and Tibshirani 2016).
#' 
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors.
#' @param y A length-n numeric vector containing the responses; `y[i]` is the
#' response corresponding to observation `X[i, ]`. (Note that for the css
#' function, y does not have to be a numeric response, but for this function,
#' the underlying selection procedure is the lasso, so y must be a real-valued
#' response.)
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of css.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @return An integer; the estimated size of the model. The minimum returned
#' value is 1, even if the lasso with cross-validation chose no features.
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
#' @export
getModelSize <- function(X, y, clusters){
    stopifnot(is.matrix(X) | is.data.frame(X))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))
    stopifnot(is.numeric(X) | is.integer(X))
    n <- nrow(X)

    # Since the model size will be determined by cross-validation, the provided
    # y must be real-valued (this should be checked internally in other
    # functions before getModelSize is called, but this check is here just in
    # case).
    if(!is.numeric(y) & !is.integer(y)){
        stop("getModelSize is trying to determine max_num_clusts using the lasso with cross-validation, but the y provided to getModelSize was not real-valued.")
    }
    stopifnot(length(y) == n)

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    ### Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

    clusters <- formatClusters(clusters, p=ncol(X),
        clust_names=clust_names)$clusters

    X_size <- X

    if(length(clusters) > 0){
        # Create modified design matrix by dropping all but one feature from
        # each cluster
        feats_to_drop <- integer()
        for(i in 1:length(clusters)){
            cluster_i <- clusters[[i]]
            if(length(cluster_i) > 1){
                feat_to_keep <- identifyPrototype(cluster_i, X, y)
                feats_to_drop <- c(feats_to_drop, setdiff(cluster_i,
                    feat_to_keep))
            }
        }
        if(length(feats_to_drop) > 0){
            X_size <- X_size[, -1*feats_to_drop]
        }
    }

    size_results <- glmnet::cv.glmnet(x=X_size, y=y, family="gaussian")
    coefs <- as.numeric(glmnet::coef.glmnet(size_results, s="lambda.1se"))

    # Number of nonzero coefficients (subtract one in order to ignore intercept)
    size <- length(coefs[coefs != 0]) - 1

    # Check output
    stopifnot(is.numeric(size) | is.integer(size))
    stopifnot(!is.na(size))
    stopifnot(length(size) == 1)
    stopifnot(size == round(size))

    return(as.integer(max(size, 1)))
}
```

Tests for `getModelSize()`:


```r
testthat::test_that("getModelSize works", {
  set.seed(1723)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=1, cluster_size=3,
                        n_clusters=2, sigma_eps_sq=10^(-6))
  
  x <- data$X
  y <- data$y
  
  good_clusters <- list(red_cluster=1L:3L, green_cluster=4L:6L)
  
  ret <- getModelSize(X=x, y=y, clusters=good_clusters)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_equal(ret, round(ret))
  testthat::expect_true(ret >= 1)
  # 11 features, but two clusters of size 3, so maximum size should
  # be 11 - 2 - 2 = 7
  testthat::expect_true(ret <= 7)
  
  ## Trying other inputs

  unnamed_clusters <- list(1L:3L, 5L:8L)
  
  ret <- getModelSize(X=x, y=y, clusters=unnamed_clusters)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_equal(ret, round(ret))
  testthat::expect_true(ret >= 1)
  # 11 features, but 3 in one cluster and 4 in another, so maximum size should
  # be 11 - 2 - 3 = 6
  testthat::expect_true(ret <= 6)
  
  # Single cluster
  ret <- getModelSize(X=x, y=y, clusters=2:5)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_equal(ret, round(ret))
  testthat::expect_true(ret >= 1)
  # 11 features, but 4 in one cluster, so maximum size should be 11 - 3 = 8
  testthat::expect_true(ret <= 8)
  
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1:3, 5:8)
  
  ret <- getModelSize(X=x, y=y, clusters=good_clusters)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_equal(ret, round(ret))
  testthat::expect_true(ret >= 1)
  # 11 features, but 3 in one cluster and 4 in another, so maximum size should
  # be 11 - 2 - 3 = 6
  testthat::expect_true(ret <= 6)
  
  ## Trying bad inputs
  
  testthat::expect_error(getModelSize(X="x", y=y, clusters=good_clusters),
                         "is.matrix(X) | is.data.frame(X) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(getModelSize(X=x[1:5, ], y=y, clusters=good_clusters),
                         "length(y) == n is not TRUE", fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=FALSE, clusters=good_clusters),
                         "getModelSize is trying to determine max_num_clusts using the lasso with cross-validation, but the y provided to getModelSize was not real-valued.",
                         fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=y, clusters=list(3:7, 7:10)),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.",
                         fixed=TRUE)
  
  testthat::expect_error(getModelSize(X=x, y=y, clusters=list(5:8, 5:8)),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=y, clusters=6:50),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=y,
                                      clusters=list(2:3, as.integer(NA))),
                         "!is.na(clusters) are not all TRUE",
                         fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=y, clusters=list(2:3, c(4, 4, 5))),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)

   testthat::expect_error(getModelSize(X=x, y=y, clusters=list(1:4, -1)),
                         "all(clusters[[i]] >= 1) is not TRUE",
                         fixed=TRUE)

   testthat::expect_error(getModelSize(X=x, y=y, clusters=list(1:4,
                                                               c(2.3, 1.2))),
                         "all(clusters[[i]] == round(clusters[[i]])) is not TRUE",
                         fixed=TRUE)

})
```

```
## ── Warning ('<text>:12'): getModelSize works ───────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getModelSize(X = x, y = y, clusters = good_clusters)
##  2. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  3. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:27'): getModelSize works ───────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getModelSize(X = x, y = y, clusters = unnamed_clusters)
##  2. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  3. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:39'): getModelSize works ───────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getModelSize(X = x, y = y, clusters = 2:5)
##  2. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  3. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:54'): getModelSize works ───────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) getModelSize(X = x, y = y, clusters = good_clusters)
##  2. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  3. glmnet:::cv.glmnet.raw(...)
```

`printCssDf()`:


```r
#' Prepares a data.frame summarazing cluster stability selection output to print
#'
#' Print a summary of the information from the css function.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param cutoff Numeric; the outputted data.frame will display only those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1. Default is 0 (in which case either all clusters are displayed, or
#' max_num_clusts are, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @return A data.frame; each row contains a cluster, arranged in decreasing
#' order of cluster selection proportion from top to bottom. The columns are
#' ClustName (the name of the cluster that was either provided to css or made by
#' css if no name was provided); ClustProtoName (the name of the selection
#' prototype from the cluster, which is the feature with the greatest individual
#' selection proportion among all the cluster members, with ties broken by
#' choosing the feature with the highest correlation with the response if the
#' response is real-valued; only returned if the features are named),
#' ClustProtoNum (the column number of the prototype in the X matrix provided to
#' css), and ClustSize (the size of the cluster).
#' @author Gregory Faletto, Jacob Bien
#' @export
printCssDf <- function(css_results, cutoff=0, min_num_clusts=1,
    max_num_clusts=NA){
    # Check inputs
    stopifnot(class(css_results) == "cssr")
    checkCutoff(cutoff)

    p <- ncol(css_results$feat_sel_mat)

    checkMinNumClusts(min_num_clusts, p, length(css_results$clusters))

    max_num_clusts <- checkMaxNumClusts(max_num_clusts, min_num_clusts, p,
        length(css_results$clusters))

    sel_clusts <- getCssSelections(css_results, cutoff=cutoff,
        min_num_clusts=min_num_clusts,
        max_num_clusts=max_num_clusts)$selected_clusts

    # sel_clusts is guaranteed to have length at least 1 by
    # getCssSelections 

    # Get prototypes (feature from each cluster with highest selection
    # proportion, breaking ties by using marginal correlations of features with
    # y from data provided to css if y is real-valued)
    prototypes <- getSelectionPrototypes(css_results, sel_clusts)
    
    # Cluster selection proportions
    if(length(sel_clusts) > 1){
        sel_clust_sel_props <- colMeans(css_results$clus_sel_mat[,
            names(sel_clusts)])
    } else{
        sel_clust_sel_props <- mean(css_results$clus_sel_mat[,
            names(sel_clusts)])
    }

    # Data.frame: name of cluster, cluster prototype, selection proportion,
    # cluster size

    if(!is.null(names(prototypes))){
        print_df <- data.frame(ClustName=names(sel_clusts),
            ClustProtoName=names(prototypes), ClustProtoNum=unname(prototypes),
            ClustSelProp=sel_clust_sel_props, ClustSize=lengths(sel_clusts))
    } else{
        print_df <- data.frame(ClustName=names(sel_clusts),
            ClustProtoNum=unname(prototypes), ClustSelProp=sel_clust_sel_props,
            ClustSize=lengths(sel_clusts))
    }

    print_df <- print_df[order(print_df$ClustSelProp, decreasing=TRUE), ]

    rownames(print_df) <- NULL
    
    stopifnot(is.data.frame(print_df))
    stopifnot(nrow(print_df) >= 1)

    return(print_df)
}
```

`getSelectionPrototypes()`


```r
#' Identify selection prototypes from selected clusters
#'
#' Takes in list of selected clusters and returns an integer vector of the
#' indices of the features that were most frequently selected from each cluster
#'
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param selected_clusts A list of integer vectors; each vector must contain
#' the indices of features in a cluster.
#' @return An integer vector (of length equal to the number of clusters) of the
#' indices of the feature prototypes (the features from each cluster that were
#' selected the most often individually by the base method in cluster stability
#' selection). In the case of a tie, the tie is broken by choosing the feature
#' most correlated with the response in the full data set provided to css.
#' @author Gregory Faletto, Jacob Bien
getSelectionPrototypes <- function(css_results, selected_clusts){
    
    # Check inputs
    stopifnot(class(css_results) == "cssr")

    stopifnot(is.list(selected_clusts))
    n_selected_clusts <- length(selected_clusts)
    stopifnot(n_selected_clusts >= 1)
    stopifnot(all(lengths(selected_clusts) >= 1))

    prototypes <- rep(as.integer(NA), n_selected_clusts)
    for(i in 1:n_selected_clusts){
        clust_i <- selected_clusts[[i]]
        sel_props_i <- colMeans(css_results$feat_sel_mat)[clust_i]
        proto_i <- clust_i[sel_props_i == max(sel_props_i)]
        stopifnot(length(proto_i) >= 1)
        if(length(proto_i) > 1){
            if(is.numeric(css_results$y) | is.integer(css_results$y)){
                # Break tie by looking at marginal correlations
                corrs_i <- stats::cor(css_results$X[, proto_i], css_results$y)[, 1]
                corrs_i <- abs(corrs_i)
                proto_i <- proto_i[corrs_i == max(corrs_i)]
            }
        }
        # If there is still a tie, break by choosing the first feature of those
        # remaining
        prototypes[i] <- proto_i[1]
        names(prototypes)[i] <- colnames(css_results$X)[proto_i]
    }

    # Check output

    stopifnot(is.integer(prototypes))
    stopifnot(all(!is.na(prototypes)))
    stopifnot(length(prototypes) == length(unique(prototypes)))

    return(prototypes)
}
```

Tests for `getSelectionPrototypes()`:


```r
testthat::test_that("getSelectionPrototypes works", {
  set.seed(67234)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=1, cluster_size=3,
                        n_clusters=2, sig_clusters=1, sigma_eps_sq=10^(-6))
  
  x <- data$X
  y <- data$y
  
  good_clusters <- list(red_cluster=1L:3L, green_cluster=4L:6L)
  
  css_results <- css(X=x, y=y, lambda=0.01, clusters=good_clusters)
  
  ret <- getSelectionPrototypes(css_results, selected_clusts=good_clusters)
  
  testthat::expect_true(is.integer(ret))
  testthat::expect_true(all(!is.na(ret)))
  testthat::expect_equal(length(ret), length(good_clusters))
  testthat::expect_equal(length(ret), length(unique(ret)))
  for(i in 1:length(ret)){
    testthat::expect_true(ret[i] %in% good_clusters[[i]])
    # Find the largest selection proportion of any feature in cluster i
    max_prop <- max(colMeans(css_results$feat_sel_mat[, good_clusters[[i]]]))
    # Find the selection proportion of the identified prototype
    proto_prop <- colMeans(css_results$feat_sel_mat)[ret[i]]
    testthat::expect_equal(max_prop, proto_prop)
  }
  
  # Try with only one selected cluster (still should be in a list)

  ret <- getSelectionPrototypes(css_results,
                                selected_clusts=list(red_cluster=1L:3L))

  testthat::expect_true(is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_true(ret %in% 1L:3L)
  # Find the largest selection proportion of any feature in the cluster
  max_prop <- max(colMeans(css_results$feat_sel_mat[, 1L:3L]))
  # Find the selection proportion of the identified prototype
  proto_prop <- colMeans(css_results$feat_sel_mat)[ret]
  testthat::expect_equal(max_prop, proto_prop)

  ## Trying bad inputs

  # Error contains quotation marks
  testthat::expect_error(getSelectionPrototypes(x, good_clusters))

  testthat::expect_error(getSelectionPrototypes(css_results, 1L:3L),
                         "is.list(selected_clusts) is not TRUE", fixed=TRUE)

  testthat::expect_error(getSelectionPrototypes(css_results, list()),
                         "n_selected_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(getSelectionPrototypes(css_results,
                                                list(red_cluster=1L:3L,
                                                     green_cluster=4L:6L,
                                                     bad_cluster=integer())),
                         "all(lengths(selected_clusts) >= 1) is not TRUE",
                         fixed=TRUE)

})
```

```
## Test passed 😸
```

Tests for `printCssDf()`:


```r
testthat::test_that("printCssDf works", {
  set.seed(67234)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=1, cluster_size=3,
                        n_clusters=2, sig_clusters=1, sigma_eps_sq=10^(-6))
  
  x <- data$X
  y <- data$y
  
  good_clusters <- list(red_cluster=1L:3L, green_cluster=4L:6L)
  
  css_results <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B=10)
  
  ret <- printCssDf(css_results)
  
  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoNum",
                                              "ClustSelProp", "ClustSize"))
  
  # Total number of clusters is 11 - (3 - 1) - (3 - 1) = 7
  testthat::expect_equal(nrow(ret), 7)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_true(all(names(good_clusters) %in% ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))
  
  testthat::expect_true(is.integer(ret$ClustProtoNum))
  testthat::expect_true(ret[ret$ClustName=="red_cluster",
                            "ClustProtoNum"] %in% 1L:3L)
  testthat::expect_true(ret[ret$ClustName=="green_cluster",
                            "ClustProtoNum"] %in% 4L:6L)
  other_rows <- !(ret$ClustName %in% c("red_cluster", "green_cluster"))
  testthat::expect_true(all(ret[other_rows, "ClustProtoNum"] %in% 7L:11L))
  testthat::expect_true(length(ret[other_rows, "ClustProtoNum"]) ==
                          length(unique(ret[other_rows, "ClustProtoNum"])))
  
  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))
  
  testthat::expect_true(is.integer(ret$ClustSize))
  testthat::expect_equal(ret[ret$ClustName=="red_cluster", "ClustSize"], 3)
  testthat::expect_equal(ret[ret$ClustName=="green_cluster", "ClustSize"], 3)
  testthat::expect_true(all(ret[other_rows, "ClustSize"] == 1))
  
  # Try again naming features
  
  x_named <- x
  colnames(x_named) <- LETTERS[1:11]
  
  css_results_name_feats <- css(X=x_named, y=y, lambda=0.01,
                                clusters=good_clusters, B=10)
  
  ret <- printCssDf(css_results_name_feats)
  
  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoName",
                                              "ClustProtoNum", "ClustSelProp",
                                              "ClustSize"))
  
  # Total number of clusters is 11 - (3 - 1) - (3 - 1) = 7
  testthat::expect_equal(nrow(ret), 7)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_true(all(names(good_clusters) %in% ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))
  
  testthat::expect_true(is.character(ret$ClustProtoName))
  testthat::expect_true(ret[ret$ClustName=="red_cluster",
                            "ClustProtoName"] %in% LETTERS[1:3])
  testthat::expect_true(ret[ret$ClustName=="green_cluster",
                            "ClustProtoName"] %in% LETTERS[4:6])
  other_rows <- !(ret$ClustName %in% c("red_cluster", "green_cluster"))
  testthat::expect_true(all(ret[other_rows, "ClustProtoName"] %in% LETTERS[7:11]))
  testthat::expect_true(length(ret[other_rows, "ClustProtoName"]) ==
                          length(unique(ret[other_rows, "ClustProtoName"])))
  
  testthat::expect_true(is.integer(ret$ClustProtoNum))
  testthat::expect_true(ret[ret$ClustName=="red_cluster",
                            "ClustProtoNum"] %in% 1L:3L)
  testthat::expect_true(ret[ret$ClustName=="green_cluster",
                            "ClustProtoNum"] %in% 4L:6L)
  testthat::expect_true(all(ret[other_rows, "ClustProtoNum"] %in% 7L:11L))
  testthat::expect_true(length(ret[other_rows, "ClustProtoNum"]) ==
                          length(unique(ret[other_rows, "ClustProtoNum"])))
  
  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))
  
  testthat::expect_true(is.integer(ret$ClustSize))
  testthat::expect_equal(ret[ret$ClustName=="red_cluster", "ClustSize"], 3)
  testthat::expect_equal(ret[ret$ClustName=="green_cluster", "ClustSize"], 3)
  testthat::expect_true(all(ret[other_rows, "ClustSize"] == 1))
  
  # Unnamed clusters
  
  unnamed_clusters <- list(1:3, 4:6)
  
  css_results_unnamed <- css(X=x, y=y, lambda=0.01, clusters=unnamed_clusters,
                             B=10)
  
  ret <- printCssDf(css_results_unnamed)
  
  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoNum",
                                              "ClustSelProp", "ClustSize"))
  
  # Total number of clusters is 11 - (3 - 1) - (3 - 1) = 7
  testthat::expect_equal(nrow(ret), 7)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))
  
  testthat::expect_true(is.integer(ret$ClustProtoNum))
  
  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))
  
  testthat::expect_true(is.integer(ret$ClustSize))
  
  # Try other settings for cutoff, min_num_clusts, max_num_clusts, etc.
  
  ret <- printCssDf(css_results, max_num_clusts=3)
  
  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoNum",
                                              "ClustSelProp", "ClustSize"))

  testthat::expect_true(nrow(ret) <= 3)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))
  
  testthat::expect_true(is.integer(ret$ClustProtoNum))
  other_rows <- !(ret$ClustName %in% c("red_cluster", "green_cluster"))
  testthat::expect_true(all(ret[other_rows, "ClustProtoNum"] %in% 7L:11L))
  testthat::expect_true(length(ret[other_rows, "ClustProtoNum"]) ==
                          length(unique(ret[other_rows, "ClustProtoNum"])))
  
  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))
  
  testthat::expect_true(is.integer(ret$ClustSize))
  testthat::expect_true(all(ret[other_rows, "ClustSize"] == 1))
  
  if("red_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="red_cluster",
                              "ClustProtoNum"] %in% 1L:3L)
    testthat::expect_equal(ret[ret$ClustName=="red_cluster", "ClustSize"], 3)
  }
  
  if("green_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="green_cluster",
                              "ClustProtoNum"] %in% 4L:6L)
    testthat::expect_equal(ret[ret$ClustName=="green_cluster", "ClustSize"], 3)
  }
  
  ret <- printCssDf(css_results, min_num_clusts=2, cutoff=1)
  
  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoNum",
                                              "ClustSelProp", "ClustSize"))
  
  # Total number of clusters is 11 - (3 - 1) - (3 - 1) = 7
  testthat::expect_true(nrow(ret) >= 2)
  testthat::expect_true(nrow(ret) <= 7)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))
  
  testthat::expect_true(is.integer(ret$ClustProtoNum))
  other_rows <- !(ret$ClustName %in% c("red_cluster", "green_cluster"))
  testthat::expect_true(all(ret[other_rows, "ClustProtoNum"] %in% 7L:11L))
  testthat::expect_true(length(ret[other_rows, "ClustProtoNum"]) ==
                          length(unique(ret[other_rows, "ClustProtoNum"])))
  
  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))
  
  testthat::expect_true(is.integer(ret$ClustSize))
  testthat::expect_true(all(ret[other_rows, "ClustSize"] == 1))
  
  if("red_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="red_cluster",
                              "ClustProtoNum"] %in% 1L:3L)
    testthat::expect_equal(ret[ret$ClustName=="red_cluster", "ClustSize"], 3)
  }
  
  if("green_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="green_cluster",
                              "ClustProtoNum"] %in% 4L:6L)
    testthat::expect_equal(ret[ret$ClustName=="green_cluster", "ClustSize"], 3)
  }
  
  #
  ret <- printCssDf(css_results, cutoff=1)

  testthat::expect_true(is.data.frame(ret))
  testthat::expect_identical(colnames(ret), c("ClustName", "ClustProtoNum",
                                              "ClustSelProp", "ClustSize"))

  testthat::expect_true(nrow(ret) >= 1)
  testthat::expect_true(nrow(ret) <= 7)

  testthat::expect_true(is.character(ret$ClustName))
  testthat::expect_equal(length(ret$ClustName), length(unique(ret$ClustName)))

  testthat::expect_true(is.integer(ret$ClustProtoNum))
  other_rows <- !(ret$ClustName %in% c("red_cluster", "green_cluster"))
  testthat::expect_true(all(ret[other_rows, "ClustProtoNum"] %in% 7L:11L))
  testthat::expect_true(length(ret[other_rows, "ClustProtoNum"]) ==
                          length(unique(ret[other_rows, "ClustProtoNum"])))

  testthat::expect_true(is.numeric(ret$ClustSelProp))
  testthat::expect_identical(ret$ClustSelProp, sort(ret$ClustSelProp,
                                                    decreasing=TRUE))

  testthat::expect_true(is.integer(ret$ClustSize))
  testthat::expect_true(all(ret[other_rows, "ClustSize"] == 1))

  if("red_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="red_cluster",
                              "ClustProtoNum"] %in% 1L:3L)
    testthat::expect_equal(ret[ret$ClustName=="red_cluster", "ClustSize"], 3)
  }

  if("green_cluster" %in% ret$ClustName){
    testthat::expect_true(ret[ret$ClustName=="green_cluster",
                              "ClustProtoNum"] %in% 4L:6L)
    testthat::expect_equal(ret[ret$ClustName=="green_cluster", "ClustSize"], 3)
  }

  ## Trying bad inputs

  # Error has quotation marks in it
  testthat::expect_error(printCssDf("css_results"))
  
  testthat::expect_error(printCssDf(css_results, cutoff=-.1),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(printCssDf(css_results, min_num_clusts=3.2),
                         "min_num_clusts == round(min_num_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(printCssDf(css_results, max_num_clusts="5"),
                         "is.numeric(max_num_clusts) | is.integer(max_num_clusts) is not TRUE",
                         fixed=TRUE)
})
```

```
## Test passed 🎉
```

`print.cssr()`


```r
#' Print cluster stability selection output
#'
#' Print a summary of the information from the css function (using output from
#' printCssDf function).
#' @param x An object of class "cssr" (the output of the function css).
#' @param cutoff Numeric; print.cssr will display only those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1. Default is 0 (in which case either all clusters are displayed, or
#' max_num_clusts are, if max_num_clusts is specified).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) Default is 1.
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param ... Additional arguments to generic print.data.frame function
#' @return A data.frame; each row contains a cluster, arranged in decreasing
#' order of cluster selection proportion from top to bottom. The columns are
#' ClustName (the name of the cluster that was either provided to css or made by
#' css if no name was provided); ClustProtoName (the name of the selection
#' prototype from the cluster, which is the feature with the greatest individual
#' selection proportion among all the cluster members, with ties broken by
#' choosing the feature with the highest correlation with the response if the
#' response is real-valued; only returned if the features are named),
#' ClustProtoNum (the column number of the prototype in the X matrix provided to
#' css), and ClustSize (the size of the cluster).
#' @author Gregory Faletto, Jacob Bien
#' @export
print.cssr <- function(x, cutoff=0, min_num_clusts=1, max_num_clusts=NA, ...){
    df <- printCssDf(css_results=x, cutoff=cutoff,
        min_num_clusts=min_num_clusts, max_num_clusts=max_num_clusts)
    print.data.frame(df, ...)
}
```


# Convenient wrapper functions {#wrapper-functions}

Finally, we provide convenient wrapper functions which yield user-desired output in a single step at the price of flexibility and efficiency. `cssSelect()` yields a selected set of clusters and features (the same output as `getCssSelections()`) in a single function call. `cssPredict()` takes in a training/selection data set as well as a test X. It uses the labeled data to select a set of features and train an OLS model on the selected features, and then it generates predictions on the test set using the fitted model.

Besides requiring only a single function call to yield desired output, these wrapper functions also automatically select hyperparameters (lambda used for the lasso, a desired model size, and even selection and training splits for `cssPredict()`) in a sensible way if these values are not provided by the user. So these functions are very convenient for an end user who wants quick results without getting "under the hood."

The two main disadvantages of these functions are flexibility and computational efficiency. For simplicity of use, these functions do not provide as many options as the component functions they call (for example, `min_num_clusts` is not an available argument for these models). Further, both of these functions make (computationally intensive) calls to `css()` internally every time they are called, so these functions are not recommended for users who want to tinker with the model size and other parameters. Instead, it would be more efficient to call css once and then work with the output as desired using the other package functions, which are very efficient given the stored output from `css()`.

`cssSelect()` and `cssPredict()` have no new dependencies; they rely only on already-defined functions.

`cssSelect()`:


```r
#' Obtain a selected set of clusters and features using cluster stability
#' selection
#'
#' Takes in data X and y and returns a set of clusters (and a set of features)
#' that are useful for predicting y from the data in X. This is a wrapper
#' function for css and getCssSelections. Using cssSelect is simpler, but it
#' has fewer options, and it executes the full (computationally expensive)
#' subsampling procedured every time it is called. In contrast, css can be
#' called just once, and then getCssSelections can quickly return results using
#' different values of cutoff, max_num_clusts, etc. from the calculations done
#' in one call to css.
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors.
#' @param y A length-n numeric vector containing the responses; `y[i]` is the
#' response corresponding to observation `X[i, ]`. (Note that for the css
#' function, y does not have to be a numeric response, but for this function,
#' the underlying selection procedure is the lasso, so y must be a real-valued
#' response.)
#' @param clusters Optional; either an integer vector of a list of integer
#' vectors; each vector should contain the indices of a cluster of features (a
#' subset of 1:p). (If there is only one cluster, clusters can either be a list
#' of length 1 or an integer vector.) All of the provided clusters must be
#' non-overlapping. Every feature not appearing in any cluster will be assumed
#' to be unclustered (that is, they  will be treated as if they are in a
#' "cluster" containing only themselves). If clusters is a list of length 0 (or
#' a list only containing clusters of length 1), then css() returns the same
#' results as stability selection (so feat_sel_mat will be identical to
#' clus_sel_mat). Names for the clusters will be needed later; any clusters that
#' are not given names in the list clusters will be given names automatically by
#' css. Default is list() (so no clusters are specified, and every feature is
#' assumed to be in a "cluster" containng only itself).
#' @param lambda Optional; the tuning parameter to be used by the lasso for
#' feature selection in each subsample. If lambda is not provided, cssSelect
#' will choose one automatically by cross-validation. Default is NA.
#' @param cutoff Numeric; cssSelect will only select those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is NA (in which case max_num_clusts are used).
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' either cutoff is used to choose the number of clusters, or if cutoff was also
#' unspecified, cssSelect chooses max_num_clusts by cross-validation).
#' @param auto_select_size Logical; if TRUE, then max_num_clusts will be
#' automatically estimated using the lasso with cross-validation. Default is
#' TRUE, though his argument is ignored if either cutoff or max_num_clusts is
#' provided. (If desired output is to return all clusters, you should set
#' auto_select_size to FALSE and do not provide cutoff or max_num_clusts.)
#' @return A named list with two items. \item{selected_clusts}{A list of
#' integer vectors; each vector contains the indices of one of the selected
#' clusters.} \item{selected_feats}{An integer vector; the indices of the
#' all of the selected features within all of the selected clusters (typically
#' only one feature is selected from each cluster).}
#' @author Gregory Faletto, Jacob Bien
#' @export
cssSelect <- function(X, y, clusters = list(), lambda=NA, cutoff=NA,
    max_num_clusts=NA, auto_select_size=TRUE){

    # Check inputs (most inputs will be checked by called functions)

    stopifnot(!is.na(auto_select_size))
    stopifnot(length(auto_select_size) == 1)
    stopifnot(is.logical(auto_select_size))

    stopifnot(is.matrix(X) | is.data.frame(X))
    stopifnot(all(!is.na(X)))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))

    if(!is.numeric(y) & !is.integer(y)){
        stop("The provided y must be real-valued, because cssSelect uses the lasso for feature selection. (In order to use a different form of response, use the css function and provide your own selection function accommodating your choice of y.)")
    }

    stopifnot(length(lambda) == 1)
    if(is.na(lambda)){
        lambda <- getLassoLambda(X, y)
    }

    css_results <- css(X, y, lambda, clusters)

    # If no indication of how to select model size was provided, choose model
    # size by cross-validation
    if(is.na(cutoff) & is.na(max_num_clusts)){
        if(auto_select_size){
            max_num_clusts <- getModelSize(X, y, css_results$clusters)
        }
    }

    if(is.na(cutoff)){
        cutoff <- 0
    }

    # Get selected features
    getCssSelections(css_results, weighting="sparse", cutoff=cutoff,
        min_num_clusts=1, max_num_clusts=max_num_clusts)
}
```

Tests for `cssSelect()`:


```r
testthat::test_that("cssSelect works", {
  set.seed(73212)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=1, cluster_size=3,
                        n_clusters=2, sig_clusters=1, sigma_eps_sq=1)
  
  x <- data$X
  y <- data$y
  
  # Intentionally don't provide clusters for all features, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:3L, 4:6)
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  testthat::expect_true(!is.null(names(res$selected_clusts)))
  testthat::expect_true(is.character(names(res$selected_clusts)))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))
  # Total of 11 - 2*(3 - 1) = 7 clusters
  testthat::expect_true(length(res$selected_clusts) <= 7)
  testthat::expect_true(length(res$selected_clusts) >= 1)

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:11))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= 11)
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:11))
  
  testthat::expect_true(length(res$selected_feats) <= 11)
  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 11))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))
  
  # No provided clusters
  
  res <- cssSelect(X=x, y=y)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  testthat::expect_true(!is.null(names(res$selected_clusts)))
  testthat::expect_true(is.character(names(res$selected_clusts)))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))

  testthat::expect_true(length(res$selected_clusts) <= 11)
  testthat::expect_true(length(res$selected_clusts) >= 1)

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:11))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= 11)
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:11))
  
  testthat::expect_true(length(res$selected_feats) <= 11)
  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 11))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))

  ## Trying other inputs

  # X as a data.frame
  X_df <- datasets::mtcars
  
  res <- cssSelect(X=X_df, y=stats::rnorm(nrow(X_df)), clusters=1:3)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  testthat::expect_true(!is.null(names(res$selected_clusts)))
  testthat::expect_true(is.character(names(res$selected_clusts)))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))
  # Total of ncol(X_df) - (3 - 1) clusters
  testthat::expect_true(length(res$selected_clusts) <= ncol(X_df) - 2)
  testthat::expect_true(length(res$selected_clusts) >= 1)

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:ncol(X_df)))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= ncol(X_df))
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:ncol(X_df)))

  testthat::expect_true(length(res$selected_feats) <= ncol(X_df))
  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= ncol(X_df)))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))

  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  res <- cssSelect(X=df2, y=stats::rnorm(nrow(X_df)), clusters=1:3)
  
  X_df_mat <- stats::model.matrix(~ ., df2)
  X_df_mat <- X_df_mat[, colnames(X_df_mat) != "(Intercept)"]
  p <- ncol(X_df_mat)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  testthat::expect_true(!is.null(names(res$selected_clusts)))
  testthat::expect_true(is.character(names(res$selected_clusts)))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))
  # Total of p - (3 - 1) clusters
  testthat::expect_true(length(res$selected_clusts) <= p - 2)
  testthat::expect_true(length(res$selected_clusts) >= 1)

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:p))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= p)
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:p))

  testthat::expect_true(length(res$selected_feats) <= p)
  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= p))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))


  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]
  
  res <- cssSelect(X=x2, y=y, clusters=good_clusters)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  testthat::expect_true(!is.null(names(res$selected_clusts)))
  testthat::expect_true(is.character(names(res$selected_clusts)))
  testthat::expect_true(length(res$selected_clusts) <=
                          length(res$selected_feats))
  # Total of 11 - 2*(3 - 1) = 7 clusters
  testthat::expect_true(length(res$selected_clusts) <= 7)
  testthat::expect_true(length(res$selected_clusts) >= 1)

  testthat::expect_true(is.list(res$selected_clusts))
  testthat::expect_equal(length(names(res$selected_clusts)),
                           length(unique(names(res$selected_clusts))))
  already_used_feats <- integer()
  for(i in 1:length(res$selected_clusts)){
    sels_i <- res$selected_clusts[[i]]
    testthat::expect_true(length(sels_i) >= 1)
    testthat::expect_true(is.integer(sels_i))
    testthat::expect_true(all(sels_i %in% 1:11))
    testthat::expect_equal(length(sels_i), length(unique(sels_i)))
    testthat::expect_equal(length(intersect(already_used_feats, sels_i)), 0)
    already_used_feats <- c(already_used_feats, sels_i)
  }
  testthat::expect_true(length(already_used_feats) <= 11)
  testthat::expect_equal(length(already_used_feats),
                         length(unique(already_used_feats)))
  testthat::expect_true(all(already_used_feats %in% 1:11))
  
  testthat::expect_true(length(res$selected_feats) <= 11)
  testthat::expect_true(is.integer(res$selected_feats))
  testthat::expect_true(length(res$selected_feats) >= 1)
  testthat::expect_equal(length(names(res$selected_feats)),
                         length(unique(names(res$selected_feats))))
  testthat::expect_true(all(res$selected_feats >= 1))
  testthat::expect_true(all(res$selected_feats <= 11))
  testthat::expect_equal(length(res$selected_feats),
                             length(unique(res$selected_feats)))
  
  # Vary inputs
  res <- cssSelect(X=x, y=y, clusters=good_clusters, lambda=0.01)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, cutoff=0.6)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, max_num_clusts=6)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  testthat::expect_true(length(res$selected_clusts) <= 6)
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, auto_select_size=FALSE)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats"))
  # Total of 11 - 2*(3 - 1) = 7 clusters
  testthat::expect_equal(length(res$selected_clusts), 7)
  
  # Bad inputs
  testthat::expect_error(cssSelect(X=x[1:10, ], y=y),
                         "n == length(y) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=character(5), y=y),
                         "is.matrix(X) | is.data.frame(X) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=matrix(1:15, 5, 3)),
                         "!is.matrix(y) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=factor(rbinom(15, size=1, prob=.5))),
                         "The provided y must be real-valued, because cssSelect uses the lasso for feature selection. (In order to use a different form of response, use the css function and provide your own selection function accommodating your choice of y.)",
                         fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=y, clusters="clusters"),
                         "is.numeric(clusters) | is.integer(clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=y, lambda=-.1),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=y, cutoff=1.1),
                         "cutoff <= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=y, max_num_clusts=1000),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)
  
  testthat::expect_error(cssSelect(X=x, y=y, auto_select_size=1),
                         "is.logical(auto_select_size) is not TRUE", fixed=TRUE)
})
```

```
## ── Warning ('<text>:14'): cssSelect works ──────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:14'): cssSelect works ──────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters)
##  2. litr (local) getModelSize(X, y, css_results$clusters)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:58'): cssSelect works ──────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:58'): cssSelect works ──────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y)
##  2. litr (local) getModelSize(X, y, css_results$clusters)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:105'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = X_df, y = stats::rnorm(nrow(X_df)), clusters = 1:3)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:105'): cssSelect works ─────────────────────────────────────
## Returning more than max_num_clusts = 1 clusters because increasing the cutoff any further would require returning 0 clusters
## Backtrace:
##  1. litr (local) cssSelect(X = X_df, y = stats::rnorm(nrow(X_df)), clusters = 1:3)
##  2. litr (local) getCssSelections(...)
##  3. litr (local) getSelectedClusters(...)
##  4. litr (local) checkSelectedClusters(...)
## 
## ── Warning ('<text>:156'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = df2, y = stats::rnorm(nrow(X_df)), clusters = 1:3)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:207'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x2, y = y, clusters = good_clusters)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:207'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x2, y = y, clusters = good_clusters)
##  2. litr (local) getModelSize(X, y, css_results$clusters)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:250'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters, lambda = 0.01)
##  2. litr (local) getModelSize(X, y, css_results$clusters)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:256'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters, cutoff = 0.6)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:262'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters, max_num_clusts = 6)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:269'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssSelect(X = x, y = y, clusters = good_clusters, auto_select_size = FALSE)
##  2. litr (local) getLassoLambda(X, y)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:285'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssSelect(X = x, y = matrix(1:15, 5, 3))
##   8. litr (local) getLassoLambda(X, y)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:292'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssSelect(X = x, y = y, clusters = "clusters")
##   8. litr (local) getLassoLambda(X, y)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:300'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssSelect(X = x, y = y, cutoff = 1.1)
##   8. litr (local) getLassoLambda(X, y)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:303'): cssSelect works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssSelect(X = x, y = y, max_num_clusts = 1000)
##   8. litr (local) getLassoLambda(X, y)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
```

`cssPredict()`:


```r
#' Wrapper function to generate predictions from cluster stability selected
#' model in one step
#'
#' Select clusters using cluster stability selection, form cluster
#' representatives, fit a linear model, and generate predictions from a matrix
#' of unlabeled data. This is a wrapper function for css and getCssPreds. Using
#' cssPredict is simpler, but it has fewer options, and it executes the full
#' (computationally expensive) subsampling procedured every time it is called.
#' In contrast, css can be called just once, and then cssPredict can quickly
#' return results for different matrices of new data or using different values
#' of cutoff, max_num_clusts, etc. by using the calculations done in one call to
#' css.
#'
#' @param X_train_selec An n x p numeric matrix (preferably) or a data.frame
#' (which will be coerced internally to a matrix by the function model.matrix)
#' containing the p >= 2 features/predictors. The data from X_train_selec and
#' y_train_selec will be split into two parts; half of the data will be used for
#' feature selection by cluster stability selection, and half will be used for
#' estimating a linear model on the selected cluster representatives.
#' @param y_train_selec A length-n numeric vector containing the responses;
#' `y[i]` is the response corresponding to observation `X[i, ]`. Unlke the more
#' general setup of css, y_train_selec must be real-valued because predictions
#' will be generated by ordinary least squares.
#' @param X_test A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the data that will be used to generate predictions. Must contain the same
#' features (in the same number of columns) as X_train_selec, and if the columns
#' of X_test are named, they must match the names of X_train_selec.
#' @param clusters Optional; either an integer vector of a list of integer
#' vectors; each vector should contain the indices of a cluster of features (a
#' subset of 1:p). (If there is only one cluster, clusters can either be a list
#' of length 1 or an integer vector.) All of the provided clusters must be
#' non-overlapping. Every feature not appearing in any cluster will be assumed
#' to be unclustered (that is, they  will be treated as if they are in a
#' "cluster" containing only themselves). If clusters is a list of length 0 (or
#' a list only containing clusters of length 1), then css() returns the same
#' results as stability selection (so feat_sel_mat will be identical to
#' clus_sel_mat). Names for the clusters will be needed later; any clusters that
#' are not given names in the list clusters will be given names automatically by
#' css. Default is list() (so no clusters are specified, and every feature is
#' assumed to be in a "cluster" containng only itself).
#' @param lambda Optional; the tuning parameter to be used by the lasso for
#' feature selection in each subsample. If lambda is not provided, cssPredict
#' will choose one automatically by cross-validation. Default is NA.
#' @param cutoff Numeric; getCssPreds will make use only of those clusters with
#' selection proportions equal to at least cutoff. Must be between 0 and 1.
#' Default is 0 (in which case either all clusters are used, or max_num_clusts
#' are used, if max_num_clusts is specified).
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Default is NA (in which case
#' max_num_clusts is ignored).
#' @param train_inds Optional; an integer or numeric vector containing the
#' indices of observations in X and y to set aside for model training after
#' feature selection. If train_inds is not provided, half of the data will be
#' used for feature selection and half for model estimation (chosen at random).
#' @param auto_select_size Logical; if TRUE, then max_num_clusts will be
#' automatically estimated using the lasso with cross-validation. Default is
#' TRUE, though his argument is ignored if either cutoff or max_num_clusts is
#' provided. (If desired output is to generate predictions using all clusters,
#' you should set auto_select_size to FALSE and do not provide cutoff or
#' max_num_clusts.)
#' @return A numeric vector of length nrow(X_test) of predictions
#' corresponding to the observations from X_test.
#' @author Gregory Faletto, Jacob Bien
#' @export
cssPredict <- function(X_train_selec, y_train_selec, X_test, clusters=list(),
    lambda=NA, cutoff=NA, max_num_clusts=NA, train_inds=NA,
    auto_select_size=TRUE){

    # Check inputs (most inputs will be checked by called functions)
    if(!is.numeric(y_train_selec) & !is.integer(y_train_selec)){
        stop("The provided y_train_selec must be real-valued, because predictions will be generated by ordinary least squares regression.")
    }

    stopifnot(!is.na(auto_select_size))
    stopifnot(length(auto_select_size) == 1)
    stopifnot(is.logical(auto_select_size))

    stopifnot(is.matrix(X_train_selec) | is.data.frame(X_train_selec))
    stopifnot(all(!is.na(X_train_selec)))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X_train_selec)){
        X_train_selec <- stats::model.matrix(~ ., X_train_selec)
        X_train_selec <- X_train_selec[, colnames(X_train_selec) !=
            "(Intercept)"]
    }

    stopifnot(is.matrix(X_train_selec))
    stopifnot(all(!is.na(X_train_selec)))

    n <- nrow(X_train_selec)

    if(any(is.na(train_inds))){
        train_inds <- sample(n, size=round(n/2))
    }

    stopifnot(length(lambda) == 1)
    if(is.na(lambda)){
        lambda <- getLassoLambda(X_train_selec[setdiff(1:n, train_inds), ],
            y_train_selec[setdiff(1:n, train_inds)]) 
    }

    css_results <- css(X=X_train_selec, y=y_train_selec, lambda=lambda,
        clusters=clusters, train_inds=train_inds)

    # If no indication of how to select model size was provided, choose model
    # size by cross-validation
    if(is.na(cutoff) & is.na(max_num_clusts)){
        if(auto_select_size){
            max_num_clusts <- getModelSize(X_train_selec[train_inds, ],
                y_train_selec[train_inds], css_results$clusters)
        }
    }

    if(is.na(cutoff)){
        cutoff <- 0
    }

    # Get predictions
    getCssPreds(css_results, testX=X_test, weighting="weighted_avg",
        cutoff=cutoff, max_num_clusts=max_num_clusts)
}
```

Tests for `cssPredict()`:


```r
testthat::test_that("cssPredict works", {
  set.seed(84231)
  
  train_data <- genClusteredData(n=30, p=11, k_unclustered=1, cluster_size=3,
                              n_clusters=2, sig_clusters=1, sigma_eps_sq=1)
  
  x <- train_data$X
  y <- train_data$y
  
  test_x <- genClusteredData(n=5, p=11, k_unclustered=1, cluster_size=3,
                          n_clusters=2, sig_clusters=1, sigma_eps_sq=1)$X
  
  # Intentionally don't provide clusters for all features, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:3L, 4:6)
  
  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    clusters=good_clusters)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)
  
  # No provided clusters

  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x)

  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)
  
  # Provide training indices
  
  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    train_inds=13:28)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  ## Trying other inputs

  # X as a data.frame
  X_df <- datasets::mtcars
  
  n <- nrow(X_df)
  test_inds <- 1:round(n/3)
  n_test <- length(test_inds)
  selec_train_inds <- setdiff(1:n, test_inds)
  n_selec_train <- length(selec_train_inds)

  res <- cssPredict(X_train_selec=X_df[selec_train_inds, ],
                    y_train_selec=stats::rnorm(n_selec_train),
                    X_test=X_df[test_inds, ])
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), n_test)

  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  res <- cssPredict(X_train_selec=df2[selec_train_inds, ],
                    y_train_selec=stats::rnorm(n_selec_train),
                    X_test=df2[test_inds, ])
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), n_test)

  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]
  test_x2 <- test_x
  colnames(test_x2) <- LETTERS[1:11]

  res <- cssPredict(X_train_selec=x2, y_train_selec=y, X_test=test_x2,
                    clusters=good_clusters)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  # Vary inputs
  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    lambda=0.01)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x, cutoff=0.6)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    max_num_clusts=6)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    auto_select_size=FALSE)
  
  testthat::expect_true(all(!is.na(res)))
  testthat::expect_true(is.numeric(res))
  testthat::expect_equal(length(res), 5)

  # Bad inputs
  testthat::expect_error(cssPredict(X_train_selec=x[1:10, ], y_train_selec=y,
                                    X_test=test_x),
                         "length(y) == n is not TRUE", fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=character(30),
                                    y_train_selec=y, X_test=test_x),
                         "is.matrix(X_train_selec) | is.data.frame(X_train_selec) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x,
                                    y_train_selec=matrix(1:30, 10, 3),
                                    X_test=test_x), "!is.matrix(y) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x,
                                    y_train_selec=factor(rbinom(30, size=1,
                                                                prob=.5)),
                                    X_test=test_x),
                         "The provided y_train_selec must be real-valued, because predictions will be generated by ordinary least squares regression.",
                         fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, clusters="clusters"),
                         "is.numeric(clusters) | is.integer(clusters) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, lambda="lambda"),
                         "For method cssLasso, lambda must be a numeric.",
                         fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, cutoff=-.1),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, max_num_clusts=0),
                         "max_num_clusts >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, auto_select_size=c(TRUE,
                                                                      FALSE)),
                         "length(auto_select_size) == 1 is not TRUE",
                         fixed=TRUE)
})
```

```
## ── Warning ('<text>:17'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:17'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:26'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(X_train_selec = x, y_train_selec = y, X_test = test_x)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:26'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(X_train_selec = x, y_train_selec = y, X_test = test_x)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:34'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:34'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:52'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:52'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:69'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:69'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:83'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:83'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:91'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getModelSize(...)
##  3. glmnet::cv.glmnet(x = X_size, y = y, family = "gaussian")
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:98'): cssPredict works ─────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:104'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:111'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##  1. litr (local) cssPredict(...)
##  2. litr (local) getLassoLambda(...)
##  3. glmnet::cv.glmnet(...)
##  4. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:119'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssPredict(X_train_selec = x[1:10, ], y_train_selec = y, X_test = test_x)
##   8. litr (local) getLassoLambda(...)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:128'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssPredict(...)
##   8. litr (local) getLassoLambda(...)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:140'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssPredict(...)
##   8. litr (local) getLassoLambda(...)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:150'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssPredict(...)
##   8. litr (local) getLassoLambda(...)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
## 
## ── Warning ('<text>:154'): cssPredict works ────────────────────────────────────
## Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
## Backtrace:
##   1. testthat::expect_error(...)
##   7. litr (local) cssPredict(...)
##   8. litr (local) getLassoLambda(...)
##   9. glmnet::cv.glmnet(...)
##  10. glmnet:::cv.glmnet.raw(...)
```

# Competitor Methods

We also provide implementations of some competitor feature selection methods. We used these in the simulation studies in our paper to compare cluster stability selection to the protolasso (Redi and Tibshirani, 2016) and the cluster representative lasso (Bühlmann et. al. 2013), two other feature selection methods that are designed for data with clustered features. These feature selection methods are in some ways closely related, so their implementations share helper functions.

* `protolasso()`
  - `processClusterLassoInputs()` checks and formats the function inputs
  - `getXglmnet()` formats the provided design matrix `Xglmnet` for the lasso as implemented by `glmnet` (for the protolasso, this means discarding all features from each cluster except the one most highly correlated with the response; for the cluster representative lasso, this means replacing the clustered features with a simple average of the cluster members).
    * `checkGetXglmnetInputs()` verifies the inputs to `getXglmnet()`
  - Finally, `getClusterSelsFromGlmnet()` extracts the relevant output from the results yielded by a `glmnet` lasso fit.
    * `getSelectedSets()` takes in a single selected set from `Xglmnet` and yields a selected feature set in the original feature space (with each selected cluster from `Xglmnet` replaced by its prototype) as well as a selected set of clusters.
* `clusterRepLasso()`

`protolasso()`:


```r
#' Select features via the protolasso (Reid and Tibshirani 2016)
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors
#' @param y The response; A length n numeric (or integer) real-valued vector.
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves).
#' Default is list() (so no clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the protolasso. Default is 100 (following the default for glmnet). For
#' now, nlambda must be at least 2 (using a single lambda is not supported).
#' @return A list with three elements. \item{selected_sets}{A list of integer
#' vectors. Entry k of this list contains a selected set (an integer vector) of
#' size k yielded by the protolasso (If no set of size k was selected, entry k
#' will be empty.)} \item{selected_clusts_list}{A list; each element of the list
#' is a named list of selected clusters. (That is, if a selected set of size k
#' was yielded by the protolasso, then selected_clusts_list[[k]] is a named
#' list of length k, where each member of the list is an integer vector
#' of cluster members. In particular, selected_clusts_lists[[k]][[j]] will be
#' the cluster that contains feature selected_sets[[k]][j].)} \item{beta}{The
#' beta output from glmnet when the lasso was estimated on a matrix of
#' prototypes. (See documentation for the function glmnet from the glmnet
#' package for details.)}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
#' @export
protolasso <- function(X, y, clusters, nlambda=100){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the protolasso procedure
    X_glmnet <- getXglmnet(x, clusters, type="protolasso",
        prototypes=prototypes)

    # Estimate the lasso on the cluster prototypes
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

    # Finally, obtain a tidy list of selected sets--one for each model size
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}
```

`processClusterLassoInputs()`:


```r
#' Check the inputs to protolasso and clusterRepLasso, format clusters, and
#' identify prototypes for each cluster
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors
#' @param y The response; A length n numeric (or integer) real-valued vector.
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves).
#' Default is list() (so no clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the protolasso. Default is 100 (following the default for glmnet). For
#' now, nlambda must be at least 2 (using a single lambda is not supported).
#' @return A list with four elements. \item{x}{The provided X, converted to a
#' matrix if it was provided as a data.frame, and with column names removed.}
#' \item{clusters}{A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters are
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features will be put in their own "cluster" of size 1).}
#' \item{prototypes}{An integer vector whose length is equal to the number of
#' clusters. Entry i is the index of the feature belonging to cluster i that is
#' most highly correlated with y (that is, the prototype for the cluster, as in
#' the protolasso; see Reid and Tibshirani 2016).} \item{var_names}{If the
#' provided X matrix had column names, the names of the featurrs in the provided
#' X matrix. If no names were provided, feat_names will be NA.}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
processClusterLassoInputs <- function(X, y, clusters, nlambda){

    stopifnot(is.matrix(X) | is.data.frame(X))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))

    feat_names <- as.character(NA)
    if(!is.null(colnames(X))){
        feat_names <- colnames(X)
        if(any(is.na(feat_names))){
            stop("Some features in provided X matrix had valid names and some had NA names; please neither name all features in X or remove the names altogether.")
        }
    }

    n <- nrow(X)

    colnames(X) <- character()

    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(n == length(y))
    stopifnot(all(!is.na(y)))

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    # Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

    cluster_results <- formatClusters(clusters, p=ncol(X),
        clust_names=clust_names, get_prototypes=TRUE, x=X, y=y)

    clusters <- cluster_results$clusters
    prototypes <- cluster_results$prototypes

    rm(cluster_results)

    stopifnot(length(clusters) == length(prototypes))

    stopifnot(is.numeric(nlambda) | is.integer(nlambda))
    stopifnot(length(nlambda) == 1)
    stopifnot(!is.na(nlambda))
    stopifnot(nlambda >= 2)
    stopifnot(nlambda == round(nlambda))

    return(list(x=X, clusters=clusters, prototypes=prototypes,
        var_names=feat_names))
}
```

Tests for `processClusterLassoInputs()`:


```r
testthat::test_that("processClusterLassoInputs works", {
  set.seed(82612)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)

  ret <- processClusterLassoInputs(X=x, y=y, clusters=good_clusters, nlambda=10)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("x", "clusters", "prototypes",
                                           "var_names"))
  
  # X
  testthat::expect_true(is.matrix(ret$x))
  testthat::expect_true(all(!is.na(ret$x)))
  testthat::expect_true(is.numeric(ret$x))
  testthat::expect_equal(ncol(ret$x), 11)
  testthat::expect_equal(nrow(ret$x), 15)
  testthat::expect_true(all(abs(ret$x - x) < 10^(-9)))
  
  # clusters
  testthat::expect_true(is.list(ret$clusters))
  testthat::expect_equal(length(ret$clusters), 5)
  testthat::expect_equal(5, length(names(ret$clusters)))
  testthat::expect_equal(5, length(unique(names(ret$clusters))))
  testthat::expect_true("red_cluster" %in% names(ret$clusters))
  testthat::expect_true("green_cluster" %in% names(ret$clusters))
  testthat::expect_true(all(!is.na(names(ret$clusters))))
  testthat::expect_true(all(!is.null(names(ret$clusters))))
  testthat::expect_true(all(names(ret$clusters) != ""))

  clust_feats <- integer()
  true_list <- list(1:4, 5:8, 9, 10, 11)
  for(i in 1:length(ret$clusters)){
    testthat::expect_true(is.integer(ret$clusters[[i]]))
    testthat::expect_equal(length(intersect(clust_feats, ret$clusters[[i]])), 0)
    testthat::expect_true(all(ret$clusters[[i]] %in% 1:11))
    testthat::expect_equal(length(ret$clusters[[i]]),
                           length(unique(ret$clusters[[i]])))
    testthat::expect_true(all(ret$clusters[[i]] == true_list[[i]]))
    clust_feats <- c(clust_feats, ret$clusters[[i]])
  }

  testthat::expect_equal(length(clust_feats), 11)
  testthat::expect_equal(11, length(unique(clust_feats)))
  testthat::expect_equal(11, length(intersect(clust_feats, 1:11)))
  
  # prototypes
  testthat::expect_true(is.integer(ret$prototypes))
  testthat::expect_true(all(ret$prototypes %in% 1:11))
  testthat::expect_equal(length(ret$prototypes), 5)
  testthat::expect_true(ret$prototypes[1] %in% 1:4)
  testthat::expect_true(ret$prototypes[2] %in% 5:8)
  testthat::expect_equal(ret$prototypes[3], 9)
  testthat::expect_equal(ret$prototypes[4], 10)
  testthat::expect_equal(ret$prototypes[5], 11)

  # var_names
  testthat::expect_equal(length(ret$var_names), 1)
  testthat::expect_true(is.na(ret$var_names))
  
  # X as a data.frame
  X_df <- datasets::mtcars
  res <- processClusterLassoInputs(X=X_df, y=stats::rnorm(nrow(X_df)),
                                   clusters=1:3, nlambda=10)
  
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("x", "clusters", "prototypes",
                                           "var_names"))
  
  X_df_model <- stats::model.matrix(~ ., X_df)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  # X
  testthat::expect_true(is.matrix(res$x))
  testthat::expect_true(all(!is.na(res$x)))
  testthat::expect_true(is.numeric(res$x))
  testthat::expect_equal(ncol(res$x), ncol(X_df_model))
  testthat::expect_equal(nrow(res$x), nrow(X_df))
  testthat::expect_true(all(abs(res$x - X_df_model) < 10^(-9)))
  
  # var_names
  testthat::expect_equal(length(res$var_names), ncol(X_df_model))
  testthat::expect_true(is.character(res$var_names))
  testthat::expect_identical(res$var_names, colnames(X_df_model))


  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=1:3, nlambda=10)
  
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("x", "clusters", "prototypes",
                                           "var_names"))
  
  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  # X
  testthat::expect_true(is.matrix(res$x))
  testthat::expect_true(all(!is.na(res$x)))
  testthat::expect_true(is.numeric(res$x))
  testthat::expect_equal(ncol(res$x), ncol(X_df_model))
  testthat::expect_equal(nrow(res$x), nrow(X_df))
  testthat::expect_true(all(abs(res$x - X_df_model) < 10^(-9)))
  
  # var_names
  testthat::expect_equal(length(res$var_names), ncol(X_df_model))
  testthat::expect_true(is.character(res$var_names))
  testthat::expect_identical(res$var_names, colnames(X_df_model))

  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]

  ret <- processClusterLassoInputs(X=x2, y=y, clusters=good_clusters, nlambda=10)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("x", "clusters", "prototypes",
                                           "var_names"))

  # X
  testthat::expect_true(is.matrix(ret$x))
  testthat::expect_true(all(!is.na(ret$x)))
  testthat::expect_true(is.numeric(ret$x))
  testthat::expect_equal(ncol(ret$x), 11)
  testthat::expect_equal(nrow(ret$x), 15)
  testthat::expect_true(all(abs(ret$x - x) < 10^(-9)))

  # var_names
  testthat::expect_equal(length(ret$var_names), ncol(x2))
  testthat::expect_true(is.character(ret$var_names))
  testthat::expect_identical(ret$var_names, LETTERS[1:11])
  
  # Bad inputs
  testthat::expect_error(processClusterLassoInputs(X="x", y=y[1:10],
                                                   clusters=good_clusters,
                                                   nlambda=10),
                         "is.matrix(X) | is.data.frame(X) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y[1:10],
                                                   clusters=good_clusters,
                                                   nlambda=10),
                         "n == length(y) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=list(1:4, 4:6),
                                                   nlambda=10),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=list(2:3, 2:3),
                                                   nlambda=10),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=list(1:4,
                                                                 as.integer(NA)),
                                                   nlambda=10),
                         "!is.na(clusters) are not all TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=list(2:3,
                                                                 c(4, 4, 5)),
                                                   nlambda=10),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=good_clusters,
                                                   nlambda=1),
                         "nlambda >= 2 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=good_clusters,
                                                   nlambda=x),
                         "length(nlambda) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=good_clusters,
                                                   nlambda="nlambda"),
                         "is.numeric(nlambda) | is.integer(nlambda) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(processClusterLassoInputs(X=x, y=y,
                                                   clusters=good_clusters,
                                                   nlambda=10.5),
                         "nlambda == round(nlambda) is not TRUE",
                         fixed=TRUE)
  
})
```

```
## Test passed 🥇
```

`getXglmnet()`:


```r
#' Converts the provided design matrix to an appropriate format for either the
#' protolasso or the cluster representative lasso.
#'
#' Creates design matrix for glmnet by dealing with clusters (for
#' type="protolasso", discards all cluster members except prototype; for
#' type="clusterRepLasso", replaces all cluster members with a simple
#' average of all the cluster members).
#' @param x A numeric matrix; the provided matrix with n observations and p
#' features.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters should
#' be equal to the number of clusters.) All identified clusters should be
#' non-overlapping. All features should appear in exactly one cluster (any
#' unclustered features should be put in their own "cluster" of size 1).
#' @param type Character; "protolasso" for the protolasso or "clusterRepLasso"
#' for the cluster representative lasso.
#' @param prototypes Only required for type "protolasso". An integer vector
#' whose length is equal to the number of clusters. Entry i should be the
#' prototype for cluster i (the feature belonging to cluster i that is most
#' highly correlated with y; see Reid and Tibshirani 2016).
#' @return A numeric matrix; the design matrix as required for the protolasso or
#' cluster representative lasso, prepared for input to glmnet.
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
getXglmnet <- function(x, clusters, type, prototypes=NA){
    
    # Check inputs
    checkGetXglmnetInputs(x, clusters, type, prototypes)

    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:length(clusters)){
        cluster_i <- clusters[[i]]

        if(length(cluster_i) == 1){
            X_glmnet_i <- x[, cluster_i]
        } else{
            stopifnot(length(cluster_i) > 1)
            
            if(type == "protolasso"){
                prototype_ind_i <- which(prototypes %in% cluster_i)
                stopifnot(length(prototype_ind_i) == 1)
                prototype_i <- prototypes[prototype_ind_i]
                X_glmnet_i <- x[, prototype_i]
            } else {
                stopifnot(type == "clusterRepLasso")
                X_glmnet_i <- rowMeans(x[, cluster_i])
            }
        }
        
        stopifnot(length(X_glmnet_i) == n)
        
        if(i == 1){
            X_glmnet <- as.matrix(X_glmnet_i)
        } else{
            X_glmnet <- cbind(X_glmnet, X_glmnet_i)
        }
    }
    
    stopifnot(ncol(X_glmnet) == length(clusters))
    stopifnot(ncol(X_glmnet) == length(clusters))
    colnames(X_glmnet) <- character()

    # Check output
    stopifnot(is.matrix(X_glmnet))
    stopifnot(nrow(X_glmnet) == n)
    stopifnot(ncol(X_glmnet) <= p)
    stopifnot(ncol(X_glmnet) >= 1)
    
    return(X_glmnet)
}
```

`checkGetXglmnetInputs()`:


```r
#' Verifies the inputs for getXglmnet.
#'
#' @param x A numeric matrix; the provided matrix with n observations and p
#' features.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters should
#' be equal to the number of clusters.) All identified clusters should be
#' non-overlapping. All features should appear in exactly one cluster (any
#' unclustered features should be put in their own "cluster" of size 1).
#' @param type Character; "protolasso" for the protolasso or "clusterRepLasso"
#' for the cluster representative lasso.
#' @param prototypes Only required for type "protolasso". An integer vector
#' whose length is equal to the number of clusters. Entry i should be the
#' prototype for cluster i (the feature belonging to cluster i that is most
#' highly correlated with y; see Reid and Tibshirani 2016).
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
checkGetXglmnetInputs <- function(x, clusters, type, prototypes){
    stopifnot(is.matrix(x))

    stopifnot(is.list(clusters))
    stopifnot(all(lengths(clusters) >= 1))

    stopifnot(length(type) == 1)
    stopifnot(is.character(type))
    stopifnot(!is.na(type))
    stopifnot(type %in% c("protolasso", "clusterRepLasso"))

    if(type=="protolasso"){
        stopifnot(!is.na(prototypes))
        stopifnot(is.integer(prototypes))
        stopifnot(all(!is.na(prototypes)))
        stopifnot(length(prototypes) == length(unique(prototypes)))
        stopifnot(all(prototypes %in% 1:ncol(x)))
    }
    for(i in 1:length(clusters)){
        cluster_i <- clusters[[i]]
        if(length(cluster_i) > 1){
            stopifnot(sum(prototypes %in% cluster_i) == 1)
        }
    }
}
```

Tests for `checkGetXglmnetInputs()`:



```r
testthat::test_that("checkGetXglmnetInputs works", {
  set.seed(82612)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  process <- processClusterLassoInputs(X=x, y=y, clusters=good_clusters,
                                       nlambda=10)

  checkGetXglmnetInputs(x=process$x, clusters=process$clusters,
                               type="protolasso", prototypes=process$prototypes)
  
  checkGetXglmnetInputs(x=process$x, clusters=process$clusters,
                               type="clusterRepLasso",
                        prototypes=process$prototypes)
  
  # X as a data.frame
  X_df <- datasets::mtcars
  res <- processClusterLassoInputs(X=X_df, y=stats::rnorm(nrow(X_df)),
                                   clusters=1:3, nlambda=10)
  
  checkGetXglmnetInputs(x=res$x, clusters=res$clusters, type="clusterRepLasso",
                        prototypes=res$prototypes)
  
  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=1:3, nlambda=10)
  
  checkGetXglmnetInputs(x=res$x, clusters=res$clusters, type="protolasso",
                        prototypes=res$prototypes)

  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]

  ret <- processClusterLassoInputs(X=x2, y=y, clusters=good_clusters, nlambda=10)

  checkGetXglmnetInputs(x=ret$x, clusters=ret$clusters, type="clusterRepLasso",
                        prototypes=ret$prototypes)

  # Bad prototype inputs
  # Error has quotation marks
  testthat::expect_error(checkGetXglmnetInputs(x=process$x,
                                               clusters=process$clusters,
                                               type="clsterRepLasso",
                                               prototypes=process$prototypes))

  testthat::expect_error(checkGetXglmnetInputs(x=process$x,
                                               clusters=process$clusters,
                                               type=c("clusterRepLasso",
                                                      "protolasso"),
                                               prototypes=process$prototypes),
                         "length(type) == 1 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetXglmnetInputs(x=process$x,
                                               clusters=process$clusters,
                                               type=2,
                                               prototypes=process$prototypes),
                         "is.character(type) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGetXglmnetInputs(x=process$x,
                                               clusters=process$clusters,
                                               type=as.character(NA),
                                               prototypes=process$prototypes),
                         "!is.na(type) is not TRUE",
                         fixed=TRUE)
  
})
```

```
## Test passed 🥇
```

Tests for `getXglmnet()`:


```r
testthat::test_that("getXglmnet works", {
  set.seed(82612)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  process <- processClusterLassoInputs(X=x, y=y, clusters=good_clusters,
                                       nlambda=10)

  res <- getXglmnet(x=process$x, clusters=process$clusters,
                               type="protolasso", prototypes=process$prototypes)
  
  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_true(is.null(colnames(res)))
  testthat::expect_true(nrow(res) == 15)
  # Each column of res should be one of the prototypes. Features 9 - 11 are
  # in clusters by themselves and are therefore their own prototypes.
  testthat::expect_true(ncol(res) == 5)
  for(i in 1:length(good_clusters)){
    proto_i_found <- FALSE
    cluster_i <- good_clusters[[i]]
    for(j in 1:length(cluster_i)){
      proto_i_found <- proto_i_found | all(abs(res[, i] - x[, cluster_i[j]]) <
                                             10^(-9))
    }
    testthat::expect_true(proto_i_found)
  }
  testthat::expect_true(all(abs(res[, 3] - x[, 9]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 4] - x[, 10]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 5] - x[, 11]) < 10^(-9)))
  
  res <- getXglmnet(x=process$x, clusters=process$clusters,
                    type="clusterRepLasso", prototypes=process$prototypes)
  
  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_true(is.null(colnames(res)))
  testthat::expect_true(nrow(res) == 15)
  # Each column of res should be one of the cluster representatives. Features 9
  # - 11 are in clusters by themselves and are therefore their own cluster
  # representatives.
  testthat::expect_true(ncol(res) == 5)
  for(i in 1:length(good_clusters)){
    cluster_i <- good_clusters[[i]]
    clus_rep_i <- rowMeans(x[, cluster_i])
    testthat::expect_true(all(abs(res[, i] - clus_rep_i) <
                                             10^(-9)))
  }
  testthat::expect_true(all(abs(res[, 3] - x[, 9]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 4] - x[, 10]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 5] - x[, 11]) < 10^(-9)))
  
  # X as a data.frame
  X_df <- datasets::mtcars
  res <- processClusterLassoInputs(X=X_df, y=stats::rnorm(nrow(X_df)),
                                   clusters=1:3, nlambda=10)

  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="protolasso",
                       prototypes=res$prototypes)
  
  X_df_model <- stats::model.matrix(~ ., X_df)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model) - 3 + 1)

  proto_found <- FALSE
  for(j in 1:3){
    proto_found <- proto_found | all(abs(ret_df[, 1] - X_df_model[, j]) < 10^(-9))
  }
  testthat::expect_true(proto_found)

  for(j in 4:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j - 2] - X_df_model[, j]) < 10^(-9)))
  }
  
  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="clusterRepLasso",
                       prototypes=res$prototypes)
  
  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model) - 3 + 1)

  proto_found <- FALSE
  clus_rep <- rowMeans(X_df_model[, 1:3])
  testthat::expect_true(all(abs(ret_df[, 1] - clus_rep) < 10^(-9)))

  for(j in 4:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j - 2] - X_df_model[, j]) < 10^(-9)))
  }

  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=1:3, nlambda=10)
  
  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="protolasso",
                       prototypes=res$prototypes)
  
  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model) - 3 + 1)

  proto_found <- FALSE
  for(j in 1:3){
    proto_found <- proto_found | all(abs(ret_df[, 1] - X_df_model[, j]) < 10^(-9))
  }
  testthat::expect_true(proto_found)

  for(j in 4:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j - 2] - X_df_model[, j]) < 10^(-9)))
  }
  
  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="clusterRepLasso",
                       prototypes=res$prototypes)
  
  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model) - 3 + 1)

  proto_found <- FALSE
  clus_rep <- rowMeans(X_df_model[, 1:3])
  testthat::expect_true(all(abs(ret_df[, 1] - clus_rep) < 10^(-9)))

  for(j in 4:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j - 2] - X_df_model[, j]) < 10^(-9)))
  }

  # X as a matrix with column names (returned X shouldn't have column names)
  x2 <- x
  colnames(x2) <- LETTERS[1:11]

  process <- processClusterLassoInputs(X=x2, y=y, clusters=good_clusters,
                                       nlambda=10)

  res <- getXglmnet(x=process$x, clusters=process$clusters,
                               type="protolasso", prototypes=process$prototypes)
  
  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_true(is.null(colnames(res)))
  testthat::expect_true(nrow(res) == 15)
  # Each column of res should be one of the prototypes. Features 9 - 11 are
  # in clusters by themselves and are therefore their own prototypes.
  testthat::expect_true(ncol(res) == 5)
  for(i in 1:length(good_clusters)){
    proto_i_found <- FALSE
    cluster_i <- good_clusters[[i]]
    for(j in 1:length(cluster_i)){
      proto_i_found <- proto_i_found | all(abs(res[, i] - x[, cluster_i[j]]) <
                                             10^(-9))
    }
    testthat::expect_true(proto_i_found)
  }
  testthat::expect_true(all(abs(res[, 3] - x[, 9]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 4] - x[, 10]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 5] - x[, 11]) < 10^(-9)))
  
  res <- getXglmnet(x=process$x, clusters=process$clusters,
                    type="clusterRepLasso", prototypes=process$prototypes)
  
  testthat::expect_true(is.matrix(res))
  testthat::expect_true(is.numeric(res))
  testthat::expect_true(is.null(colnames(res)))
  testthat::expect_true(nrow(res) == 15)
  # Each column of res should be one of the cluster representatives. Features 9
  # - 11 are in clusters by themselves and are therefore their own cluster
  # representatives.
  testthat::expect_true(ncol(res) == 5)
  for(i in 1:length(good_clusters)){
    cluster_i <- good_clusters[[i]]
    clus_rep_i <- rowMeans(x[, cluster_i])
    testthat::expect_true(all(abs(res[, i] - clus_rep_i) <
                                             10^(-9)))
  }
  testthat::expect_true(all(abs(res[, 3] - x[, 9]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 4] - x[, 10]) < 10^(-9)))
  testthat::expect_true(all(abs(res[, 5] - x[, 11]) < 10^(-9)))

  # Bad prototype inputs
  # Error has quotation marks
  testthat::expect_error(getXglmnet(x=process$x, clusters=process$clusters,
                                    type="clsterRepLasso",
                                    prototypes=process$prototypes))

  testthat::expect_error(getXglmnet(x=process$x, clusters=process$clusters,
                                    type=c("clusterRepLasso", "protolasso"),
                                    prototypes=process$prototypes),
                         "length(type) == 1 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getXglmnet(x=process$x, clusters=process$clusters,
                                    type=2, prototypes=process$prototypes),
                         "is.character(type) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(getXglmnet(x=process$x, clusters=process$clusters,
                                    type=as.character(NA),
                                    prototypes=process$prototypes),
                         "!is.na(type) is not TRUE",
                         fixed=TRUE)
  
})
```

```
## Test passed 🥇
```


`getClusterSelsFromGlmnet()`:


```r
#' Extracts selected clusters and cluster prototypes from the glmnet lasso
#' output
#'
#' @param lasso_sets A list of integer vectors. Each vector represents a set of
#' features selected by the lasso for a given value of the penalty parameter
#' lambda.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features must be in their own "cluster" of size 1).
#' @param prototypes An integer vector whose length must be equal to the number
#' of clusters. Entry i should be the index of the feature belonging to cluster
#' i that is most highly correlated with y (that is, the prototype for the
#' cluster, as in the protolasso; see Reid and Tibshirani 2016).
#' @param feat_names Character vector; the names of the features in X. (If the
#' X provided to protolasso or clusterRepLasso did not have feature names,
#' feat_names will be NA.)
#' @return A list containing the following items: \item{selected_sets}{A list of
#' integer vectors. Entry k of this list contains a selected set of size k
#' yielded by glmnet--each member of the set is the index of a single feature
#' from a cluster selected by either the protolasso or the cluster
#' representative lasso (the prototype from that cluster--the cluster member
#' most highly correlated with y). (If no set of size k was selected, entry k
#' will be NULL.)} \item{selected_clusts_list}{A list of lists; entry k of this
#' list is a list of length k of clusters (the clusters that were selected by
#' the cluster representative lasso). Again, if no set of size k was selected,
#' entry k will be NULL.}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}. \cr Bühlmann, P.,
#' Rütimann, P., van de Geer, S., & Zhang, C. H. (2013). Correlated variables in
#' regression: Clustering and sparse estimation.
#' \emph{Journal of Statistical Planning and Inference}, 143(11), 1835–1858.
#' \url{https://doi.org/10.1016/j.jspi.2013.05.019}.
getClusterSelsFromGlmnet <- function(lasso_sets, clusters, prototypes,
    feat_names){

    if(any(!is.na(feat_names))){
        stopifnot(all(!is.na(feat_names)))
    }

    # Largest selected set among all those in lasso_sets
    max_length <- max(vapply(lasso_sets, length, integer(1)))

    # Preparing lists to store 
    selected_sets <- list()
    selected_clusts_list <- list()
    
    for(j in 1:max_length){
        # Lasso selected set of size j
        lasso_sets_j <- lasso_sets[lapply(lasso_sets, length) == j]
        # Are there any lasso selected sets of size j? (If not, we will skip to
        # the next j, and slot j in the list will be empty.)
        if(length(lasso_sets_j) > 0){

            # Select the first set of size j
            lasso_set_j <- lasso_sets_j[[1]]
            stopifnot(length(lasso_set_j) == j)
            
            ret <- getSelectedSets(lasso_set=lasso_set_j, clusters=clusters,
                prototypes=prototypes, feat_names=feat_names)

            selected_sets[[j]] <- ret$selected_set
            selected_clusts_list[[j]] <- ret$selected_clusts_list

            rm(ret)
        }
    }

    stopifnot(length(selected_sets) <= max_length)
    stopifnot(length(selected_clusts_list) <= max_length)

    return(list(selected_sets=selected_sets,
        selected_clusts_list=selected_clusts_list))
}
```

`getSelectedSets()`:


```r
#' Converts a selected set from X_glmnet to selected sets and selected clusters
#' from the original feature space of X.
#'
#' @param lasso_set A vector containing the indices of selected cluster
#' representatives or prototypes.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features must be in their own "cluster" of size 1).
#' @param prototypes An integer vector whose length must be equal to the number
#' of clusters. Entry i should be the index of the feature belonging to cluster
#' i that is most highly correlated with y (that is, the prototype for the
#' cluster, as in the protolasso).
#' @param feat_names Character vector; the names of the features in X.
#' @return A list containing two items: \item{selected_set}{An integer vector
#' with length equal to lasso_set containing a set of selected features in the
#' original X matrix. (Selections in lasso_set corresponding to a cluster will
#' be replaced by the cluster's prototype from X.)}
#' \item{selected_clusts_list}{A named list of integer vectors with length equal
#' to selected_set. selected_clusts_list[[k]] will be an integer vector
#' containing the indices of the features in X that are in the cluster
#' containing prototype selected_set[k].}
#' @author Gregory Faletto, Jacob Bien
getSelectedSets <- function(lasso_set, clusters, prototypes, feat_names){
    
    model_size <- length(lasso_set)
    stopifnot(model_size > 0)

    stopifnot(length(unique(lasso_set)) == model_size)
    stopifnot(all(lasso_set <= length(clusters)))

    selected_set <- integer()
    selected_clusts_list <- list()
    # Recover features from original feature space
    for(k in 1:model_size){
        selected_cluster_k <- clusters[[lasso_set[k]]]
        stopifnot(is.integer(selected_cluster_k))
        selected_clusts_list[[k]] <- selected_cluster_k

        if(length(selected_cluster_k) == 1){
            stopifnot(!(selected_cluster_k %in% selected_set))
            selected_set <- c(selected_set, selected_cluster_k)
        } else{
            sel_prototype <- which(prototypes %in% selected_cluster_k)
            stopifnot(length(sel_prototype) == 1)
            stopifnot(!(prototypes[sel_prototype] %in% selected_set))
            selected_set <- c(selected_set, prototypes[sel_prototype])
        }
    }

    stopifnot(length(selected_set) == model_size)
    stopifnot(length(unique(selected_set)) == model_size)
    
    if(any(!is.na(feat_names))){
        names(selected_set) <- feat_names[selected_set]
    }

    stopifnot(length(selected_clusts_list) == model_size)
    all_feats <- unlist(selected_clusts_list)
    stopifnot(length(all_feats) == length(unique(all_feats)))

    return(list(selected_set=selected_set,
        selected_clusts_list=selected_clusts_list))
}
```

Tests for `getSelectedSets()`:


```r
testthat::test_that("getSelectedSets works", {
  set.seed(82612)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  process <- processClusterLassoInputs(X=x, y=y, clusters=good_clusters,
                                       nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[5]]
  
  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))
  
  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))
  
  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:11))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }
  
  # Try again with cluster representative lasso
  
  
  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="clusterRepLasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[5]]
  
  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))
  
  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))
  
  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:11))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }
  

  
  
  
  # X as a data.frame
  X_df <- datasets::mtcars

  X_df_model <- stats::model.matrix(~ ., X_df)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  process <- processClusterLassoInputs(X=X_df, y=rnorm(nrow(X_df)),
                                       clusters=1:3, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=rnorm(nrow(X_df)), family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[min(length(lasso_sets), 3)]]
  
  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))
  
  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))
  
  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }
  
  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=1:3, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="clusterRepLasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=rnorm(nrow(df2)), family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[min(length(lasso_sets), 3)]]
  
  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))
  
  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))
  
  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }


  
  
  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=1:3, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="clusterRepLasso", prototypes=process$prototypes)

  fit <- glmnet::glmnet(x=X_glmnet, y=rnorm(nrow(df2)), family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[min(length(lasso_sets), 3)]]

  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))

  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))

  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }

  
  
  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]

  process <- processClusterLassoInputs(X=x2, y=y,
                                       clusters=good_clusters, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # Pick an arbitrary lasso set
  lasso_set <- lasso_sets[[min(length(lasso_sets), 3)]]
  
  res <- getSelectedSets(lasso_set, process$clusters, process$prototypes,
                         process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_set",
                                           "selected_clusts_list"))
  
  # selected_set
  testthat::expect_true(is.integer(res$selected_set))
  testthat::expect_true(all(!is.na(res$selected_set)))
  testthat::expect_true(all(res$selected_set %in% process$prototypes))
  
  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  testthat::expect_equal(length(res$selected_set),
                         length(res$selected_clusts_list))
  sel_feats <- unlist(res$selected_clusts_list)
  testthat::expect_true(all(sel_feats %in% 1:11))
  n_clusts <- length(res$selected_clusts_list)
  for(i in 1:n_clusts){
    clust_i_found <- FALSE
    clust_i <- res$selected_clusts_list[[i]]
    for(j in 1:length(process$clusters)){
      clust_i_found <- clust_i_found | identical(clust_i, process$clusters[[j]])
    }
    testthat::expect_true(clust_i_found)
  }
  
})
```

```
## Test passed 😸
```

Tests for `getClusterSelsFromGlmnet()`:


```r
testthat::test_that("getClusterSelsFromGlmnet works", {
  set.seed(61282)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  process <- processClusterLassoInputs(X=x, y=y, clusters=good_clusters,
                                       nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)
  
  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  
  res <- getClusterSelsFromGlmnet(lasso_sets, process$clusters,
                                  process$prototypes, process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }
  

  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:11))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(process$clusters)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     process$clusters[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }

  # Try again with cluster representative lasso

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="clusterRepLasso", prototypes=process$prototypes)

  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

  res <- getClusterSelsFromGlmnet(lasso_sets, process$clusters,
                                  process$prototypes, process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:11))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(process$clusters)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     process$clusters[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }

  
  
  
  
  # X as a data.frame
  X_df <- datasets::mtcars

  X_df_model <- stats::model.matrix(~ ., X_df)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  process <- processClusterLassoInputs(X=X_df, y=rnorm(nrow(X_df)),
                                       clusters=1:3, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)

  fit <- glmnet::glmnet(x=X_glmnet, y=rnorm(nrow(X_df)), family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

  res <- getClusterSelsFromGlmnet(lasso_sets, process$clusters,
                                  process$prototypes, process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(process$clusters)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     process$clusters[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }

  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=1:3, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="clusterRepLasso", prototypes=process$prototypes)

  fit <- glmnet::glmnet(x=X_glmnet, y=rnorm(nrow(df2)), family="gaussian",
                        nlambda=100)
  
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

  res <- getClusterSelsFromGlmnet(lasso_sets, process$clusters,
                                  process$prototypes, process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(process$clusters)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     process$clusters[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }



  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]

  process <- processClusterLassoInputs(X=x2, y=y,
                                       clusters=good_clusters, nlambda=100)

  X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
                         type="protolasso", prototypes=process$prototypes)

  fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian",
                        nlambda=100)
  lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

  res <- getClusterSelsFromGlmnet(lasso_sets, process$clusters,
                                  process$prototypes, process$var_names)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:11))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(process$clusters)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     process$clusters[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }
  
})
```

```
## Test passed 🥳
```

Finally, tests for `protolasso()`:



```r
testthat::test_that("protolasso works", {
  set.seed(61282)
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  good_clusters <- list(red_cluster=1L:4L, green_cluster=5L:8L)
  
  # Get properly formatted clusters and prototypes for testing
  format_clust_res <- formatClusters(clusters=good_clusters, p=11,
                                     clust_names=names(good_clusters),
                                     get_prototypes=TRUE, x=x, y=y)
  
  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters
  
  res <- protolasso(x, y, good_clusters)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:11))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(clus_formatted)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     clus_formatted[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }

  

  
  
  
  
  # X as a data.frame
  X_df <- datasets::mtcars

  X_df_model <- stats::model.matrix(~ ., X_df)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  y_df <- rnorm(nrow(X_df))
  
  # Get properly formatted clusters and prototypes for testing
  format_clust_res <- formatClusters(clusters=1:3, p=ncol(X_df_model),
                                     get_prototypes=TRUE, x=X_df_model, y=y_df)
  
  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters
  
  res <- protolasso(X_df, y_df, 1:3)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_clusts_list),
                         unique(lengths(res$selected_clusts_list)))

  for(k in 1:length(res$selected_clusts_list)){
    if(!is.null(res$selected_clusts_list[[k]])){
      testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_sets[[k]]),
                             length(res$selected_clusts_list[[k]]))
      testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
      sel_feats <- unlist(res$selected_clusts_list[[k]])
      testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
      testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
      n_clusts <- k
      for(i in 1:n_clusts){
        clust_i_found <- FALSE
        clust_i <- res$selected_clusts_list[[k]][[i]]
        for(j in 1:length(clus_formatted)){
          clust_i_found <- clust_i_found | identical(clust_i,
                                                     clus_formatted[[j]])
        }
        testthat::expect_true(clust_i_found)
      }
    } else{
      testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
    }
  }

  # X as a dataframe with factors (number of columns of final design matrix
  # after one-hot encoding factors won't match number of columns of df2)
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]


  # Get properly formatted clusters and prototypes for testing
  format_clust_res <- formatClusters(clusters=1:3, p=ncol(X_df_model),
                                     get_prototypes=TRUE, x=X_df_model, y=y_df)

  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters

  res <- protolasso(X_df, y_df, 1:3)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))


  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  testthat::expect_identical(lengths(res$selected_sets),
                         unique(lengths(res$selected_sets)))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      # testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      # testthat::expect_equal(length(res$selected_sets[[i]]), i)
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # # selected_clusts_list
  # testthat::expect_true(is.list(res$selected_clusts_list))
  # # Selected models should have one of each size without repetition
  # testthat::expect_identical(lengths(res$selected_clusts_list),
  #                        unique(lengths(res$selected_clusts_list)))
  # 
  # for(k in 1:length(res$selected_clusts_list)){
  #   if(!is.null(res$selected_clusts_list[[k]])){
  #     testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
  #     testthat::expect_equal(length(res$selected_sets[[k]]),
  #                            length(res$selected_clusts_list[[k]]))
  #     testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
  #     sel_feats <- unlist(res$selected_clusts_list[[k]])
  #     testthat::expect_true(all(sel_feats %in% 1:ncol(X_df_model)))
  #     testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
  #     n_clusts <- k
  #     for(i in 1:n_clusts){
  #       clust_i_found <- FALSE
  #       clust_i <- res$selected_clusts_list[[k]][[i]]
  #       for(j in 1:length(clus_formatted)){
  #         clust_i_found <- clust_i_found | identical(clust_i,
  #                                                    clus_formatted[[j]])
  #       }
  #       testthat::expect_true(clust_i_found)
  #     }
  #   } else{
  #     testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
  #   }
  # }
  # 
  # 
  # 
  # # X as a matrix with column names
  # x2 <- x
  # colnames(x2) <- LETTERS[1:11]
  # 
  # process <- processClusterLassoInputs(X=x2, y=y,
  #                                      clusters=good_clusters, nlambda=100)
  # 
  # X_glmnet <- getXglmnet(x=process$x, clusters=process$clusters,
  #                        type="protolasso", prototypes=process$prototypes)
  # 
  # fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian",
  #                       nlambda=100)
  # lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))
  # 
  # res <- protolasso(lasso_sets, process$clusters,
  #                                 process$prototypes, process$var_names)
  # 
  # testthat::expect_true(is.list(res))
  # testthat::expect_identical(names(res), c("selected_sets",
  #                                          "selected_clusts_list"))
  # 
  # # selected_sets
  # testthat::expect_true(is.list(res$selected_sets))
  # # Selected models should have one of each size without repetition
  # testthat::expect_identical(lengths(res$selected_sets),
  #                        unique(lengths(res$selected_sets)))
  # for(i in 1:length(res$selected_sets)){
  #   if(!is.null(res$selected_sets[[i]])){
  #     testthat::expect_true(is.integer(res$selected_sets[[i]]))
  #     testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
  #     testthat::expect_true(all(res$selected_sets[[i]] %in% process$prototypes))
  #     testthat::expect_equal(length(res$selected_sets[[i]]), i)
  #   } else{
  #     testthat::expect_true(is.null(res$selected_sets[[i]]))
  #   }
  # }
  # 
  # 
  # # selected_clusts_list
  # testthat::expect_true(is.list(res$selected_clusts_list))
  # # Selected models should have one of each size without repetition
  # testthat::expect_identical(lengths(res$selected_clusts_list),
  #                        unique(lengths(res$selected_clusts_list)))
  # 
  # for(k in 1:length(res$selected_clusts_list)){
  #   if(!is.null(res$selected_clusts_list[[k]])){
  #     testthat::expect_true(is.list(res$selected_clusts_list[[k]]))
  #     testthat::expect_equal(length(res$selected_sets[[k]]),
  #                            length(res$selected_clusts_list[[k]]))
  #     testthat::expect_equal(length(res$selected_clusts_list[[k]]), k)
  #     sel_feats <- unlist(res$selected_clusts_list[[k]])
  #     testthat::expect_true(all(sel_feats %in% 1:11))
  #     testthat::expect_equal(length(sel_feats), length(unique(sel_feats)))
  #     n_clusts <- k
  #     for(i in 1:n_clusts){
  #       clust_i_found <- FALSE
  #       clust_i <- res$selected_clusts_list[[k]][[i]]
  #       for(j in 1:length(process$clusters)){
  #         clust_i_found <- clust_i_found | identical(clust_i,
  #                                                    process$clusters[[j]])
  #       }
  #       testthat::expect_true(clust_i_found)
  #     }
  #   } else{
  #     testthat::expect_true(is.null(res$selected_clusts_list[[k]]))
  #   }
  # }
  
  
  
  
  # Test for names of features, clusters
  
  # Bad inputs
  
})
```

```
## Test passed 🥇
```


`clusterRepLasso()`:


```r
#' Select features via the cluster representative lasso (Bühlmann et. al. 2013)
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors
#' @param y The response; A length n numeric (or integer) real-valued vector.
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves).
#' Default is list() (so no clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the cluster representative lasso. Default is 100 (following the default
#' for glmnet). For now, nlambda must be at least 2 (using a single lambda is
#' not supported).
#' @return A list with three elements. \item{selected_sets}{A list of integer
#' vectors. Entry k of this list contains a selected set (an integer vector) of
#' size k yielded by the lasso--each member of the set is the index of a single
#' feature from a cluster selected by the cluster representative lasso (the
#' prototype from that cluster--the cluster member most highly correlated with
#' y). (If no set of size k was selected, entry k will be empty.)}
#' \item{selected_clusts_list}{A list; each element of the list is a named list
#' of selected clusters. (That is, if a selected set of size k was yielded by
#' the cluster representative lasso, then selected_clusts_list[[k]] is a named
#' list of length k, where each member of the list is an integer vector
#' of cluster members. Note that selected_clusts_lists[[k]][[j]] will be the
#' cluster that contains feature selected_sets[[k]][j].)} \item{beta}{The beta
#' output from glmnet when the lasso was estimated on a matrix of prototypes.
#' (See documentation for the function glmnet from the glmnet package for
#' details.)}
#' @references Bühlmann, P., Rütimann, P., van de Geer, S., & Zhang, C. H.
#' (2013). Correlated variables in regression: Clustering and sparse estimation.
#' \emph{Journal of Statistical Planning and Inference}, 143(11), 1835–1858.
#' \url{https://doi.org/10.1016/j.jspi.2013.05.019}. \cr Jerome Friedman, Trevor
#' Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear
#' Models via Coordinate Descent. \emph{Journal of Statistical Software}, 33(1)
#' ' 1-22. URL \url{https://www.jstatsoft.org/v33/i01/}.
clusterRepLasso <- function(X, y, clusters=list(), nlambda=100){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the cluster
    # representative lasso procedure
    X_glmnet <- getXglmnet(x, clusters, type="clusterRepLasso")

    # Estimate the lasso on the cluster representatives
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

    # Finally, extract the desired information from the lasso fit--all the
    # sets of selected clusters (one for each observed model size), and
    # corresponding sets of selected features
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}
```

Tests for `clusterRepLasso()`:

# Documenting the package and adding README

We finish by running commands that will document, build, and install the package.  It may also be a good idea to check the package from within this file.


```r
litr::document() # <-- use instead of devtools::document()
```

```
## ℹ Updating cssr documentation
## ℹ Loading cssr
## Writing ']8;;file:///Users/gregfaletto/Documents/GitHub/cssr-project/cssr/NAMESPACENAMESPACE]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkB')checkB.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkClusters')checkClusters.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkCssClustersInput')checkCssClustersInput.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkCssInputs')checkCssInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkCssLassoInputs')checkCssLassoInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkCssLoopOutput')checkCssLoopOutput.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkCutoff')checkCutoff.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkFormCssDesignInputs')checkFormCssDesignInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkFormatClustersInput')checkFormatClustersInput.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkGenClusteredDataInputs')checkGenClusteredDataInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkGetClusterSelMatrixInput')checkGetClusterSelMatrixInput.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkGetCssPredsInputs')checkGetCssPredsInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkGetSelectedClustersOutput')checkGetSelectedClustersOutput.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkGetXglmnetInputs')checkGetXglmnetInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkMaxNumClusts')checkMaxNumClusts.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkMinNumClusts')checkMinNumClusts.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkNewXProvided')checkNewXProvided.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkPropFeatsRemove')checkPropFeatsRemove.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkSamplingType')checkSamplingType.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkSelectedClusters')checkSelectedClusters.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkWeighting')checkWeighting.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkXInputResults')checkXInputResults.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('checkY')checkY.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('clusterRepLasso')clusterRepLasso.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('corFunction')corFunction.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('createSubsamples')createSubsamples.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('css')css.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('cssLasso')cssLasso.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('cssLoop')cssLoop.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('cssPredict')cssPredict.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('cssSelect')cssSelect.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('cssr-package')cssr-package.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('formCssDesign')formCssDesign.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('formatClusters')formatClusters.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('genClusteredData')genClusteredData.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('genMuXZSd')genMuXZSd.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getAllClustWeights')getAllClustWeights.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getClustWeights')getClustWeights.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getClusterSelMatrix')getClusterSelMatrix.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getClusterSelsFromGlmnet')getClusterSelsFromGlmnet.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getCssDesign')getCssDesign.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getCssPreds')getCssPreds.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getCssSelections')getCssSelections.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getLassoLambda')getLassoLambda.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getModelSize')getModelSize.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getPrototypes')getPrototypes.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getSelMatrix')getSelMatrix.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getSelectedClusters')getSelectedClusters.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getSelectedSets')getSelectedSets.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getSelectionPrototypes')getSelectionPrototypes.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getSubsamps')getSubsamps.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('getXglmnet')getXglmnet.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('identifyPrototype')identifyPrototype.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('makeCoefficients')makeCoefficients.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('makeCovarianceMatrix')makeCovarianceMatrix.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('print.cssr')print.cssr.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('printCssDf')printCssDf.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('processClusterLassoInputs')processClusterLassoInputs.Rd]8;;'
## Writing ']8;;ide:run:pkgload::dev_help('protolasso')protolasso.Rd]8;;'
```

## Add README


```r
litr::add_readme("../source-files/README.Rmd")
```

```
## ✔ Writing 'README.Rmd'
## ✔ Adding '^README\\.Rmd$' to '.Rbuildignore'
## ✔ Creating '.git/hooks/'
## ✔ Writing '.git/hooks/pre-commit'
```


<!--chapter:end:index.Rmd-->
