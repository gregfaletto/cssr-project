# Cluster stability selection

<!-- badges: start -->
<!-- badges: end -->

## For those interested in using cssr

Please see [here](https://gregfaletto.github.io/cssr-project/) for the `cssr` website. This includes
all the information needed for those interested in using the package.

## For those interested in the cssr code

`cssr` was defined using literate programming with [litr](https://jacobbien.github.io/litr-project/). Those interested in modifying the code should edit [this Rmd file](index.Rmd). Those
interested in reading the code can do so [here](https://gregfaletto.github.io/cssr-project//create).

After modifying the code, run the following from the repo root:

``` sh
Rscript build.R
```

This renders `index.Rmd` (creating the `cssr` R package, website, and bookdown) and republishes the bookdown to `docs/create/`, the directory where github renders it nicely.