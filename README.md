# Cluster stability selection

<!-- badges: start -->
<!-- badges: end -->

## For those interested in using cssr

Please see [here](https://jacobbien.github.io/cssr-project) for the `cssr` website. This includes
all the information needed for those interested in using the package.

## For those interested in the cssr code

`cssr` was defined using literate programming with [litr](https://jacobbien.github.io/litr-project/). Those interested in modifying the code should edit [this Rmd file](index.Rmd). Those
interested in reading the code can do so [here](https://jacobbien.github.io/cssr-project/create).

After modifying the code, the following should be run from the R console:

``` r
litr::render("index.Rmd")
fs::dir_copy("_book", "docs/create", overwrite = TRUE)
fs::dir_delete("_book")
```

The first line creates the `cssr` R package, website, and bookdown.  The next two lines move the bookdown to a directory where github will render it nicely.