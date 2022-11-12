# Generated from create-cssr.Rmd: do not edit by hand  
testthat::test_that("checkY works", {
  testthat::expect_null(checkY(as.numeric(1:20)*.1, 20))
  testthat::expect_null(checkY(as.integer(1:15), 15))
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

testthat::test_that("checkClusters works", {
  good_clusters <- list(c1=as.integer(1:5), c2=as.integer(6:8), c3=9L)
  
  testthat::expect_null(checkClusters(good_clusters, 9))
  testthat::expect_error(checkClusters(good_clusters, 10),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(as.integer(1:10), 10),
                         "is.list(clusters) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=9L,
                                            c4=integer()), 9),
                         "all(lengths(clusters) >= 1) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=9L,
                                            c4=as.integer(NA)), 9),
                         "all(!is.na(clusters)) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=9L,
                                            c2=as.integer(6:8)), 9),
                         "n_clusters == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkClusters(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=10L), 9),
                         "all(all_clustered_feats <= p) is not TRUE",
                         fixed=TRUE)
})

testthat::test_that("identifyPrototype works", {
  testthat::expect_identical(identifyPrototype(10L, "a", 5), 10L)
  n <- 10
  p <- 5
  X <- matrix(stats::rnorm(n*p), nrow=n, ncol=p)
  y <- X[, p]
  testthat::expect_equal(identifyPrototype(as.integer(p), X, y), p)
  testthat::expect_equal(identifyPrototype(2L, X, y), 2)
  # testthat::expect_equal(identifyPrototype(as.integer(2:p), X, y), p)
  # testthat::expect_error(identifyPrototype(as.integer(2:p), y, X),
  #                        "incorrect number of dimensions",
  #                        fixed=TRUE)
})

testthat::test_that("corFunction works", {
  testthat::expect_identical(corFunction(rep(1, 10), 1:10), 0)
  testthat::expect_identical(corFunction(rep(1.2, 5), 1:5), 0)
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

testthat::test_that("getSelMatrix works", {
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

testthat::test_that("cssLasso works", {
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

testthat::test_that("getClusterSelMatrix works", {
  good_clusters <- list(red_cluster=as.integer(1:5),
                        green_cluster=as.integer(6:8), blue_clust=9L)
  
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
  bad_clusters <- list(red_cluster=as.integer(1:5),
                        green_cluster=as.integer(6:7), blue_clust=9L)

  testthat::expect_error(getClusterSelMatrix(bad_clusters, good_res),
                         "length(all_clustered_feats) == p is not TRUE",
                         fixed=TRUE)

  bad_res_entries <- as.integer(sample(c(0, 1, 2), size=2*B*p, replace=TRUE))

  bad_res <- matrix(bad_res_entries, nrow=2*B, ncol=p)

  testthat::expect_error(getClusterSelMatrix(good_clusters, bad_res),
                         "all(res %in% c(0, 1)) is not TRUE",
                         fixed=TRUE)
})

testthat::test_that("css works", {
  
  x <- matrix(stats::rnorm(15*11), nrow=15, ncol=11)
  y <- stats::rnorm(15)
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=as.integer(1:5),
                        green_cluster=as.integer(6:8),
                        c4=as.numeric(10:11))
  
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

  # prop_feats_remove
  testthat::expect_identical(class(css(X=x, y=y, lambda=0.01, B = 10,
                                       prop_feats_remove=0.3)), "cssr")
  # Weirdly high, but still valid, value of prop_feats_remove
  testthat::expect_identical(class(css(X=x, y=y, lambda=0.01, B = 10,
                                       prop_feats_remove=0.9999999999)), "cssr")

})

