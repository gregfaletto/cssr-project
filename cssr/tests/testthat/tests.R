# Generated from _main.Rmd: do not edit by hand  
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
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
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
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
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
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
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
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
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
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
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
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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

# TODO(gregfaletto): deal with the fact that clusters argument doesn't work
# for a data.frame input that has a categorical random variable with more than
# two levels (because then p, and the numbering of the features, changes)
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
  
  res <- protolasso(x, y, good_clusters, nlambda=60)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(is.null(names(res$selected_sets[[i]])))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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

  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == 11 - 8 + 2)
  testthat::expect_true(ncol(res$beta) <= 60)

  
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
  
  res <- protolasso(X_df, y_df, 1:3, nlambda=80)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  colnames(X_df_model)))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  
  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == ncol(X_df_model) - 3 + 1)
  testthat::expect_true(ncol(res$beta) <= 80)

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
  format_clust_res <- formatClusters(clusters=4:6, p=ncol(X_df_model),
                                     get_prototypes=TRUE, x=X_df_model, y=y_df)

  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters

  res <- protolasso(X_df_model, y_df, 4:6, nlambda=70)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))


  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  colnames(X_df_model)))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  
  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == ncol(X_df_model) - 3 + 1)
  testthat::expect_true(ncol(res$beta) <= 70)



  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]


  # Get properly formatted clusters and prototypes for testing
  format_clust_res <- formatClusters(clusters=good_clusters, p=11,
                                     clust_names=names(good_clusters),
                                     get_prototypes=TRUE, x=x2, y=y)
  
  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters
  
  res <- protolasso(x2, y, good_clusters, nlambda=50)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))
  
  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  LETTERS[1:11]))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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
  
  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == 11 - 8 + 2)
  testthat::expect_true(ncol(res$beta) <= 50)
  
  # Bad inputs
  testthat::expect_error(protolasso(X="x", y=y[1:10], clusters=good_clusters,
                                    nlambda=10),
                         "is.matrix(X) | is.data.frame(X) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y[1:10], clusters=good_clusters,
                                    nlambda=10),
                         "n == length(y) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=list(1:4, 4:6),
                                    nlambda=10),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.", fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=list(2:3, 2:3),
                                    nlambda=10),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y,
                                    clusters=list(1:4, as.integer(NA)),
                                    nlambda=10),
                         "!is.na(clusters) are not all TRUE", fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=list(2:3, c(4, 4, 5)),
                                    nlambda=10),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=1), "nlambda >= 2 is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=x),
                         "length(nlambda) == 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=good_clusters,
                                    nlambda="nlambda"),
                         "is.numeric(nlambda) | is.integer(nlambda) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(protolasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=10.5),
                         "nlambda == round(nlambda) is not TRUE",
                         fixed=TRUE)
  
})

# TODO(gregfaletto): deal with the fact that clusters argument doesn't work
# for a data.frame input that has a categorical random variable with more than
# two levels (because then p, and the numbering of the features, changes)
testthat::test_that("clusterRepLasso works", {
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
  
  res <- clusterRepLasso(x, y, good_clusters, nlambda=60)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(is.null(names(res$selected_sets[[i]])))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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

  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == 11 - 8 + 2)
  testthat::expect_true(ncol(res$beta) <= 60)


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

  res <- clusterRepLasso(X_df, y_df, 1:3, nlambda=80)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  colnames(X_df_model)))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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

  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == ncol(X_df_model) - 3 + 1)
  testthat::expect_true(ncol(res$beta) <= 80)

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
  format_clust_res <- formatClusters(clusters=4:6, p=ncol(X_df_model),
                                     get_prototypes=TRUE, x=X_df_model, y=y_df)

  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters

  res <- clusterRepLasso(X_df_model, y_df, 4:6, nlambda=70)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))


  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  colnames(X_df_model)))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))
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

  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == ncol(X_df_model) - 3 + 1)
  testthat::expect_true(ncol(res$beta) <= 70)



  # X as a matrix with column names
  x2 <- x
  colnames(x2) <- LETTERS[1:11]


  # Get properly formatted clusters and prototypes for testing
  format_clust_res <- formatClusters(clusters=good_clusters, p=11,
                                     clust_names=names(good_clusters),
                                     get_prototypes=TRUE, x=x2, y=y)

  prototypes <- format_clust_res$prototypes
  clus_formatted <- format_clust_res$clusters

  res <- clusterRepLasso(x2, y, good_clusters, nlambda=50)

  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

  # selected_sets
  testthat::expect_true(is.list(res$selected_sets))
  # Selected models should have one of each size without repetition
  lengths <- lengths(res$selected_sets)
  lengths <- lengths[lengths != 0]
  testthat::expect_identical(lengths, unique(lengths))
  for(i in 1:length(res$selected_sets)){
    if(!is.null(res$selected_sets[[i]])){
      testthat::expect_true(is.integer(res$selected_sets[[i]]))
      testthat::expect_true(all(!is.na(res$selected_sets[[i]])))
      testthat::expect_true(all(res$selected_sets[[i]] %in% prototypes))
      testthat::expect_equal(length(res$selected_sets[[i]]), i)
      testthat::expect_true(all(names(res$selected_sets[[i]]) %in%
                                  LETTERS[1:11]))
    } else{
      testthat::expect_true(is.null(res$selected_sets[[i]]))
    }
  }


  # selected_clusts_list
  testthat::expect_true(is.list(res$selected_clusts_list))
  # Selected models should have one of each size without repetition
  clust_lengths <- lengths(res$selected_clusts_list)
  clust_lengths <- clust_lengths[clust_lengths != 0]
  testthat::expect_identical(clust_lengths, unique(clust_lengths))

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

  # beta
  testthat::expect_true(grepl("dgCMatrix", class(res$beta)))
  testthat::expect_true(nrow(res$beta) == 11 - 8 + 2)
  testthat::expect_true(ncol(res$beta) <= 50)

  # Bad inputs
  testthat::expect_error(clusterRepLasso(X="x", y=y[1:10], clusters=good_clusters,
                                    nlambda=10),
                         "is.matrix(X) | is.data.frame(X) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y[1:10], clusters=good_clusters,
                                    nlambda=10),
                         "n == length(y) is not TRUE", fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=list(1:4, 4:6),
                                    nlambda=10),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.", fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=list(2:3, 2:3),
                                    nlambda=10),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y,
                                    clusters=list(1:4, as.integer(NA)),
                                    nlambda=10),
                         "!is.na(clusters) are not all TRUE", fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=list(2:3, c(4, 4, 5)),
                                    nlambda=10),
                         "length(clusters[[i]]) == length(unique(clusters[[i]])) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=1), "nlambda >= 2 is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=x),
                         "length(nlambda) == 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=good_clusters,
                                    nlambda="nlambda"),
                         "is.numeric(nlambda) | is.integer(nlambda) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(clusterRepLasso(X=x, y=y, clusters=good_clusters,
                                    nlambda=10.5),
                         "nlambda == round(nlambda) is not TRUE",
                         fixed=TRUE)
  
})

