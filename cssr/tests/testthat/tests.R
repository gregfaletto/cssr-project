# Generated from create-cssr.Rmd: do not edit by hand  
testthat::test_that("checkCssClustersInput works", {
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=as.integer(1:4),
                        green_cluster=as.integer(5:8))
  
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
  
  unnamed_clusters <- list(as.integer(1:3), as.integer(5:8))
  
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

testthat::test_that("checkFormatClustersInput works", {
  
  # Intentionally don't provide clusters for all feature, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=as.integer(1:4),
                        green_cluster=as.integer(5:8))
  
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
  
  unnamed_clusters <- list(as.integer(1:3), as.integer(5:8))

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

  testthat::expect_identical(getPrototypes(list(as.integer(1:5)), X, y), 5L)

  testthat::expect_identical(getPrototypes(list(1L, as.integer(2:5)), X, y),
                             c(1L, 5L))

  testthat::expect_identical(getPrototypes(list(as.integer(3:5)), X, y), 5L)

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
  good_clusters <- list(red_cluster=as.integer(1:5),
                        green_cluster=as.integer(6:8)
                        # , c4=as.numeric(10:11)
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

  output <- cssLoop(input=as.integer(1:4), x=x, y=y, lambda=0.05,
                    fitfun=cssLasso)

  testthat::expect_true(is.integer(output))

  testthat::expect_equal(length(output), length(unique(output)))

  testthat::expect_true(length(output) <= 8)

  testthat::expect_true(all(output >= 1))

  testthat::expect_true(all(output <= 8))

  testthat::expect_error(cssLoop(input=as.integer(1:6), x=x, y=y, lambda=0.05,
                                 fitfun=cssLasso),
                         "floor(n/2) == length(subsample) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=as.integer(1:4), x=x, y=y[1:8],
                                 lambda=0.05, fitfun=cssLasso),
                         "length(y) == n is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=as.integer(1:4), x=x, y=logical(9),
                                 lambda=0.05, fitfun=cssLasso),
                         "For method cssLasso, y must be a numeric vector.",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=as.integer(1:4), x=x, y=y,
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

  testthat::expect_true(is.integer(cssLoop(input=as.integer(1:4), x=x, y=y,
                                           lambda=TRUE, fitfun=testFitfun)))

  testthat::expect_true(is.integer(cssLoop(input=as.integer(1:4), x=x,
                                           y=character(9), lambda=.05,
                                           fitfun=testFitfun)))

})

testthat::test_that("checkGetClusterSelMatrixInput works", {
  
  good_clusters <- list(happy=as.integer(1:8), sad=as.integer(9:10), med=11L)
  
  res <- matrix(sample(c(0, 1), size=6*11, replace=TRUE), nrow=6, ncol=11)
  
  testthat::expect_null(checkGetClusterSelMatrixInput(good_clusters, res))
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(happy=as.integer(1:8),
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
  
  testthat::expect_error(checkGetClusterSelMatrixInput(as.integer(1:10), res),
                         "is.list(clusters) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=as.integer(1:5),
                                                            c2=as.integer(6:8),
                                                            c3=9L,
                                                            c4=integer()), res),
                         "all(lengths(clusters) >= 1) is not TRUE",
                         fixed=TRUE)
                         
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=9L,
                                            c4=as.integer(NA)), res),
                         "all(!is.na(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=9L,
                                            c2=as.integer(6:8)), res),
                         "n_clusters == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetClusterSelMatrixInput(list(c1=as.integer(1:5),
                                            c2=as.integer(6:8), c3=14L), res),
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
  set.seed(8712)
  
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
  
  sel_feats <- 10:26
  names(sel_feats) <- LETTERS[10:26]
  
  testthat::expect_null(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30))
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=letters[1:4],
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30),
                         "is.numeric(selected_clusts) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=-sel_clusts,
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30),
                         "all(selected_clusts >= 0) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=10*sel_clusts,
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30),
                         "all(selected_clusts <= 1) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=numeric(),
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30),
                         "length(selected_clusts) >= 1 is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats,
                               n_clusters=8, p=30),
                         "length(selected_clusts) <= n_clusters is not TRUE",
                         fixed=TRUE)
  
  bad_clusts <- sel_clusts
  names(bad_clusts) <- rep("a", length(bad_clusts))
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats,
                               n_clusters=10, p=30),
                         "length(names(selected_clusts)) == length(unique(names(selected_clusts))) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=unname(sel_clusts),
                                                       selected_feats=sel_feats,
                                                       n_clusters=10, p=30),
                         "!is.null(names(selected_clusts)) is not TRUE",
                         fixed=TRUE)
  
  bad_clusts <- sel_clusts
  names(bad_clusts)[1] <- ""
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats,
                               n_clusters=10, p=30),
                         "all(!is.na(names(selected_clusts)) & names(selected_clusts) !=  .... is not TRUE",
                         fixed=TRUE)
  
  names(bad_clusts)[1] <- as.character(NA)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=bad_clusts,
                               selected_feats=sel_feats,
                               n_clusters=10, p=30),
                         "all(!is.na(names(selected_clusts)) & names(selected_clusts) !=  .... is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=0.1,
                                                       n_clusters=10, p=30),
                         "is.integer(selected_feats) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                                                       selected_feats=c(1L,
                                                                        rep(2L,
                                                                            2)),
                                                       n_clusters=10, p=30),
                         "length(selected_feats) == length(unique(selected_feats)) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats,
                               n_clusters=10, p=25),
                         "all(selected_feats %in% 1:p) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=sel_clusts,
                               selected_feats=sel_feats[1:8],
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

  # # Name features
  # colnames(x) <- LETTERS[1:ncol(x)]
  # css_res3 <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)
  # res <- getCssSelections(css_res3, weighting="sparse", cutoff=0.05,
  #                            min_num_clusts=1, max_num_clusts=NA)
  # 
  # testthat::expect_true(is.list(res))
  # testthat::expect_equal(length(res), 3)
  # testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
  #                                          "weights"))
  # testthat::expect_true(length(res$selected_clusts) <=
  #                         length(res$selected_feats))
  # 
  # testthat::expect_true(is.numeric(res$selected_clusts))
  # testthat::expect_true(length(res$selected_clusts) >= 1)
  # testthat::expect_equal(length(names(res$selected_clusts)),
  #                        length(res$selected_clusts))
  # testthat::expect_equal(length(names(res$selected_clusts)),
  #                        length(unique(names(res$selected_clusts))))
  # testthat::expect_true(all(res$selected_clusts >= 0))
  # testthat::expect_true(all(res$selected_clusts <= 1))
  # 
  # testthat::expect_true(is.integer(res$selected_feats))
  # testthat::expect_true(length(res$selected_feats) >= 1)
  # testthat::expect_equal(length(names(res$selected_feats)),
  #                        length(unique(names(res$selected_feats))))
  # testthat::expect_true(all(res$selected_feats >= 1))
  # testthat::expect_true(all(res$selected_feats <= 5))
  # testthat::expect_equal(length(res$selected_feats),
  #                            length(unique(res$selected_feats)))
  # testthat::expect_equal(length(names(res$selected_feats)),
  #                        length(res$selected_feats))
  # testthat::expect_equal(length(names(res$selected_feats)),
  #                        length(unique(names(res$selected_feats))))
})

testthat::test_that("checkXInputResults works", {
  set.seed(26717)

  x <- matrix(stats::rnorm(10*5), nrow=10, ncol=5)
  y <- stats::rnorm(10)

  good_clusters <- list("a"=1:2, "b"=3:4, "c"=5)

  res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, fitfun = cssLasso,
    sampling_type = "SS", B = 10, prop_feats_remove = 0, train_inds = integer(),
    num_cores = 1L)

  sel_props <- colMeans(res$feat_sel_mat)

  sel_clusts <- list("a"=1L:2L, "b"=3L:4L)

  # # sparse
  # testthat::expect_identical(getClustWeights(cluster_i=c(3L, 4L, 5L),
  #                                            weighting="sparse",
  #                                            feat_sel_props=sel_props),
  #                            c(0, 0, 1))
  #
  # # weighted_avg
  # cluster=c(1L, 3L, 5L)
  # true_weights <- sel_props[cluster]/sum(sel_props[cluster])
  #
  # testthat::expect_identical(getClustWeights(cluster_i=cluster,
  #                                            weighting="weighted_avg",
  #                                            feat_sel_props=sel_props),
  #                            true_weights)
  #
  # # simple_avg
  # testthat::expect_identical(getClustWeights(cluster_i=c(2L, 3L, 4L, 5L),
  #                                            weighting="simple_avg",
  #                                            feat_sel_props=sel_props),
  #                            rep(0.25, 4))
})

