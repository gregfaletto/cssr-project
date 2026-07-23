# Generated from _main.Rmd: do not edit by hand  
testthat::test_that("coerceDataFrameToMatrix keeps a single-column data.frame as a matrix (#43)", {
  # A 1-column data.frame must coerce to a 1-column matrix, not collapse to a
  # vector (which used to crash with the cryptic "argument is of length zero",
  # both with and without clusters).
  num_df <- data.frame(a = as.numeric(1:8))
  res_clust <- coerceDataFrameToMatrix(num_df, clusters = list(1))
  testthat::expect_true(is.matrix(res_clust))
  testthat::expect_equal(dim(res_clust), c(8L, 1L))
  res_noclust <- coerceDataFrameToMatrix(num_df, clusters = list())
  testthat::expect_true(is.matrix(res_noclust))
  testthat::expect_equal(dim(res_noclust), c(8L, 1L))

  # A matrix passes through unchanged.
  m <- matrix(as.numeric(1:8), nrow = 8, ncol = 1)
  testthat::expect_identical(coerceDataFrameToMatrix(m, clusters = list()), m)

  # The factor-expansion guard still fires (a >= 3-level factor changes ncol).
  fac_df <- data.frame(a = factor(c("x", "y", "z", "x", "y", "z", "x", "y")))
  testthat::expect_error(coerceDataFrameToMatrix(fac_df, clusters = list(1)),
    "the number of columns changed", fixed = TRUE)

  # A non-numeric (character) matrix is rejected (#152).
  testthat::expect_error(
    coerceDataFrameToMatrix(matrix(letters[1:6], nrow = 3, ncol = 2), clusters = list()),
    "must be a numeric matrix", fixed = TRUE)
})

testthat::test_that("non-numeric matrix X is rejected at every entry point (#152)", {
    set.seed(1)
    cx <- matrix(sample(letters, 20, TRUE), 10, 2)   # character matrix
    testthat::expect_error(
        css(X = cx, y = stats::rnorm(10), lambda = 0.01, B = 5),
        "must be a numeric matrix", fixed = TRUE)
    testthat::expect_error(
        protolasso(matrix(sample(letters, 40, TRUE), 20, 2), stats::rnorm(20)),
        "must be a numeric matrix", fixed = TRUE)
    testthat::expect_error(
        clusterRepLasso(matrix(sample(letters, 40, TRUE), 20, 2), stats::rnorm(20)),
        "must be a numeric matrix", fixed = TRUE)
    # getCssPreds(testX = <character matrix>): build a valid css result on
    # numeric data, then pass a character testX of the SAME ncol so it
    # reaches coerceDataFrameToMatrix (not an ncol-mismatch check first).
    nx <- matrix(stats::rnorm(60), 30, 2)
    res <- css(X = nx[1:20, ], y = stats::rnorm(20), lambda = 0.01, B = 5)
    testthat::expect_error(
        getCssPreds(res, testX = matrix(sample(letters, 20, TRUE), 10, 2),
            trainX = nx[21:30, ], trainY = stats::rnorm(10)),
        "must be a numeric matrix", fixed = TRUE)
})

testthat::test_that("checkNoNAs flags NA/NaN/Inf in a matrix or data.frame and passes clean input", {
  m_ok <- matrix(as.numeric(1:8), nrow = 4, ncol = 2)
  testthat::expect_identical(checkNoNAs(m_ok, "X"), m_ok)   # returns input invisibly
  m_na <- m_ok; m_na[2, 1] <- NA
  testthat::expect_error(checkNoNAs(m_na, "X"),
    "must not contain missing", fixed = TRUE)
  # data.frame with a numeric NA is caught too (before any coercion).
  df_na <- data.frame(a = c(1, NA, 3), b = c(4, 5, 6))
  testthat::expect_error(checkNoNAs(df_na, "X"),
    "must not contain missing", fixed = TRUE)
  # arg_name appears in the message.
  testthat::expect_error(checkNoNAs(m_na, "newx"), "newx", fixed = TRUE)

  # Non-finite values are rejected too (#99): is.na(Inf) is FALSE, so the
  # previous bare is.na() check let Inf/-Inf slip through into glmnet and the
  # cluster-representative averaging, silently corrupting results.
  m_inf <- m_ok; m_inf[2, 1] <- Inf
  testthat::expect_error(checkNoNAs(m_inf, "X"), "non-finite", fixed = TRUE)
  m_neginf <- m_ok; m_neginf[3, 2] <- -Inf
  testthat::expect_error(checkNoNAs(m_neginf, "X"), "non-finite", fixed = TRUE)
  # NaN is caught by is.na() (is.na(NaN) is TRUE).
  m_nan <- m_ok; m_nan[1, 1] <- NaN
  testthat::expect_error(checkNoNAs(m_nan, "X"),
    "must not contain missing", fixed = TRUE)
  # data.frame with Inf in a numeric column is rejected too.
  df_inf <- data.frame(a = c(1, Inf, 3), b = c(4, 5, 6))
  testthat::expect_error(checkNoNAs(df_inf, "X"), "non-finite", fixed = TRUE)

  # A finite data.frame passes through unchanged.
  df_ok <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  testthat::expect_identical(checkNoNAs(df_ok, "X"), df_ok)
  # A data.frame with non-numeric (factor/character) columns plus clean numeric
  # columns still passes: the is.infinite() check is per-numeric-column, so it
  # must not false-positive on non-numeric columns.
  df_mixed <- data.frame(
    f = factor(c("x", "y", "z")),
    s = c("a", "b", "c"),
    n = c(1, 2, 3),
    stringsAsFactors = FALSE)
  testthat::expect_identical(checkNoNAs(df_mixed, "X"), df_mixed)
})

testthat::test_that("checkFiniteY accepts finite numeric/integer y and rejects non-finite or non-numeric y (#100)", {
  # Finite numeric and integer y pass, returning the input invisibly.
  y_num <- as.numeric(1:8) * 0.5
  testthat::expect_identical(checkFiniteY(y_num, "y"), y_num)
  y_int <- 1L:8L
  testthat::expect_identical(checkFiniteY(y_int, "y"), y_int)
  testthat::expect_invisible(checkFiniteY(y_num, "y"))

  # NA, NaN, Inf, -Inf each error with the non-finite message.
  testthat::expect_error(checkFiniteY(c(1, NA, 3), "y"),
    "must not contain missing (NA) or non-finite (Inf) values", fixed = TRUE)
  testthat::expect_error(checkFiniteY(c(1, NaN, 3), "y"),
    "must not contain missing (NA) or non-finite (Inf) values", fixed = TRUE)
  testthat::expect_error(checkFiniteY(c(1, Inf, 3), "y"),
    "must not contain missing (NA) or non-finite (Inf) values", fixed = TRUE)
  testthat::expect_error(checkFiniteY(c(1, -Inf, 3), "y"),
    "must not contain missing (NA) or non-finite (Inf) values", fixed = TRUE)

  # A non-numeric (logical/character) y errors with the numeric message.
  testthat::expect_error(checkFiniteY(c(TRUE, FALSE, TRUE), "y"),
    "must be a numeric (real-valued) vector", fixed = TRUE)
  testthat::expect_error(checkFiniteY(c("a", "b"), "y"),
    "must be a numeric (real-valued) vector", fixed = TRUE)

  # arg_name is interpolated into both messages.
  testthat::expect_error(checkFiniteY(c(1, Inf), "y_train_selec"),
    "The provided y_train_selec must not contain missing", fixed = TRUE)
  testthat::expect_error(checkFiniteY(c(TRUE, FALSE), "y_train_selec"),
    "The provided y_train_selec must be a numeric", fixed = TRUE)
})

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
                         "all(is.finite(y)) is not TRUE", fixed=TRUE)
  # Inf is now rejected too (previously all(!is.na(y)) missed it) (#100)
  testthat::expect_error(checkY(c(1, Inf, 3), 3),
                         "all(is.finite(y)) is not TRUE", fixed=TRUE)
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

testthat::test_that("identifyPrototype handles a constant cluster member silently (#59)", {
  # A constant column has undefined correlation; the vectorized cor() path
  # returns 0 for it -- and SILENTLY (base cor() warns "the standard deviation
  # is zero" on a constant column; identifyPrototype now suppresses that to
  # preserve the silent-0 contract).
  n <- 12
  set.seed(7321)
  varying <- stats::rnorm(n)
  # Column 1 is constant; column 2 equals y (perfectly correlated).
  x <- cbind(rep(1.5, n), varying)
  y <- varying
  # The prototype must be the varying member (2), never the constant column (1).
  testthat::expect_identical(identifyPrototype(c(1L, 2L), x, y), 2L)
  # And no "standard deviation is zero" warning may leak.
  testthat::expect_silent(identifyPrototype(c(1L, 2L), x, y))
})

testthat::test_that("identifyPrototype warns on a constant response y (#67)", {
  # Moved from the removed helper's test: a constant y makes every
  # cluster-member correlation undefined, and identifyPrototype warns.
  set.seed(5417)
  X <- matrix(stats::rnorm(8 * 2), nrow = 8, ncol = 2)
  y_const <- rep(2.5, 8)
  testthat::expect_warning(
    identifyPrototype(c(1L, 2L), X, y_const),
    "identifyPrototype: the response y has only one unique value",
    fixed = TRUE)
})

testthat::test_that("identifyPrototype selects by absolute correlation (#67)", {
  # The prototype is the member with the largest ABSOLUTE correlation with y, so
  # a near-perfectly NEGATIVELY correlated member must beat a weak positive one.
  set.seed(6708)
  n <- 20
  y <- stats::rnorm(n)
  X <- cbind(-y, y + stats::rnorm(n, sd = 4))  # col 1: cor=-1; col 2: weak +cor
  # abs() => col 1 (|cor|=1) wins despite its negative sign.
  testthat::expect_identical(identifyPrototype(c(1L, 2L), X, y), 1L)
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

  # Out-of-range cluster index is now caught up front by checkFormatClustersInput
  # (#104, L3) with a message naming the offending index and p, instead of the
  # downstream cryptic length(all_clustered_feats) == p stopifnot.
  testthat::expect_error(formatClusters(list(5:8), p=7),
                         "Cluster index 8 exceeds the number of features (p = 7).",
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

testthat::test_that("checkAlpha validates alpha in (0, 1] (#72)", {
  testthat::expect_null(checkAlpha(1))
  testthat::expect_null(checkAlpha(0.5))
  testthat::expect_error(checkAlpha("0.5"),
                         "is.numeric(alpha) | is.integer(alpha) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkAlpha(c(0.5, 0.6)),
                         "length(alpha) == 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkAlpha(NA_real_), "!is.na(alpha) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(checkAlpha(0), "alpha > 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkAlpha(1.5), "alpha <= 1 is not TRUE", fixed=TRUE)
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

  # Regression test: train_inds with an out-of-range index (> n) must be
  # rejected. Previously the upper-bound check was a no-op due to a misplaced
  # parenthesis (all(train_inds) <= n instead of all(train_inds <= n)), so
  # out-of-range indices were silently accepted. (n = 15 here; 16 is invalid.)
  testthat::expect_error(checkCssInputs(X=x, y=y, lambda=0.01,
                                        clusters=good_clusters,
                                        fitfun = cssLasso, sampling_type = "SS",
                                        B = 13, prop_feats_remove = 0,
                                        train_inds = c(1L, 2L, 16L),
                                        num_cores = 1L))

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

  testthat::expect_error(
    checkCssLoopOutput(selected=c(TRUE, FALSE), p=7, feats_on_subsamp=1:7),
    "returned a logical vector", fixed=TRUE)

  testthat::expect_error(
    checkCssLoopOutput(selected=NULL, p=7, feats_on_subsamp=1:7),
    "returned NULL", fixed=TRUE)

})

testthat::test_that("fitfunFailureMessage surfaces the cause and subsample index (#73)", {
  # try-error path: the wrapped condition message is surfaced (this is what
  # getSelMatrix actually receives from mclapply under forking).
  te <- structure("Error\n", class = "try-error",
    condition = simpleError("fitfun returned a character vector"))
  msg <- fitfunFailureMessage(te, 4L)
  testthat::expect_match(msg, "subsample 4", fixed = TRUE)
  testthat::expect_match(msg, "fitfun returned a character vector", fixed = TRUE)
  # defensive non-try-error fallback: the class is reported.
  msg2 <- fitfunFailureMessage("not an integer vector", 2L)
  testthat::expect_match(msg2, "class character", fixed = TRUE)
  testthat::expect_match(msg2, "subsample 2", fixed = TRUE)
  testthat::expect_match(msg2, "integer vector of selected feature indices",
    fixed = TRUE)
})

testthat::test_that("checkCssLassoInputs works", {
  set.seed(761)
  
  x <- matrix(stats::rnorm(15*4), nrow=15, ncol=4)
  y <- stats::rnorm(15)
  
  testthat::expect_null(checkCssLassoInputs(X=x, y=y, lambda=0.01))
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=logical(15), lambda=0.05),
                         "For method cssLasso, y must be a numeric or integer vector.",
                         fixed=TRUE)

  # Integer y is accepted (is.numeric(1L) is TRUE; the | !is.integer guard
  # makes the validator read uniformly with the other entry points). Regression
  # test for issue #13 -- this is NOT a behavior change, integer y always worked.
  testthat::expect_null(checkCssLassoInputs(X=x, y=as.integer(round(y * 10)),
                                            lambda=0.01))

  testthat::expect_error(checkCssLassoInputs(X=x[1:13, ], y=y, lambda=0.01),
                         "For method cssLasso, y must be a vector of length equal to nrow(X).",
                         fixed=TRUE)
  
  testthat::expect_error(checkCssLassoInputs(X=x, y=rep(1.2, 15), lambda=0.05),
                         "Subsample with only one unique value of y detected: for the default cssLasso, every subsample of y (of size floor(n/2)) must have more than one unique value. css draws random subsamples, so this abort is seed-dependent and becomes more likely as B grows. It indicates that y is too discrete for the default cssLasso--supply a less discrete (more continuous) response, or pass a custom fitfun that tolerates a constant-y subsample.",
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
                         "For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).",
                         fixed=TRUE)

  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=numeric()),
                         "For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).",
                         fixed=TRUE)

  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=-0.01),
                         "For method cssLasso, lambda must be nonnegative.",
                         fixed=TRUE)

  # Bundled c(lambda=, alpha=) form (elastic net)
  testthat::expect_null(checkCssLassoInputs(X=x, y=y,
                                            lambda=c(lambda=0.1, alpha=0.5)))

  # Order-insensitive (setequal on names)
  testthat::expect_null(checkCssLassoInputs(X=x, y=y,
                                            lambda=c(alpha=0.5, lambda=0.1)))

  # alpha at the boundary 1 is allowed
  testthat::expect_null(checkCssLassoInputs(X=x, y=y,
                                            lambda=c(lambda=0.1, alpha=1)))

  # Unnamed length-2 vector is rejected
  testthat::expect_error(checkCssLassoInputs(X=x, y=y, lambda=c(0.1, 0.5)),
                         "For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).",
                         fixed=TRUE)

  # alpha = 0 (degenerate ridge) is rejected
  testthat::expect_error(checkCssLassoInputs(X=x, y=y,
                                             lambda=c(lambda=0.1, alpha=0)),
                         "For method cssLasso, the alpha component of lambda must be in (0, 1].",
                         fixed=TRUE)

  # alpha > 1 is rejected
  testthat::expect_error(checkCssLassoInputs(X=x, y=y,
                                             lambda=c(lambda=0.1, alpha=1.2)),
                         "For method cssLasso, the alpha component of lambda must be in (0, 1].",
                         fixed=TRUE)

  # Length-3 vector is rejected
  testthat::expect_error(checkCssLassoInputs(X=x, y=y,
                                             lambda=c(lambda=0.1, alpha=0.5,
                                                      extra=0.3)),
                         "For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).",
                         fixed=TRUE)

})

testthat::test_that("cssLasso alpha (bundled in lambda) drives selection", {
  # Correlated-block design: features 1-4 are relevant and share a common
  # latent factor at correlation rho; features 5-20 are irrelevant noise.
  set.seed(1)
  n <- 100
  p <- 20
  rho <- 0.9
  block <- 4
  Z <- matrix(stats::rnorm(n*p), n, p)
  common <- stats::rnorm(n)
  for(j in 1:block){
    Z[, j] <- sqrt(rho)*common + sqrt(1 - rho)*Z[, j]
  }
  y <- as.numeric(Z %*% c(rep(1, block), rep(0, p - block)) + stats::rnorm(n))
  L <- 3.72251

  s_lasso <- cssLasso(Z, y, lambda=c(lambda=L, alpha=1))
  s_enet <- cssLasso(Z, y, lambda=c(lambda=L, alpha=0.2))

  # Pure lasso selects only the two strongest block members; the elastic net
  # pulls in the full correlated block.
  testthat::expect_identical(s_lasso, c(3L, 4L))
  testthat::expect_identical(s_enet, c(1L, 2L, 3L, 4L))

  # The elastic-net set is a strict superset of the lasso set (the round-2
  # cosmetic: robust to L within +/-10%).
  testthat::expect_true(all(s_lasso %in% s_enet))
  testthat::expect_true(length(setdiff(s_enet, s_lasso)) > 0)

  # Back-compatibility: a scalar lambda is byte-identical to the bundled form
  # with alpha = 1 (the default pure-lasso path is unchanged).
  testthat::expect_identical(cssLasso(Z, y, lambda=L),
                             cssLasso(Z, y, lambda=c(lambda=L, alpha=1)))
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
                         "For method cssLasso, y must be a numeric or integer vector.",
                         fixed=TRUE)

  testthat::expect_error(cssLoop(input=1L:4L, x=x, y=y,
                                 lambda=x, fitfun=cssLasso),
                         "For method cssLasso, lambda must be either a single nonnegative numeric or a named length-2 numeric vector c(lambda=<value>, alpha=<value>).",
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

testthat::test_that("cssLoop validates the raw fitfun return before remapping (#151)", {
  set.seed(1)
  n <- 40
  p <- 6
  x <- matrix(stats::rnorm(n * p), n, p)
  y <- stats::rnorm(n)
  subsample <- as.integer(1:(n %/% 2))
  feats_to_keep <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)  # p_sub = 4
  input <- list(subsample = subsample, feats_to_keep = feats_to_keep)

  mk <- function(val) function(X, y, lambda) val   # fitfun returning a fixed value

  # Each invalid raw return must error (was silently wrong on main #151):
  testthat::expect_error(cssLoop(input, x, y, lambda = 0.1,
                                 fitfun = mk(-1L)))                # non-positive
  testthat::expect_error(cssLoop(input, x, y, lambda = 0.1,
                                 fitfun = mk(c(TRUE, FALSE))),     # logical
                         "logical vector", fixed = TRUE)
  testthat::expect_error(cssLoop(input, x, y, lambda = 0.1,
                                 fitfun = mk(1.5)))                # fractional
  testthat::expect_error(cssLoop(input, x, y, lambda = 0.1,
                                 fitfun = mk(NULL)),               # NULL
                         "NULL", fixed = TRUE)
  # 5 > p_sub = 4; on main this errors but MIS-reports "NA values" (post-remap
  # NA). Asserting the message makes this a real regression test AND locks in
  # the cosmetic fix.
  testthat::expect_error(cssLoop(input, x, y, lambda = 0.1,
                                 fitfun = mk(5L)),
                         "index greater than ncol(X)", fixed = TRUE)

  # Valid raw returns still work and remap correctly:
  testthat::expect_equal(
    cssLoop(input, x, y, lambda = 0.1, fitfun = mk(integer(0))),  # select nothing
    integer(0))
  testthat::expect_equal(
    cssLoop(input, x, y, lambda = 0.1, fitfun = mk(c(1L, 3L))),   # raw cols 1,3 ...
    as.integer(which(feats_to_keep)[c(1L, 3L)]))                  # ... remap to 1,4
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

testthat::test_that("createSubsamples returns properly-named elements with feature removal (#69)", {
  set.seed(692)
  res <- createSubsamples(n = 30, p = 8, B = 5, sampling_type = "SS",
                          prop_feats_remove = 0.3)
  testthat::expect_equal(length(res), 2L * 5L)   # SS -> 2B elements
  # Every element is a named list(subsample, feats_to_keep) -- the invariant the
  # (now-meaningful) output stopifnot guards.
  testthat::expect_true(all(vapply(res,
      function(s) identical(names(s), c("subsample", "feats_to_keep")),
      logical(1))))
})

testthat::test_that("createSubsamples copies feats_to_keep onto each SS complementary pair (#128)", {
  set.seed(128)
  res <- createSubsamples(n = 30, p = 20, B = 5, sampling_type = "SS",
                          prop_feats_remove = 0.5)
  # SS with feature removal -> 2B elements.
  testthat::expect_equal(length(res), 10L)
  # Each complementary pair (i, B + i) must share the SAME feats_to_keep -- the
  # Shah-Samworth pairing guarantee (createSubsamples copies subsample i's mask
  # onto its pair B + i). With p = 20, two independent rbinom draws would match
  # with probability ~2^-20 (~1e-6), so the identity below genuinely exercises
  # the copy rather than passing by coincidence. Every subsample must also retain
  # >= 2 features (the while-loop guarantee).
  for(i in 1:5){
    testthat::expect_identical(res[[5L + i]]$feats_to_keep,
                               res[[i]]$feats_to_keep)
    testthat::expect_gte(sum(res[[i]]$feats_to_keep), 2)
    testthat::expect_gte(sum(res[[5L + i]]$feats_to_keep), 2)
  }
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

testthat::test_that("getSelMatrix detects a killed parallel worker (#104)", {
  set.seed(98623)
  x <- matrix(stats::rnorm(25*6), nrow=25, ncol=6)
  y <- stats::rnorm(25)
  subsamps_object <- createSubsamples(n=25, p=6, B=12, sampling_type="SS",
                                      prop_feats_remove=0)

  # Simulate a worker that was killed: mclapply returns a list containing NULL.
  testthat::local_mocked_bindings(
    mclapply = function(...) list(NULL),
    .package = "parallel")

  testthat::expect_error(
    getSelMatrix(x=x, y=y, lambda=0.01, B=12, sampling_type="SS",
                 subsamps_object=subsamps_object, num_cores=1,
                 fitfun=cssLasso),
    "parallel worker failed")
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

  # (#105, L12) Value-pin EVERY cluster column (not just cluster 2): each column
  # must equal the "at least one member of that cluster was selected" indicator.
  # This pins the full feat2clus mapping/ordering that the #57 rowsum refactor
  # relies on, including the singleton cluster (drop=FALSE keeps it a matrix).
  for(j in seq_along(good_clusters)){
    testthat::expect_equal(res[, j],
        as.integer(rowSums(good_res[, good_clusters[[j]], drop=FALSE]) > 0))
  }

  # Single-cluster case (#57): t(clust_counts > 0L) collapses to one column
  res1 <- getClusterSelMatrix(list(only = 1L:9L), good_res)
  testthat::expect_equal(ncol(res1), 1)
  testthat::expect_true(all(is.integer(res1)))

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  res_fitfun <- css(X=df2, y=stats::rnorm(nrow(X_df)), lambda=0.01, B = 10)
  testthat::expect_identical(class(res_fitfun), "cssr")
  
  # Should get error if I try to use clusters in this data.frame that contains
  # factors with more than two levels
  testthat::expect_error(css(X=df2, y=stats::rnorm(nrow(X_df)), lambda=0.01,
                             B = 10, clusters=1:3), "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.",
                         fixed=TRUE)
  
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

testthat::test_that("css errors on an X containing NA (#74)", {
  set.seed(8642)
  X <- matrix(stats::rnorm(20 * 4), nrow = 20, ncol = 4)
  X[3, 2] <- NA
  y <- stats::rnorm(20)
  testthat::expect_error(
    css(X = X, y = y, lambda = 0.01),
    "must not contain missing",
    fixed = TRUE)
})

testthat::test_that("css accepts any (X, y, lambda) fitfun signature and rejects wrong args (#154)", {
    set.seed(1)
    x <- matrix(stats::rnorm(60), 20, 3)
    y <- stats::rnorm(20)

    # (a) A default on lambda is accepted (rejected on the base by identical()).
    f_default <- function(X, y, lambda = 0.1) cssLasso(X, y, lambda)
    res_def <- css(x, y, lambda = 0.01, fitfun = f_default, B = 10)
    testthat::expect_identical(class(res_def), "cssr")

    # (b) Reordered args are accepted AND produce identical results
    #     (cssLoop calls fitfun by keyword, so order is irrelevant).
    f_reordered <- function(y, X, lambda) cssLasso(X, y, lambda)
    set.seed(7); res_re  <- css(x, y, lambda = 0.01, fitfun = f_reordered, B = 10)
    set.seed(7); res_ord <- css(x, y, lambda = 0.01, fitfun = cssLasso,    B = 10)
    testthat::expect_equal(res_re$feat_sel_mat, res_ord$feat_sel_mat)

    # (c) A wrong argument set is rejected, with the message listing the ACTUAL args.
    testthat::expect_error(
        css(x, y, lambda = 0.01, fitfun = function(X, y, z) 1L, B = 5),
        "must accept exactly the arguments X, y, and lambda (in any order, with or without defaults). Detected arguments to fitfun: X, y, z", fixed = TRUE)
    # (d) Extra args are rejected, with the new "exactly" wording.
    testthat::expect_error(
        css(x, y, lambda = 0.01, fitfun = function(X, y, lambda, extra) 1L, B = 5),
        "must accept exactly the arguments", fixed = TRUE)
})

testthat::test_that("cssLoop seeds fitfun reproducibly; css parallel RNG is isolated (#12)", {
  set.seed(99)
  x_rng <- matrix(stats::rnorm(40 * 8), nrow=40, ncol=8)
  y_rng <- stats::rnorm(40)

  # A stochastic fitfun: randomly selects 2 features (uses the RNG), so its
  # output depends on the seed in effect when it runs.
  rfit <- function(X, y, lambda){ sort(sample.int(ncol(X), 2)) }
  sub <- sort(sample.int(40L, 20L))

  # cssLoop's seed argument makes a stochastic fitfun reproducible: the same
  # seed gives the same selection. (On main, cssLoop has no seed argument, so
  # this behavior does not exist there.) This needs no parallelism.
  r1 <- cssLoop(input=sub, x=x_rng, y=y_rng, lambda=0.1, fitfun=rfit, seed=123L)
  r2 <- cssLoop(input=sub, x=x_rng, y=y_rng, lambda=0.1, fitfun=rfit, seed=123L)
  testthat::expect_identical(r1, r2)

  # ...and the seed genuinely drives the (stochastic) selection -- different
  # seeds give more than one distinct result (so the check above is not vacuous).
  outs <- lapply(1:8, function(s) cssLoop(input=sub, x=x_rng, y=y_rng,
                                          lambda=0.1, fitfun=rfit, seed=s))
  testthat::expect_true(length(unique(outs)) > 1)

  # End-to-end, serial css is reproducible across re-runs.
  set.seed(1)
  d1 <- css(x_rng, y_rng, lambda=0.1, fitfun=rfit, B=10L, num_cores=1L)$feat_sel_mat
  set.seed(1)
  d2 <- css(x_rng, y_rng, lambda=0.1, fitfun=rfit, B=10L, num_cores=1L)$feat_sel_mat
  testthat::expect_identical(d1, d2)

  # Parallel checks need real forking; run the parallel css calls defensively
  # and skip (rather than fail) when forking is unavailable -- Windows, CRAN, or
  # a fully-loaded machine. A genuine reproducibility regression would still
  # surface as a failed expectation below, not a skip.
  par_runs <- tryCatch({
    set.seed(1)
    a <- css(x_rng, y_rng, lambda=0.1, fitfun=rfit, B=10L, num_cores=2L)$feat_sel_mat
    set.seed(1)
    b <- css(x_rng, y_rng, lambda=0.1, fitfun=rfit, B=10L, num_cores=2L)$feat_sel_mat
    list(a=a, b=b)
  }, error=function(e) NULL)
  if(is.null(par_runs)){
    testthat::skip("parallel forking unavailable in this environment")
  }
  testthat::expect_identical(par_runs$a, par_runs$b)   # parallel reproducible
  testthat::expect_identical(par_runs$a, d1)           # serial == parallel
})

testthat::test_that("discreteYAbortProb estimates the constant-subsample probability (#131)", {
  # Continuous y: every value is distinct, so no size-floor(n/2) subsample can
  # be constant. The aggregate underflows to exactly 0 -- assert the inequality,
  # NOT expect_equal to a tiny value.
  set.seed(131)
  y_cont <- stats::rnorm(100)
  testthat::expect_true(discreteYAbortProb(y_cont, 50L, "SS") < 1e-6)

  # Balanced binary at n = 100: each value's count (50) equals m = 50, so
  # p_const = 2 * C(50,50)/C(100,50) ~ 2e-29 and the aggregate underflows to 0.
  # This is the KEY false positive that the naive max(table(y)) >= floor(n/2)
  # trigger gets wrong (there count == m would fire).
  y_bin <- rep(0:1, each = 50)
  testthat::expect_true(discreteYAbortProb(y_bin, 50L, "SS") < 1e-6)

  # Skewed y (95/5): 0 is a strong majority (count 95 >> m = 50), so some
  # subsample is very likely all-zeros (~94% at B = 50, SS).
  y_skew <- c(rep(0, 95), rep(1, 5))
  testthat::expect_true(discreteYAbortProb(y_skew, 50L, "SS") > 0.5)

  # Fully constant y: every subsample is constant -> probability exactly 1.
  testthat::expect_equal(discreteYAbortProb(rep(0, 100), 50L, "SS"), 1)

  # Monotone in n_sub: "SS" draws 2*B subsamples, a non-"SS" string draws B, so
  # the SS probability is at least the B-count probability for the same y. (Pure
  # helper call exercising the n_sub branch; not a css() run.)
  testthat::expect_true(
    discreteYAbortProb(y_skew, 50L, "SS") >=
      discreteYAbortProb(y_skew, 50L, "MB"))
})

testthat::test_that("css() warns up front on a highly discrete y for the default cssLasso (#131)", {
  set.seed(2718)
  n <- 40L
  x_disc <- matrix(stats::rnorm(n * 6), nrow = n, ncol = 6)
  # 0 is a strong majority (count 38 >> floor(40/2) = 20), so the estimated
  # abort probability is ~99% -- well over the 1% threshold. y has 3 unique
  # values, so it passes input checks and reaches the warning.
  y_disc <- c(rep(0, 38), 1, 2)

  # (a) discrete y + default cssLasso -> the up-front warning fires. Such a run
  # also aborts later in the loop (a constant-y subsample), so wrap the call in
  # try(): the warning is computed before any subsampling/RNG and fires either
  # way (B = 10L so the small-B checkB warning does not also fire).
  testthat::expect_warning(
    try(css(X = x_disc, y = y_disc, lambda = 0.01, B = 10L), silent = TRUE),
    "highly discrete", fixed = TRUE)

  # The warning message reports a plausible percentage and names cssLasso.
  disc_warns <- testthat::capture_warnings(
    try(css(X = x_disc, y = y_disc, lambda = 0.01, B = 10L), silent = TRUE))
  disc_msg <- disc_warns[grepl("highly discrete", disc_warns, fixed = TRUE)]
  testthat::expect_length(disc_msg, 1L)
  testthat::expect_match(disc_msg, "%", fixed = TRUE)
  testthat::expect_match(disc_msg, "cssLasso", fixed = TRUE)

  # (b) continuous y -> no warning at all (a clean, complete run).
  set.seed(2719)
  x_cont <- matrix(stats::rnorm(n * 6), nrow = n, ncol = 6)
  testthat::expect_no_warning(
    css(X = x_cont, y = stats::rnorm(n), lambda = 0.01, B = 10L))

  # (c) balanced binary at n = 100 -> NO "highly discrete" warning (the KEY
  # false-positive guard). At small n balanced binary can legitimately warn, so
  # keep n large. Capture all warnings and assert none match.
  set.seed(2720)
  x_bin <- matrix(stats::rnorm(100 * 6), nrow = 100, ncol = 6)
  y_bin <- rep(0:1, each = 50)
  bin_warns <- testthat::capture_warnings(
    css(X = x_bin, y = y_bin, lambda = 0.01, B = 10L))
  testthat::expect_false(any(grepl("highly discrete", bin_warns, fixed = TRUE)))

  # (d) custom fitfun with the SAME discrete y -> NO warning (proves the
  # identical(fitfun, cssLasso) gate). The stub ignores y (so it tolerates a
  # constant-y subsample) and returns a fixed, valid selected set.
  tol_fitfun <- function(X, y, lambda){ c(1L, 2L) }
  testthat::expect_no_warning(
    css(X = x_disc, y = y_disc, lambda = 0.01, B = 10L, fitfun = tol_fitfun))

  # (e) non-finite numeric y (majority Inf) + default cssLasso -> NO "highly
  # discrete" warning (proves the all(is.finite(sel_y)) guard). Without the
  # guard, table() would miscount the 38 Inf cells as a dominating value and
  # warn (~100%); with it, the diagnostic is skipped and cssLasso instead aborts
  # on the non-finite value (checkCssLassoInputs). Every size-floor(40/2) = 20
  # subsample must include an Inf (only 2 finite cells), so the abort is
  # deterministic; wrap the warning-capture call in try().
  y_nonfin <- c(rep(Inf, 38), 0, 1)
  nonfin_warns <- testthat::capture_warnings(
    try(css(X = x_disc, y = y_nonfin, lambda = 0.01, B = 10L), silent = TRUE))
  testthat::expect_false(any(grepl("highly discrete", nonfin_warns, fixed = TRUE)))
  # ...and cssLasso still rejects the non-finite y (no warning to suppress now).
  testthat::expect_error(
    css(X = x_disc, y = y_nonfin, lambda = 0.01, B = 10L),
    "non-finite", fixed = TRUE)
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
  # min_num_clusts = 0 is now allowed (empty threshold selection)
  testthat::expect_null(checkMinNumClusts(0, 13, 7))

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
  testthat::expect_error(checkMinNumClusts(-1, 13, 7),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(checkMinNumClusts(-1, 9, 8),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)
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

  # (#105, L11) Zero-selection fallback: when every member of the cluster has
  # selection proportion 0 (so sum(sel_props) == 0), both "sparse" and
  # "weighted_avg" must fall back to equal weights rep(1/n, n) rather than
  # dividing by zero.
  zero_props <- c(0, 0, 0, 0.5, 0.8)
  testthat::expect_identical(getClustWeights(cluster_i=c(1L, 2L, 3L),
                                             weighting="sparse",
                                             feat_sel_props=zero_props),
                             rep(1/3, 3))
  testthat::expect_identical(getClustWeights(cluster_i=c(1L, 2L, 3L),
                                             weighting="weighted_avg",
                                             feat_sel_props=zero_props),
                             rep(1/3, 3))

  # (#105, L11) Sparse tie-split: when two cluster members tie for the top
  # selection proportion, "sparse" splits the weight equally between them (and
  # gives 0 to the rest) rather than arbitrarily picking one.
  tie_props <- c(0.9, 0.9, 0.3, 0.5, 0.8)
  testthat::expect_identical(getClustWeights(cluster_i=c(1L, 2L, 3L),
                                             weighting="sparse",
                                             feat_sel_props=tie_props),
                             c(0.5, 0.5, 0))
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
    true_weights[[i]] <- true_weights[[i]]/sum(true_weights[[i]])
  }
  
  names(true_weights) <- clust_names[1:2]
  
  testthat::expect_identical(getAllClustWeights(res,
                                                colMeans(res$clus_sel_mat[, 1:2]),
                                                "sparse"), true_weights)

  # weighted_avg
  true_weights <- list()

  for(i in 1:2){
    true_weights[[i]] <- sel_props[sel_clusts[[i]]]/sum(sel_props[unlist(sel_clusts[[i]])])
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
  
  # An empty selection is now allowed (min_num_clusts=0), so length 0 no longer
  # trips a guard; an unnamed empty selected_clusts is still rejected (it fails
  # the names check before the length check would matter).
  testthat::expect_error(checkGetSelectedClustersOutput(selected_clusts=numeric(),
                                                       selected_feats=sel_feats,
                                                       weights=weights,
                                                       n_clusters=10, p=30),
                         "!is.null(names(selected_clusts)) is not TRUE",
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
    # For "sparse" weighting, either there should only be one nonzero weight and
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

  # Regression test (#10): the cutoff is adjusted by repeated +/- 1/B in the
  # min/max loops, accumulating floating-point error, so a cluster sitting
  # exactly at the threshold could be dropped -- breaking the max_num_clusts
  # loop early and returning MORE clusters than max_num_clusts. With B=10 and
  # cluster proportions (0, 0.3, 0.2), cutoff=0.1 / min=1 / max=1 must return a
  # single cluster (the 0.3 one); pre-fix it returned two (0.3 was excluded by
  # the float-accumulated cutoff 0.30000000000000004).
  csm <- cbind(as.integer(rep(0, 10)),
               as.integer(c(rep(1, 3), rep(0, 7))),
               as.integer(c(rep(1, 2), rep(0, 8))))
  clus_mat <- csm
  colnames(clus_mat) <- c("c1", "c2", "c3")
  feat_mat <- csm
  colnames(feat_mat) <- c("f1", "f2", "f3")
  mock_css <- list(feat_sel_mat=feat_mat, clus_sel_mat=clus_mat,
                   X=matrix(stats::rnorm(30), nrow=10, ncol=3,
                            dimnames=list(NULL, c("f1", "f2", "f3"))),
                   y=stats::rnorm(10),
                   clusters=list(c1=1L, c2=2L, c3=3L),
                   train_inds=integer())
  class(mock_css) <- "cssr"
  res_tie <- getSelectedClusters(mock_css, weighting="simple_avg", cutoff=0.1,
                                 min_num_clusts=1, max_num_clusts=1)
  testthat::expect_true(length(res_tie$selected_clusts) <= 1)
  testthat::expect_identical(names(res_tie$selected_clusts), "c2")

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

testthat::test_that("getSelectedClusters does not hang when min_num_clusts > n_clusters (#69)", {
  # Minimal cssr object (same pattern as the #42 test) with 2 clusters.
  # min_num_clusts = 5 exceeds the 2 available clusters: pre-#69 the min-loop
  # decrements the cutoff forever; post-#69 the guard breaks once all clusters
  # are selected, so checkSelectedClusters warns and the function returns them.
  B <- 10
  clusters <- list(c1 = 1:2, c2 = 3:4)
  clus_sel_mat <- cbind(c1 = rep(1, B), c2 = c(rep(1, B - 1), 0))
  feat_sel_mat <- cbind(X1 = rep(1, B), X2 = rep(1, B),
                        X3 = c(rep(1, B - 1), 0), X4 = c(rep(1, B - 1), 0))
  obj <- structure(list(feat_sel_mat = feat_sel_mat, clus_sel_mat = clus_sel_mat,
                        clusters = clusters), class = "cssr")
  testthat::expect_warning(
    res <- getSelectedClusters(obj, weighting = "simple_avg", cutoff = 0,
                               min_num_clusts = 5L, max_num_clusts = NA),
    "Returning fewer than min_num_clusts", fixed = TRUE)
  testthat::expect_equal(length(res$selected_clusts), 2L)  # both clusters, no hang
})

testthat::test_that("getSelectedClusters max_num_clusts handles proportion-1.0 ties (#42)", {
  # Minimal cssr object: cluster c1 at selection proportion 1.0, cluster c2 at
  # (B - 1)/B (just below 1.0). With max_num_clusts = 1 the cutoff loop must
  # raise the threshold to 1.0 to drop c2 and keep only the proportion-1.0
  # cluster. The cutoff accumulates +1/B, and for these B the cumulative sum
  # floats just above 1 (e.g. B = 9: 1.0000000000000002), so the old
  # `if(cutoff > 1) break` fired before the cutoff == 1 filter ran and wrongly
  # kept c2 (returning 2 clusters for max_num_clusts = 1). The `+ tol` guard
  # fixes this.
  make_obj <- function(B){
    clusters <- list(c1 = 1:2, c2 = 3:4)
    clus_sel_mat <- cbind(c1 = rep(1, B), c2 = c(rep(1, B - 1), 0))
    feat_sel_mat <- cbind(X1 = rep(1, B), X2 = rep(1, B),
                          X3 = c(rep(1, B - 1), 0), X4 = c(rep(1, B - 1), 0))
    obj <- list(feat_sel_mat = feat_sel_mat, clus_sel_mat = clus_sel_mat,
                clusters = clusters)
    class(obj) <- "cssr"
    obj
  }
  for(B in c(9, 11, 20)){
    res <- getSelectedClusters(make_obj(B), weighting = "simple_avg", cutoff = 0,
      min_num_clusts = 1, max_num_clusts = 1)
    # Only the proportion-1.0 cluster c1 survives; c2 at (B-1)/B is below 1.
    testthat::expect_identical(names(res$selected_clusts), "c1")
    testthat::expect_equal(unname(res$selected_clusts), 1)
  }
})

testthat::test_that("getCssSelections works", {

  set.seed(26717)
  
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  res <- getCssSelections(css_res)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
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
  
  testthat::expect_equal(length(res$selected_clusts), length(res$weights))
  for(i in 1:length(res$weights)){
    weights_i <- res$weights[[i]]
    num_nonzero_weights <- sum(weights_i > 0)
    # For "sparse" weighting, either there should only be one nonzero weight and
    # it should equal 1 (if there were no ties in selection proportions among
    # cluster members) or the nonzero weights should all be
    # 1/num_nonzero_weights
    testthat::expect_true(all(weights_i[weights_i > 0] == 1/num_nonzero_weights))
  }

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
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
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
  testthat::expect_error(getCssSelections(css_res, min_num_clusts=-1),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, max_num_clusts=50),
                         "max_num_clusts <= p is not TRUE", fixed=TRUE)
  testthat::expect_error(getCssSelections(css_res, max_num_clusts=4.5),
                         "max_num_clusts == round(max_num_clusts) is not TRUE",
                         fixed=TRUE)
})

testthat::test_that("min_num_clusts = 0 allows an empty threshold selection", {
  # Deterministic empty-producing fixture: a fixed seed, all-singleton clusters
  # (clusters = list(), so every feature is its own cluster), and a large lambda
  # so the base lasso selects almost nothing. We assert that the maximum cluster
  # selection proportion is below 1 - 1/(2*B) so that cutoff = 1 genuinely
  # selects no cluster (otherwise the test would not exercise the empty path).
  set.seed(8)
  B <- 10
  x <- matrix(stats::rnorm(30*8), nrow=30, ncol=8)
  y <- stats::rnorm(30)
  css_res <- css(X=x, y=y, lambda=0.5, clusters=list(), B=B)

  testthat::expect_true(max(colMeans(css_res$clus_sel_mat)) < 1 - 1/(2*B))

  # Selection path: getCssSelections returns a clean empty result. (weights is a
  # NAMED empty list, so assert via length/type rather than expect_equal(.,list()).)
  empty_sel <- getCssSelections(css_res, cutoff=1, min_num_clusts=0)
  testthat::expect_length(empty_sel$selected_clusts, 0)
  testthat::expect_true(is.list(empty_sel$selected_clusts))
  testthat::expect_length(empty_sel$selected_feats, 0)
  testthat::expect_true(is.integer(empty_sel$selected_feats))
  testthat::expect_length(empty_sel$weights, 0)
  testthat::expect_true(is.list(empty_sel$weights))

  # Design / prediction paths: one clear error (no design/predictions possible).
  testthat::expect_error(getCssDesign(css_res, newX=x, cutoff=1,
                                      min_num_clusts=0),
                         "No clusters were selected", fixed=TRUE)
  testthat::expect_error(getCssPreds(css_res, testX=x, trainX=x, trainY=y,
                                     cutoff=1, min_num_clusts=0),
                         "No clusters were selected", fixed=TRUE)

  # Legacy regression: the default min_num_clusts = 1 still forces >= 1 cluster
  # on the SAME fixture (the floor of 1 is unchanged).
  legacy_sel <- getCssSelections(css_res, cutoff=1, min_num_clusts=1)
  testthat::expect_equal(length(legacy_sel$selected_clusts), 1)
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
  # cyl, gear, and carb are factors with more than 2 levels
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

  # An NA-containing newX was previously read as "not provided" (the
  # all(!is.na) sentinel) and silently replaced by the train_inds data; it is
  # now treated as provided and rejected by checkNoNAs (#71).
  x_na <- x_new
  x_na[3, 2] <- NA
  testthat::expect_error(checkNewXProvided(x_na, css_res_train),
                         "must not contain missing", fixed = TRUE)
  
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

  # (#104, L5) Dispatch on the NA-sentinel default, not a length() heuristic.
  # css_res_train has train_inds, so the OLD length()-based code silently fell
  # back to that training data for these misclassified inputs; they must now be
  # treated as "provided".
  # A bare numeric vector (length > 1) is not a valid design matrix and now
  # errors clearly instead of failing later on a cryptic is.matrix() stopifnot.
  testthat::expect_error(
    checkNewXProvided(stats::rnorm(5), css_res_train),
    "newX must be a matrix or data.frame", fixed=TRUE)
  # A one-column data.frame (length 1) used to be misread as "not provided" and
  # silently replaced by the train_inds data; it now enters the provided branch
  # and errors (here on a feature-name/column mismatch) rather than falling back.
  testthat::expect_error(
    checkNewXProvided(data.frame(a=stats::rnorm(8)), css_res_train))

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
                                                  min_num_clusts=-1,
                                                  max_num_clusts=NA,
                                                  newx=x_new),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)

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

  # Value-level checks: pin every entry of the design matrix to a hand-computed
  # value across all three weighting schemes (#101). The structural checks above
  # would pass for a wrong-but-well-shaped design (column misalignment,
  # transposed/reversed weights, or one cluster's weights applied to another).
  feat_sel_props <- colMeans(css_res$feat_sel_mat)
  for (w in c("simple_avg", "weighted_avg", "sparse")) {
    des <- formCssDesign(css_res, weighting = w, cutoff = 0, min_num_clusts = 1, newx = x_new)
    wts <- getSelectedClusters(css_res, weighting = w, cutoff = 0,
                               min_num_clusts = 1, max_num_clusts = NA)$weights
    # exported-path equivalence (getCssDesign just forwards to formCssDesign)
    testthat::expect_equal(
      getCssDesign(css_res, newX = x_new, weighting = w, cutoff = 0, min_num_clusts = 1), des)
    for (name in names(wts)) {
      clust <- css_res$clusters[[name]]
      w_i <- wts[[name]]
      sp <- feat_sel_props[clust]
      # (1) APPLICATION: weights applied to the correct columns, correct orientation, correct cluster
      testthat::expect_equal(des[, name],
        as.numeric(x_new[, clust, drop = FALSE] %*% w_i), info = paste(w, name))
      # (2) SEMANTIC anchor per scheme (independent of getSelectedClusters$weights)
      if (w == "simple_avg") {
        testthat::expect_equal(des[, name], rowMeans(x_new[, clust, drop = FALSE]), info = name)
      } else if (w == "weighted_avg") {
        if (sum(sp) > 0) {
          testthat::expect_equal(unname(w_i), unname(sp / sum(sp)), info = name)
        } else {
          testthat::expect_equal(unname(w_i), rep(1 / length(clust), length(clust)), info = name)
        }
      } else { # sparse: equal weight on the member(s) tied at max(sp); else equal weights
        if (sum(sp) > 0) {
          maxes <- sp == max(sp)
          testthat::expect_equal(des[, name],
            as.numeric(rowMeans(x_new[, clust[maxes], drop = FALSE])), info = name)
        } else {
          testthat::expect_equal(des[, name], rowMeans(x_new[, clust, drop = FALSE]), info = name)
        }
      }
    }
  }

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

  testthat::expect_error(formCssDesign(css_results=css_res, min_num_clusts=-1,
                                       newx=x_new),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)

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

  # A single-row newX is accepted (#44): getCssDesign used to require > 1 row,
  # while getCssPreds/cssPredict already accept a 1-row test set.
  res_1row <- getCssDesign(css_res, newX = x_new[1, , drop = FALSE])
  testthat::expect_true(is.matrix(res_1row))
  testthat::expect_equal(nrow(res_1row), 1)
  testthat::expect_equal(ncol(res_1row), length(css_res$clusters))
  
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
  # #142: the data.frame newX path must NOT emit the spurious "no variable names"
  # warning (checkXInputResults strips the coerced names; getCssDesign re-attaches
  # the already-validated ones before formCssDesign).
  df_warns <- character(0)
  res_df <- withCallingHandlers(
    getCssDesign(css_results=css_res_df, weighting="simple_avg",
                 cutoff=0.7, min_num_clusts=3, max_num_clusts=NA,
                 newX=X_df[fit_inds, ]),
    warning=function(w){ df_warns <<- c(df_warns, conditionMessage(w));
      invokeRestart("muffleWarning") })
  testthat::expect_false(any(grepl("no variable names", df_warns, fixed=TRUE)))

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

  testthat::expect_error(getCssDesign(css_results=css_res, min_num_clusts=-1,
                                       newX=x_new),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)

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

testthat::test_that("getCssDesign/getCssPreds handle length-1 train_inds (#127)", {
  set.seed(51274)

  x_select <- matrix(stats::rnorm(10*6), nrow=10, ncol=6)
  y_select <- stats::rnorm(10)
  x_pred <- matrix(stats::rnorm(4*6), nrow=4, ncol=6)

  # clusters covers features 1:3; features 4, 5, 6 become singleton clusters, so
  # css_res has 4 clusters. train_inds = 5L sets aside a single observation.
  css_res <- css(X=x_select, y=y_select, lambda=0.01,
                 clusters=list(cluster1=1:3), B=10, train_inds=5L)

  # getCssDesign with no newX re-derives the design from the single training row
  # and must return a 1 x (#clusters) matrix without error.
  des <- getCssDesign(css_res)
  testthat::expect_true(is.matrix(des))
  testthat::expect_true(is.numeric(des))
  testthat::expect_equal(nrow(des), 1)
  testthat::expect_equal(ncol(des), length(css_res$clusters))

  # getCssPreds with the same length-1 train_inds cannot fit OLS (1 training row
  # is not more than the number of clusters + intercept): it must fail with the
  # clean OLS-size guard message, NOT a subscript/matrix crash. This guard is
  # deliberate -- 1 training row genuinely cannot fit the prediction model.
  testthat::expect_error(getCssPreds(css_res, testX=x_pred),
                         "not provided with enough indices to fit OLS",
                         fixed=TRUE)

  # A length-2 train_inds still yields a valid 2-row design (no regression).
  css_res2 <- css(X=x_select, y=y_select, lambda=0.01,
                  clusters=list(cluster1=1:3), B=10, train_inds=c(5L, 6L))
  des2 <- getCssDesign(css_res2)
  testthat::expect_true(is.matrix(des2))
  testthat::expect_equal(nrow(des2), 2)
  testthat::expect_equal(ncol(des2), length(css_res2$clusters))
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
                                                cutoff=0.1, min_num_clusts=-1,
                                                max_num_clusts=NA,
                                                trainX=x_train, trainY=y_train),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)

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

  # (#104, L6) The OLS adequacy guard must account for the lm() intercept. At the
  # default cutoff the training design has one column per cluster; css_res has 4
  # clusters (red, blue, green, and feature 6 as a singleton). With exactly 4
  # training rows, lm(y ~ .) is rank-deficient (4 observations vs 4 cluster
  # coefficients + an intercept), so getCssPreds must now error rather than
  # silently fit a rank-deficient model with NA coefficients.
  x_train_eq <- matrix(stats::rnorm(4*6), nrow=4, ncol=6)
  y_train_eq <- stats::rnorm(4)
  testthat::expect_error(
    getCssPreds(css_res, testX=x_pred, trainX=x_train_eq, trainY=y_train_eq),
    "one more training observation than the number of clusters", fixed=TRUE)
  # One extra training row (5 observations vs 4 coefficients + intercept) is
  # enough, and predictions are produced normally.
  x_train_ok <- matrix(stats::rnorm(5*6), nrow=5, ncol=6)
  y_train_ok <- stats::rnorm(5)
  res_ok <- getCssPreds(css_res, testX=x_pred, trainX=x_train_ok,
                        trainY=y_train_ok)
  testthat::expect_true(all(!is.na(res_ok)))
  testthat::expect_equal(length(res_ok), 7)

  # (#105, L9) The value-producing calls above only exercise the default
  # weighting="weighted_avg"; cover "sparse" and "simple_avg" too on the
  # trainX/trainY path. Each weighting must return a finite numeric vector of
  # length nrow(x_pred).
  preds_by_w <- list()
  for(w in c("weighted_avg", "sparse", "simple_avg")){
    preds_w <- getCssPreds(css_res, testX=x_pred, weighting=w, trainX=x_train,
                           trainY=y_train)
    testthat::expect_true(is.numeric(preds_w))
    testthat::expect_true(all(!is.na(preds_w)))
    testthat::expect_equal(length(preds_w), nrow(x_pred))
    preds_by_w[[w]] <- preds_w
  }
  # getCssPreds already enforces finiteness internally, so "didn't error" is
  # nearly vacuous; pin the behavior by requiring the three weightings to
  # produce mutually different predictions.
  testthat::expect_false(isTRUE(all.equal(preds_by_w[["weighted_avg"]],
                                          preds_by_w[["sparse"]])))
  testthat::expect_false(isTRUE(all.equal(preds_by_w[["weighted_avg"]],
                                          preds_by_w[["simple_avg"]])))
  testthat::expect_false(isTRUE(all.equal(preds_by_w[["sparse"]],
                                          preds_by_w[["simple_avg"]])))

  # (#105, L10a) trainY OLS-response contract. A non-numeric trainY is caught
  # upstream by checkGetCssPredsInputs' stopifnot(is.numeric(trainY)) (which
  # shadows getCssPreds' own "must be real-valued" guard -- that guard is dead
  # code, flagged for a future cleanup), so the message is the stopifnot text.
  testthat::expect_error(
    getCssPreds(css_res, testX=x_pred, trainX=x_train,
                trainY=factor(sample(letters[1:3], nrow(x_train),
                                     replace=TRUE))),
    "is.numeric(trainY) is not TRUE", fixed=TRUE)

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

  testthat::expect_error(getCssPreds(css_res, testX=x_pred, min_num_clusts=-1,
                                     trainX=x_train, trainY=y_train),
                         "min_num_clusts >= 0 is not TRUE", fixed=TRUE)

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

  # (#105, L9) Same weighting coverage ("sparse"/"simple_avg" plus the default)
  # on the train_inds (no-trainX) path, and the same mutual-difference pin.
  preds_by_w_ti <- list()
  for(w in c("weighted_avg", "sparse", "simple_avg")){
    preds_w_ti <- getCssPreds(css_res_train, testX=x_pred, weighting=w)
    testthat::expect_true(is.numeric(preds_w_ti))
    testthat::expect_true(all(!is.na(preds_w_ti)))
    testthat::expect_equal(length(preds_w_ti), nrow(x_pred))
    preds_by_w_ti[[w]] <- preds_w_ti
  }
  testthat::expect_false(isTRUE(all.equal(preds_by_w_ti[["weighted_avg"]],
                                          preds_by_w_ti[["sparse"]])))
  testthat::expect_false(isTRUE(all.equal(preds_by_w_ti[["weighted_avg"]],
                                          preds_by_w_ti[["simple_avg"]])))
  testthat::expect_false(isTRUE(all.equal(preds_by_w_ti[["sparse"]],
                                          preds_by_w_ti[["simple_avg"]])))

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
  # 18 (not 17) training rows: once the factor columns below are one-hot encoded
  # the design has 17 cluster representatives, and the OLS adequacy guard (#104,
  # L6) now requires strictly more training rows than clusters (the lm()
  # intercept), so 17 == 17 would (correctly) error.
  train_inds <- (max(selec_inds) + 1):(max(selec_inds) + 18)
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
  # #142: this data.frame testX+trainX path used to emit two spurious column-name
  # warnings -- checkNewXProvided strips trainX's names, so a named testX and a
  # name-less trainX tripped "provided for testX but not for trainX" and, via
  # formCssDesign(newx=trainX), "New X provided had no variable names". After the
  # trainX re-attach in checkGetCssPredsInputs, NEITHER fires. Capture warnings
  # and assert both #142 messages are absent.
  df_warns <- character(0)
  res_df <- withCallingHandlers(
    getCssPreds(css_res_df, testX=X_df[test_inds, ],
                trainX=X_df[train_inds, ], trainY=y[train_inds]),
    warning=function(w){
      df_warns <<- c(df_warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  testthat::expect_false(any(grepl("testX but not for trainX", df_warns,
                                   fixed=TRUE)))
  testthat::expect_false(any(grepl("New X provided had no variable names",
                                   df_warns, fixed=TRUE)))

  testthat::expect_true(all(!is.na(res_df)))
  testthat::expect_true(is.numeric(res_df))
  testthat::expect_equal(length(res_df), length(test_inds))

  # (#105, L10b) Reachable real-valued-y guard on the train_inds (no-trainX)
  # path: when css was fit on a non-real-valued y (here a character response,
  # which css accepts because validation is delegated to fitfun), getCssPreds
  # cannot fit the OLS model from css_results$y[train_inds] and must error. The
  # custom fitfun's formals must match cssLasso's (X, y, lambda) exactly. Seeded
  # last so this RNG-consuming css() call does not shift any earlier draws.
  set.seed(10510)
  n_chr <- 12
  X_chr <- matrix(stats::rnorm(n_chr*6), nrow=n_chr, ncol=6)
  y_chr <- as.character(sample(letters, n_chr, replace=TRUE))
  fitfun_chr <- function(X, y, lambda) 1:2
  css_res_chr <- css(X_chr, y_chr, lambda=0.01, train_inds=8:12,
                     fitfun=fitfun_chr, B=10)
  testthat::expect_error(
    getCssPreds(css_res_chr, testX=matrix(stats::rnorm(3*6), nrow=3, ncol=6)),
    "Can't generate predictions from the data", fixed=TRUE)

})

testthat::test_that("getCssPreds data.frame path is warning-free and matches matrix path (#142)", {
  set.seed(14200)

  p <- 5

  Xm_select <- matrix(stats::rnorm(24 * p), nrow=24, ncol=p)
  colnames(Xm_select) <- paste0("V", 1:p)
  y_select <- stats::rnorm(24)

  Xm_train <- matrix(stats::rnorm(10 * p), nrow=10, ncol=p)
  colnames(Xm_train) <- paste0("V", 1:p)
  y_train <- stats::rnorm(10)

  Xm_test <- matrix(stats::rnorm(6 * p), nrow=6, ncol=p)
  colnames(Xm_test) <- paste0("V", 1:p)

  # Four clusters over five features; at the default cutoff the OLS design has at
  # most four cluster representatives, and 10 training rows > 4 clears the
  # lm()-intercept adequacy guard so predictions are produced (rather than
  # hitting the OLS-size stop).
  clusters <- list(a=1:2, b=3, c=4, d=5)
  css_res <- css(X=Xm_select, y=y_select, lambda=0.01, clusters=clusters, B=10)

  Xd_train <- as.data.frame(Xm_train)
  Xd_test  <- as.data.frame(Xm_test)

  # (a) The data.frame testX+trainX path emits NEITHER #142 warning -- and, for a
  # clean numeric data.frame with consistent names, no warning at all.
  df_warns <- character(0)
  preds_df <- withCallingHandlers(
    getCssPreds(css_res, testX=Xd_test, trainX=Xd_train, trainY=y_train),
    warning=function(w){
      df_warns <<- c(df_warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  testthat::expect_false(any(grepl("testX but not for trainX", df_warns,
                                   fixed=TRUE)))
  testthat::expect_false(any(grepl("New X provided had no variable names",
                                   df_warns, fixed=TRUE)))
  testthat::expect_length(df_warns, 0)

  # (c) Byte-identity: column names never enter the numeric path (formCssDesign
  # strips them and forms cluster representatives positionally), so the
  # data.frame-input predictions equal the matrix-input predictions exactly.
  preds_mat <- getCssPreds(css_res, testX=Xm_test, trainX=Xm_train,
                           trainY=y_train)
  testthat::expect_equal(unname(preds_df), unname(preds_mat))

  # (b) A genuine column-name mismatch must still error -- the re-attach uses the
  # already-validated feat_names and cannot mask a real mismatch: a trainX
  # data.frame whose names differ from the X css was fit on is rejected.
  Xd_train_bad <- Xd_train
  colnames(Xd_train_bad) <- paste0("W", 1:p)
  testthat::expect_error(
    getCssPreds(css_res, testX=Xd_test, trainX=Xd_train_bad, trainY=y_train),
    "identical(feat_names, colnames(css_X)) is not TRUE", fixed=TRUE)
})

testthat::test_that("getCssPreds handles partial trainX/trainY inputs (#128)", {
  set.seed(128)
  X <- matrix(stats::rnorm(15 * 6), nrow = 15, ncol = 6)
  y <- stats::rnorm(15)
  # 1:3 as one cluster; feats 4, 5, 6 become singletons -> 4 clusters total.
  clusters <- list(cluster1 = 1:3)

  # No train_inds (train_inds empty).
  css_no_train <- css(X = X, y = y, lambda = 0.01, clusters = clusters, B = 10)
  # train_inds length 6 > 4 clusters, so the fallback path clears the downstream
  # OLS-size guard (needs more training rows than clusters) and can predict
  # rather than error.
  css_with_train <- css(X = X, y = y, lambda = 0.01, clusters = clusters,
                        B = 10, train_inds = 10:15)

  testX <- matrix(stats::rnorm(4 * 6), nrow = 4, ncol = 6)
  trainX <- matrix(stats::rnorm(8 * 6), nrow = 8, ncol = 6)

  # (A) trainX provided, trainY missing (scalar NA), css had NO train_inds ->
  # hard stop.
  testthat::expect_error(
    getCssPreds(css_no_train, testX = testX, trainX = trainX, trainY = NA),
    "must provide both trainX and trainY", fixed = TRUE)

  # (B) trainX provided, trainY missing, css HAD train_inds -> warn, fall back to
  # the train_inds observations, then predict.
  testthat::expect_warning(
    getCssPreds(css_with_train, testX = testX, trainX = trainX, trainY = NA),
    "trainX provided but no trainY", fixed = TRUE)

  # (C) trainY provided but no trainX, css HAD train_inds -> warn, use the
  # train_inds observations (trainY is ignored on this path), then predict.
  testthat::expect_warning(
    getCssPreds(css_with_train, testX = testX, trainY = c(1, 2, 3)),
    "trainY provided but no trainX", fixed = TRUE)
})

testthat::test_that("olsPredictRankSafe predicts from rank-deficient OLS without crashing (#117)", {
  # (a) Crash/finiteness guard. On R >= 4.4 predict.lm()'s default
  # rankdeficient="warnif" aborts ("NA/NaN/Inf in foreign function call") when
  # qr.R(model$qr) holds NaN/Inf -- a rank-deficient fit plus a LINPACK numerical
  # pathology seen on real high-dimensional data. Reproduce that precondition by
  # fitting a rank-deficient lm (a duplicated column) and injecting NaN into an
  # OFF-DIAGONAL upper-triangular entry of the aliased column. (The aliased
  # diagonal entry alone does not reproduce it: predict.lm overwrites diagonal
  # entries in rows beyond the rank with 1.)
  set.seed(11711)
  n_a <- 20
  df_a <- data.frame(y=stats::rnorm(n_a), a=stats::rnorm(n_a),
                     b=stats::rnorm(n_a), c=stats::rnorm(n_a))
  df_a$d <- df_a$b                          # duplicate column -> rank-deficient
  model_a <- stats::lm(y ~ ., data=df_a)
  R_a <- qr.R(model_a$qr)
  aliased_col <- which.min(abs(diag(R_a)))  # pivoted-last near-zero diagonal
  testthat::expect_true(aliased_col > 1L)
  model_a$qr$qr[1L, aliased_col] <- NaN     # off-diagonal aliased entry
  # Precondition (version-independent): qr.R now holds a non-finite value.
  testthat::expect_true(any(!is.finite(qr.R(model_a$qr))))

  newdata_a <- data.frame(a=stats::rnorm(5), b=stats::rnorm(5),
                          c=stats::rnorm(5), d=stats::rnorm(5))

  testthat::skip_if(getRversion() < "4.4.0")
  # Pre-fix path (default predict.lm) genuinely crashes on this model.
  testthat::expect_error(stats::predict.lm(model_a, newdata=newdata_a))
  # The helper returns a finite numeric vector of the right length...
  preds_a <- olsPredictRankSafe(model_a, newdata_a)
  testthat::expect_true(all(is.finite(preds_a)))
  testthat::expect_equal(length(preds_a), nrow(newdata_a))
  # ...and does so without surfacing the stock rank-deficient warning.
  testthat::expect_no_warning(olsPredictRankSafe(model_a, newdata_a))

  # (b) Warning muffle on a normal-scale rank-deficient design (duplicated
  # column): predict.lm warns but does NOT crash. The helper must emit no
  # "rank-deficient fit" warning and still return finite predictions.
  set.seed(51422)
  n_b <- 30
  df_b <- data.frame(y=stats::rnorm(n_b), a=stats::rnorm(n_b),
                     b=stats::rnorm(n_b), c=stats::rnorm(n_b))
  df_b$d <- df_b$b
  model_b <- stats::lm(y ~ ., data=df_b)
  newdata_b <- data.frame(a=stats::rnorm(6), b=stats::rnorm(6),
                          c=stats::rnorm(6), d=stats::rnorm(6))
  # Sanity: the stock default warns on this design (confirms the muffle is real).
  testthat::expect_warning(stats::predict.lm(model_b, newdata=newdata_b),
                           "rank-deficient fit")
  testthat::expect_no_warning(preds_b <- olsPredictRankSafe(model_b,
                                                                    newdata_b))
  testthat::expect_true(all(is.finite(preds_b)))
  testthat::expect_equal(length(preds_b), nrow(newdata_b))
})

testthat::test_that("css and getCssPreds reject non-finite (Inf) inputs (#99)", {
  set.seed(99001)

  X <- matrix(stats::rnorm(10 * 6), nrow = 10, ncol = 6)
  y <- stats::rnorm(10)
  good_clusters <- list("red" = 1:2, "blue" = 3:4, "green" = 5)

  # One Inf cell in the css design matrix now errors (previously: silent
  # corruption of the selection proportions).
  X_inf <- X
  X_inf[3, 2] <- Inf
  testthat::expect_error(
    css(X = X_inf, y = y, lambda = 0.01, clusters = good_clusters, B = 10),
    "must not contain missing", fixed = TRUE)

  # A valid css fit; then an Inf in testX passed to getCssPreds must error too.
  # testX is gated via checkGetCssPredsInputs -> checkXInputResults(testX) ->
  # checkNoNAs(newx, "newx"), so the message names "newx", not "testX"; assert
  # the stable non-finite substring instead.
  css_res <- css(X = X, y = y, lambda = 0.01, clusters = good_clusters, B = 10)

  x_train <- matrix(stats::rnorm(8 * 6), nrow = 8, ncol = 6)
  y_train <- stats::rnorm(8)
  x_pred <- matrix(stats::rnorm(7 * 6), nrow = 7, ncol = 6)

  # Sanity: clean inputs still predict finite values.
  preds_ok <- getCssPreds(css_res, testX = x_pred, trainX = x_train,
                          trainY = y_train)
  testthat::expect_true(all(is.finite(preds_ok)))

  x_pred_inf <- x_pred
  x_pred_inf[1, 1] <- Inf
  testthat::expect_error(
    getCssPreds(css_res, testX = x_pred_inf, trainX = x_train,
                trainY = y_train),
    "must not contain missing", fixed = TRUE)
})

testthat::test_that("the lasso entry points reject non-finite y (#100)", {
  set.seed(100001)

  X <- matrix(stats::rnorm(15 * 6), nrow = 15, ncol = 6)
  y <- stats::rnorm(15)
  good_clusters <- list("red" = 1:3, "green" = 4:6)
  test_X <- matrix(stats::rnorm(7 * 6), nrow = 7, ncol = 6)

  y_na <- y; y_na[4] <- NA
  y_inf <- y; y_inf[9] <- Inf

  # --- getLassoLambda nondeterminism pin -------------------------------------
  # getLassoLambda fits cv.glmnet on a RANDOM subsample (sample(1:n, ...)), so
  # before this fix a non-finite y in a single cell errored only on the draws
  # that happened to include it. checkFiniteY runs BEFORE that sample(), so it
  # must now error under EVERY seed -- never returning a stray lambda.
  for(s in c(1L, 2L, 7L, 13L, 101L, 2024L)){
    set.seed(s)
    testthat::expect_error(getLassoLambda(X = X, y = y_na, nfolds = 4),
      "must not contain missing", fixed = TRUE)
    set.seed(s)
    testthat::expect_error(getLassoLambda(X = X, y = y_inf, nfolds = 4),
      "must not contain missing", fixed = TRUE)
  }

  # A clean y still returns a valid lambda (no success -> error regression).
  set.seed(100002)
  lam_ok <- getLassoLambda(X = X, y = y, nfolds = 4)
  testthat::expect_true(is.numeric(lam_ok) && length(lam_ok) == 1 &&
                          !is.na(lam_ok) && lam_ok >= 0)

  # --- entry-point integration ----------------------------------------------
  testthat::expect_error(cssSelect(X = X, y = y_na),
    "must not contain missing", fixed = TRUE)
  testthat::expect_error(cssPredict(X_train_selec = X, y_train_selec = y_na,
                                    X_test = test_X),
    "must not contain missing", fixed = TRUE)
  testthat::expect_error(getModelSize(X = X, y = y_inf, clusters = good_clusters),
    "must not contain missing", fixed = TRUE)

  # protolasso / clusterRepLasso route through processClusterLassoInputs, whose
  # stopifnot(all(is.finite(y))) now catches Inf (previously all(!is.na(y))).
  testthat::expect_error(protolasso(X = X, y = y_inf, clusters = good_clusters),
    "is.finite", fixed = TRUE)
  testthat::expect_error(clusterRepLasso(X = X, y = y_inf,
                                         clusters = good_clusters),
    "is.finite", fixed = TRUE)

  # css()'s default cssLasso path validates each random subsample via
  # checkCssLassoInputs. A single non-finite cell would fire only on the
  # subsamples that draw it; set enough Inf cells (> n - floor(n/2)) that EVERY
  # size-floor(n/2) subsample must contain one, so css errors deterministically.
  y_many_inf <- y; y_many_inf[1:9] <- Inf
  testthat::expect_error(
    css(X = X, y = y_many_inf, lambda = 0.01, clusters = good_clusters, B = 10),
    "non-finite", fixed = TRUE)

  # Clean finite y still selects without error.
  res_ok <- cssSelect(X = X, y = y, clusters = good_clusters)
  testthat::expect_true(is.list(res_ok))
})

testthat::test_that("checkGenClusteredDataInputs works", {
  set.seed(7612)

  # Should get no error
  checkGenClusteredDataInputs(p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                           sig_clusters=2, rho=.8, beta_latent=1.5,
                           beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  
  checkGenClusteredDataInputs(p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                           sig_clusters=2, rho=.8, beta_latent=1.5,
                           beta_unclustered=-2, snr=1, sigma_eps_sq=NA)
  
  # sig_clusters
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters="2", rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(sig_clusters) | is.integer(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=4, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters <= n_clusters is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=-1, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters >= 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=.6, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters == round(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  
  # n_clusters
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5,
                                                  n_clusters="3",
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(n_clusters) | is.integer(n_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3.2,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters == round(n_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=0,
                                                  sig_clusters=0, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters >= 1 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=1, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "cluster_size >= 2 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=16, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "p >= n_clusters * cluster_size + k_unclustered is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "rho > 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=0,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_latent != 0 is not TRUE", fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=0, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_unclustered != 0 is not TRUE", fixed=TRUE)
  
  # k_unclustered
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered="2",
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(k_unclustered) | is.integer(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=0,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2.2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered == round(k_unclustered) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=NA),
                         "Must specify one of snr or sigma_eps_sq", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=-.2,
                                                  sigma_eps_sq=NA),
                         "snr > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(checkGenClusteredDataInputs(p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=-.3),
                         "sigma_eps_sq >= 0 is not TRUE", fixed=TRUE)
  
})

testthat::test_that("getNoiseVar works", {
  # getNoiseVar(cor) returns the noise variance v such that a proxy Z + N(0, v)
  # has correlation cor with Z, i.e. v = 1/cor^2 - 1 (vectorized; cor in (0, 1]).
  testthat::expect_equal(getNoiseVar(1), 0)
  testthat::expect_equal(getNoiseVar(0.5), 3)
  testthat::expect_equal(getNoiseVar(0.9), 1/0.81 - 1)
  testthat::expect_equal(getNoiseVar(c(0.5, 1, 0.9)), c(3, 0, 1/0.81 - 1))
  # The defining property: a proxy with this noise variance has correlation cor.
  testthat::expect_equal(1/sqrt(1 + getNoiseVar(0.8)), 0.8)

  # cor must be numeric, non-NA, non-empty, and in (0, 1] (checks are vectorized).
  testthat::expect_error(getNoiseVar(0), "all(cor > 0) is not TRUE", fixed=TRUE)
  testthat::expect_error(getNoiseVar(-0.5), "all(cor > 0) is not TRUE",
    fixed=TRUE)
  testthat::expect_error(getNoiseVar(c(0.5, 0)), "all(cor > 0) is not TRUE",
    fixed=TRUE)
  testthat::expect_error(getNoiseVar(1.5), "all(cor <= 1) is not TRUE",
    fixed=TRUE)
  testthat::expect_error(getNoiseVar(NA_real_), "all(!is.na(cor)) is not TRUE",
    fixed=TRUE)
  testthat::expect_error(getNoiseVar("0.5"),
    "is.numeric(cor) | is.integer(cor) is not TRUE", fixed=TRUE)
  testthat::expect_error(getNoiseVar(numeric(0)), "length(cor) >= 1 is not TRUE",
    fixed=TRUE)
})

testthat::test_that("genClusteredData works", {
  set.seed(23478)

  ret <- genClusteredData(n=25, p=19, k_unclustered=2, cluster_size=5,
                          n_clusters=3, sig_clusters=2, rho=.99,
                          beta_latent=1.5, beta_unclustered=-2, snr=NA,
                          sigma_eps_sq=.5)
  
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
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5,
                          n_clusters=3, sig_clusters=2, rho=.8, beta_latent=1.5,
                          beta_unclustered=-2, snr=1, sigma_eps_sq=NA)
  
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  
  # Specifying both snr and sigma_eps_sq is an error (issue #13): only one of
  # the two may be given.
  testthat::expect_error(
    genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5, n_clusters=3,
                     sig_clusters=2, rho=.8, beta_latent=1.5,
                     beta_unclustered=-2, snr=.01, sigma_eps_sq=.25),
    "Only one of snr and sigma_eps_sq may be specified", fixed=TRUE)
  
  # Try a single latent variable (z should be a one-column matrix)
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5,
                          n_clusters=1, sig_clusters=1, rho=.8, beta_latent=1.5,
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
                                          sig_clusters="2", rho=.8,
                                          beta_latent=1.5, beta_unclustered=-2,
                                          snr=1, sigma_eps_sq=NA),
                         "is.numeric(sig_clusters) | is.integer(sig_clusters) is not TRUE",
                         fixed=TRUE)
  
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                          cluster_size=5, n_clusters=3,
                                          sig_clusters=4, rho=.8,
                                          beta_latent=1.5, beta_unclustered=-2,
                                          snr=1, sigma_eps_sq=NA),
                         "sig_clusters <= n_clusters is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                          cluster_size=5, n_clusters=3,
                                          sig_clusters=-1, rho=.8,
                                          beta_latent=1.5, beta_unclustered=-2,
                                          snr=1, sigma_eps_sq=NA),
                         "sig_clusters >= 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=.6, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "sig_clusters == round(sig_clusters) is not TRUE",
                         fixed=TRUE)


  # n_clusters
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5,
                                                  n_clusters="3",
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(n_clusters) | is.integer(n_clusters) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3.2,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters == round(n_clusters) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=0,
                                                  sig_clusters=0, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "n_clusters >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=1, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "cluster_size >= 2 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(p=16, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "p >= n_clusters * cluster_size + k_unclustered is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "rho > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=0,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_latent != 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=0, snr=1,
                                                  sigma_eps_sq=NA),
                         "beta_unclustered != 0 is not TRUE", fixed=TRUE)

  # k_unclustered
  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered="2",
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "is.numeric(k_unclustered) | is.integer(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=0,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered >= 1 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2.2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=1,
                                                  sigma_eps_sq=NA),
                         "k_unclustered == round(k_unclustered) is not TRUE",
                         fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=NA),
                         "Must specify one of snr or sigma_eps_sq", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=-.2,
                                                  sigma_eps_sq=NA),
                         "snr > 0 is not TRUE", fixed=TRUE)

  testthat::expect_error(genClusteredData(n=5, p=19, k_unclustered=2,
                                                  cluster_size=5, n_clusters=3,
                                                  sig_clusters=2, rho=0.8,
                                                  beta_latent=1.5,
                                                  beta_unclustered=-2, snr=NA,
                                                  sigma_eps_sq=-.3),
                         "sigma_eps_sq >= 0 is not TRUE", fixed=TRUE)

  # sigma_eps_sq = 0 is allowed (issue #13) and yields noiseless data (y == mu)
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5,
                          n_clusters=3, sig_clusters=2, rho=.8, beta_latent=1.5,
                          beta_unclustered=-2, snr=NA, sigma_eps_sq=0)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ret$y, ret$mu)

  # k_unclustered = 1 is allowed (issue #13)
  ret <- genClusteredData(n=5, p=19, k_unclustered=1, cluster_size=5,
                          n_clusters=3, sig_clusters=2, rho=.8, beta_latent=1.5,
                          beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(nrow(ret$X), 5)
  testthat::expect_equal(ncol(ret$X), 19)

})

testthat::test_that("genClusteredDataWeighted works", {
  set.seed(23478)

  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_strong_cluster_vars=3, n_clusters=3,
                                  sig_clusters=2, rho_high=.99, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)

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
  testthat::expect_true(min(cor(ret$X[, 1:3])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 1:5], ret$X[, 6:19]))) < .6)

  testthat::expect_true(min(cor(ret$X[, 6:8])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 6:10],
                                    ret$X[, c(1:5, 11:19)]))) < .6)

  testthat::expect_true(min(cor(ret$X[, 11:13])) > .9)
  testthat::expect_true(max(abs(cor(ret$X[, 11:15],
                                    ret$X[, c(1:10, 16:19)]))) < .7)

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
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_strong_cluster_vars=3, n_clusters=3,
                                  sig_clusters=2, rho_high=.99, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=1, sigma_eps_sq=NA)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))

  # Specifying both snr and sigma_eps_sq is an error (issue #13): only one of
  # the two may be given.
  testthat::expect_error(
    genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
                             n_strong_cluster_vars=3, n_clusters=3,
                             sig_clusters=2, rho_high=.99, rho_low=.5,
                             beta_latent=1.5, beta_unclustered=-2,
                             snr=.01, sigma_eps_sq=.25),
    "Only one of snr and sigma_eps_sq may be specified", fixed=TRUE)

  # Try a single latent variable (z should be a one-column matrix)
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_strong_cluster_vars=3, n_clusters=1,
                                  sig_clusters=1, rho_high=.99, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))

  testthat::expect_true(is.numeric(ret$Z))
  testthat::expect_true(is.matrix(ret$Z))
  testthat::expect_equal(nrow(ret$Z), 25)
  testthat::expect_equal(ncol(ret$Z), 1)

  # sigma_eps_sq = 0 is allowed (issue #13) and yields noiseless data (y == mu)
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_strong_cluster_vars=3, n_clusters=3,
                                  sig_clusters=2, rho_high=.99, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=0)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ret$y, ret$mu)

  # k_unclustered = 1 is allowed (issue #13)
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=1, cluster_size=5,
                                  n_strong_cluster_vars=3, n_clusters=3,
                                  sig_clusters=2, rho_high=.99, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ncol(ret$X), 19)

})

testthat::test_that("genClusteredDataWeightedRandom works", {
  set.seed(23478)

  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_clusters=3,
                                  sig_clusters=2, rho_high=1, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)

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
  testthat::expect_true(min(cor(ret$X[, 1:3])) > .2)
  testthat::expect_true(max(abs(cor(ret$X[, 1:5], ret$X[, 6:19]))) < .6)

  testthat::expect_true(min(cor(ret$X[, 6:8])) > .2)
  testthat::expect_true(max(abs(cor(ret$X[, 6:10],
                                    ret$X[, c(1:5, 11:19)]))) < .6)

  testthat::expect_true(min(cor(ret$X[, 11:13])) > .2)
  testthat::expect_true(max(abs(cor(ret$X[, 11:15],
                                    ret$X[, c(1:10, 16:19)]))) < .7)

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
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_clusters=3,
                                  sig_clusters=2, rho_high=1, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=1, sigma_eps_sq=NA)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))

  # Specifying both snr and sigma_eps_sq is an error (issue #13): only one of
  # the two may be given.
  testthat::expect_error(
    genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
                                   n_clusters=3,
                                   sig_clusters=2, rho_high=.99, rho_low=.5,
                                   beta_latent=1.5, beta_unclustered=-2,
                                   snr=.01, sigma_eps_sq=.25),
    "Only one of snr and sigma_eps_sq may be specified", fixed=TRUE)

  # Try a single latent variable (z should be a one-column matrix)
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_clusters=1,
                                  sig_clusters=1, rho_high=1, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)

  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))

  testthat::expect_true(is.numeric(ret$Z))
  testthat::expect_true(is.matrix(ret$Z))
  testthat::expect_equal(nrow(ret$Z), 25)
  testthat::expect_equal(ncol(ret$Z), 1)

  # sigma_eps_sq = 0 is allowed (issue #13) and yields noiseless data (y == mu)
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
                                  n_clusters=3,
                                  sig_clusters=2, rho_high=1, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=0)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ret$y, ret$mu)

  # k_unclustered = 1 is allowed (issue #13)
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=1, cluster_size=5,
                                  n_clusters=3,
                                  sig_clusters=2, rho_high=1, rho_low=.5,
                                  beta_latent=1.5, beta_unclustered=-2,
                                  snr=NA, sigma_eps_sq=.5)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ncol(ret$X), 19)

})

# Characterization snapshots: pin the EXACT generated values (X / y / Z / mu)
# under fixed seeds so the #16 consolidation of the genClusteredData* functions
# and their validators is provably behavior-preserving. The existing "... works"
# tests only check distributional properties (correlations, ranges), NOT the
# exact RNG draw order, which differs across the three generators (bulk vs
# per-column vs random-per-column). Regenerate ONLY if the data-generating
# algorithm intentionally changes.

testthat::test_that("genClusteredData output is byte-stable (characterization snapshot, #16)", {
  set.seed(23478)
  ret <- genClusteredData(n=25, p=19, k_unclustered=2, cluster_size=5,
    n_clusters=3, sig_clusters=2, rho=.99, beta_latent=1.5, beta_unclustered=-2,
    snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-1.02641989570528,
    -0.113749750828501, -0.0540798505887575, -0.719018251689331,
    -1.02002920513898, -0.133167075802704, -0.228504275273568,
    -0.594853445315136, -1.3743321671407, -0.691923420713347, -0.11653157345392,
    -0.949501763500117, -1.09911307059764, -0.234373142554013,
    -0.0610136281766905, -0.614831158775111, -0.875588846521461,
    -0.267096146835881, 0.243882464912086, -0.525573959766865, 0.38889466127034,
    -0.196999108158916, -0.548580993298435, 1.29708342622908), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(1.78818155412284, 2.55825580753328,
    0.0881535457958548, 1.21771079504409))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538,
    0.402628944106856, -0.0568963388614887, -0.533358544824988, 1.23055042537552,
    0.495888208134907, 2.19477907424054, 0.357725198858587, -0.863633909554439),
    dim = 4:3))
  testthat::expect_equal(ret$mu[1:4], c(1.12211695300006, 3.29507281236772,
    -0.106753094432726, 1.52394871772745))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = 7.85857128981734, sumY = 13.5810460445817, sumZ = 3.15823002771053,
      sumMu = 9.93005749962466))

  # single latent variable (n_clusters = 1): Z is a one-column matrix, and the
  # proxy block adds bulk noise to the raw vector Z.
  set.seed(23478)
  ret <- genClusteredData(n=5, p=19, k_unclustered=2, cluster_size=5,
    n_clusters=1, sig_clusters=1, rho=.8, beta_latent=1.5, beta_unclustered=-2,
    snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-0.00161764234876749,
    -0.557794858227907, -0.827386446973581, -0.460377784885637, -2.67619233265038,
    0.671711827459298, 1.65236925524076, -0.600115109545631, -0.280741265470752,
    -1.67026187144442, -0.0473803806232191, 0.0826508004462609, 0.170200984778884,
    0.976190994120857, -0.499749723868518, -0.474295388720649, -1.17346153974724,
    -0.574655697546479, -0.120791507647279, -0.567678976454219, -1.3596277359555,
    -0.0619416164679036, 0.538496824968004, 0.51591446765952), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(-0.558336409902388, -2.73369761842788,
    -0.884087327686163, -3.17343267752372))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538),
    dim = c(4L, 1L)))
  testthat::expect_equal(ret$mu[1:4], c(0.195781398124046, -1.51661095607912,
    -0.722190912632797, -2.90119777610212))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = -5.88971766152033, sumY = -7.36101068003228, sumZ = -2.4782435456402,
      sumMu = -5.08843939501818))
})

testthat::test_that("genClusteredDataWeighted output is byte-stable (characterization snapshot, #16)", {
  set.seed(23478)
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
    n_strong_cluster_vars=3, n_clusters=3, sig_clusters=2, rho_high=.99,
    rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-1.02641989570528,
    -0.113749750828501, -0.0540798505887575, -0.719018251689331,
    -1.02002920513898, -0.133167075802704, -0.228504275273568,
    -0.594853445315136, -1.3743321671407, -0.691923420713347, -0.11653157345392,
    -0.949501763500117, -0.748805376677026, 0.925796953277494, -0.766705940388196,
    -0.348877774329742, 1.96822115764394, 0.528035720534176, 2.93942827641455,
    0.736079254765787, 0.38889466127034, -0.196999108158916, -0.548580993298435,
    1.29708342622908), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(1.78818155412284, 2.55825580753328,
    0.0881535457958548, 1.21771079504409))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538,
    0.402628944106856, -0.0568963388614887, -0.533358544824988, 1.23055042537552,
    0.495888208134907, 2.19477907424054, 0.357725198858587, -0.863633909554439),
    dim = 4:3))
  testthat::expect_equal(ret$mu[1:4], c(1.12211695300006, 3.29507281236772,
    -0.106753094432726, 1.52394871772745))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = 15.3829830134111, sumY = 13.5810460445817, sumZ = 3.15823002771053,
      sumMu = 9.93005749962466))

  # single latent variable (n_clusters = 1)
  set.seed(23478)
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
    n_strong_cluster_vars=3, n_clusters=1, sig_clusters=1, rho_high=.99,
    rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-1.07114417787482,
    -0.368013007285506, 0.014438919713251, -0.845503147886515, -1.23739163512048,
    -0.174811089926991, -0.0829364724064908, -0.479069797377207, -1.12138274604786,
    -0.402828108682171, 0.104291325515473, -0.844159398768831, 0.352430473911601,
    -1.4960796572646, -1.23544829374631, 0.148162490964714, -1.03021388828778,
    0.576416731940481, -0.513340292522171, -1.14008022670496, 0.402628944106856,
    -0.0568963388614887, -0.533358544824988, 1.23055042537552), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(-3.91149964768012, -4.73964429926795,
    0.513991031913566, -2.95972727928473))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538),
    dim = c(4L, 1L)))
  testthat::expect_equal(ret$mu[1:4], c(-3.20232310305676, -3.49765450037239,
    0.564187042362111, -2.19774596140566))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = -24.0234160115409, sumY = -10.2198990332918, sumZ = 0.586596841562748,
      sumMu = -5.15679097838167))
})

testthat::test_that("genClusteredDataWeightedRandom output is byte-stable (characterization snapshot, #16)", {
  set.seed(23478)
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2,
    cluster_size=5, n_clusters=3, sig_clusters=2, rho_high=1, rho_low=.5,
    beta_latent=1.5, beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-1.1897691657824,
    -0.00982041549935259, 0.665172439447775, -0.756543570417209, 1.15949678203449,
    -2.91344510959381, 0.491240389956153, -1.30476423524191, -0.485131132932798,
    0.707045727376943, 3.2222522503854, -3.27350445562509, -1.85428176680484,
    -0.0656090373877866, -0.551130785327929, -1.90090513408521, -1.29616909361208,
    -0.144918159176611, 0.544180897409365, -0.50148178532334, 0.627200376817289,
    0.171599744439457, -1.07507438640901, 1.5693792481444), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(1.78818155412284, 2.55825580753328,
    0.0881535457958548, 1.21771079504409))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538,
    0.402628944106856, -0.0568963388614887, -0.533358544824988, 1.23055042537552,
    0.495888208134907, 2.19477907424054, 0.357725198858587, -0.863633909554439),
    dim = 4:3))
  testthat::expect_equal(ret$mu[1:4], c(1.12211695300006, 3.29507281236772,
    -0.106753094432726, 1.52394871772745))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = 3.23025166163967, sumY = 13.5810460445817, sumZ = 3.15823002771053,
      sumMu = 9.93005749962466))

  # single latent variable (n_clusters = 1)
  set.seed(23478)
  ret <- genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2,
    cluster_size=5, n_clusters=1, sig_clusters=1, rho_high=1, rho_low=.5,
    beta_latent=1.5, beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_equal(ret$X[1:4, 1:6], structure(c(-1.36526166060881,
    0.586409931283558, 1.2227469255493, -2.17655371414979, 0.326418806550987,
    -1.09714072579561, 1.4239012653582, -0.760011975131564, -0.227256340192126,
    -1.84845832529723, 0.184388770399019, -0.394752299747441, -1.47062212216842,
    -0.12215942773548, -0.0749451055463801, -0.842364831584477, -1.41223753691949,
    -1.68487949598676, 0.247717311839652, -1.86068883953814, 0.402628944106856,
    -0.0568963388614887, -0.533358544824988, 1.23055042537552), dim = c(4L, 6L)))
  testthat::expect_equal(ret$y[1:4], c(-3.91149964768012, -4.73964429926795,
    0.513991031913566, -2.95972727928473))
  testthat::expect_equal(ret$Z[1:4, , drop = FALSE], structure(c(-1.13051559031852,
    -0.338373896594486, 0.00224652036037424, -0.638671932314538),
    dim = c(4L, 1L)))
  testthat::expect_equal(ret$mu[1:4], c(-3.20232310305676, -3.49765450037239,
    0.564187042362111, -2.19774596140566))
  testthat::expect_equal(
    c(sumX=sum(ret$X), sumY=sum(ret$y), sumZ=sum(ret$Z), sumMu=sum(ret$mu)),
    c(sumX = -28.8273582900822, sumY = -10.2198990332918, sumZ = 0.586596841562748,
      sumMu = -5.15679097838167))
})

testthat::test_that("genClusteredData* reject n < 2 and non-integer cluster_size / p (#70)", {
  ok <- list(n = 10, p = 8, k_unclustered = 2, cluster_size = 3, n_clusters = 1,
             sig_clusters = 1, sigma_eps_sq = 1)
  testthat::expect_error(do.call(genClusteredData, utils::modifyList(ok, list(n = 1L))),
                         "n >= 2", fixed = TRUE)
  testthat::expect_error(do.call(genClusteredData, utils::modifyList(ok, list(cluster_size = 2.5))),
                         "cluster_size == round(cluster_size)", fixed = TRUE)
  testthat::expect_error(do.call(genClusteredData, utils::modifyList(ok, list(p = 8.5))),
                         "p == round(p)", fixed = TRUE)
  # n = 2 is the tight boundary -- it must still WORK (locks the threshold
  # against a future "n >= 3" / "> 2" regression).
  res2 <- do.call(genClusteredData, utils::modifyList(ok, list(n = 2L)))
  testthat::expect_equal(nrow(res2$X), 2L)
  # The shared genZmuY n-guard and the shared Pre cluster_size check cover the
  # weighted generators too:
  wok <- list(n = 1L, p = 8, k_unclustered = 2, cluster_size = 3, n_clusters = 1,
              n_strong_cluster_vars = 2, sig_clusters = 1, rho_high = 0.8,
              rho_low = 0.2, sigma_eps_sq = 1)
  testthat::expect_error(do.call(genClusteredDataWeighted, wok),
                         "n >= 2", fixed = TRUE)
  testthat::expect_error(
    do.call(genClusteredDataWeighted, utils::modifyList(wok, list(n = 10L, cluster_size = 2.5))),
    "cluster_size == round(cluster_size)", fixed = TRUE)
})

testthat::test_that("genClusteredDataWeighted/Random validate snr/sigma_eps_sq type (#35)", {
  # Newly rejected (these previously slipped past the weighted validators' lean
  # checks and only failed later, cryptically, inside genZmuY).
  testthat::expect_error(
    genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_strong_cluster_vars=3, n_clusters=3, sig_clusters=2, rho_high=.99,
      rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr="1",
      sigma_eps_sq=NA),
    "is.numeric(snr) | is.integer(snr) is not TRUE", fixed=TRUE)
  testthat::expect_error(
    genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_strong_cluster_vars=3, n_clusters=3, sig_clusters=2, rho_high=.99,
      rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr=NA,
      sigma_eps_sq="0.5"),
    "is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq) is not TRUE", fixed=TRUE)
  testthat::expect_error(
    genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_clusters=3, sig_clusters=2, rho_high=1, rho_low=.5, beta_latent=1.5,
      beta_unclustered=-2, snr="1", sigma_eps_sq=NA),
    "is.numeric(snr) | is.integer(snr) is not TRUE", fixed=TRUE)
  testthat::expect_error(
    genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_clusters=3, sig_clusters=2, rho_high=1, rho_low=.5, beta_latent=1.5,
      beta_unclustered=-2, snr=NA, sigma_eps_sq="0.5"),
    "is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq) is not TRUE", fixed=TRUE)

  # Range checks (snr > 0, sigma_eps_sq >= 0) are unchanged by #35.
  testthat::expect_error(
    genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_strong_cluster_vars=3, n_clusters=3, sig_clusters=2, rho_high=.99,
      rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr=-.2,
      sigma_eps_sq=NA),
    "snr > 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(
    genClusteredDataWeightedRandom(n=25, p=19, k_unclustered=2, cluster_size=5,
      n_clusters=3, sig_clusters=2, rho_high=1, rho_low=.5, beta_latent=1.5,
      beta_unclustered=-2, snr=NA, sigma_eps_sq=-.3),
    "sigma_eps_sq >= 0 is not TRUE", fixed=TRUE)

  # A valid scalar numeric snr / sigma_eps_sq is still accepted.
  ret <- genClusteredDataWeighted(n=25, p=19, k_unclustered=2, cluster_size=5,
    n_strong_cluster_vars=3, n_clusters=3, sig_clusters=2, rho_high=.99,
    rho_low=.5, beta_latent=1.5, beta_unclustered=-2, snr=NA, sigma_eps_sq=.5)
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
})

testthat::test_that("genZmuY one-weak-signal config no longer crashes, all 3 generators (#124 bug 1)", {
  # p == n_clusters*cluster_size + 1 leaves exactly ONE weak-signal/noise
  # feature, so other_X had a single column; without drop = FALSE it collapsed
  # to a vector and other_X[, j] errored "incorrect number of dimensions". The
  # pre-check only requires p_orig_feat_mat >= k_unclustered + n_clusters
  # (here 2 >= 2), so it passed straight into the crash.
  ret <- genClusteredData(n = 20, p = 3, k_unclustered = 1, cluster_size = 2,
                          n_clusters = 1, snr = 2)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_true(is.matrix(ret$X))
  testthat::expect_equal(ncol(ret$X), 3)
  testthat::expect_equal(nrow(ret$X), 20)

  # genClusteredDataWeighted: n_strong_cluster_vars is REQUIRED and its checker
  # enforces >= 1 AND < cluster_size, so with cluster_size = 2 the only valid
  # value is 1.
  retW <- genClusteredDataWeighted(n = 20, p = 3, k_unclustered = 1,
                                   cluster_size = 2, n_clusters = 1,
                                   n_strong_cluster_vars = 1, snr = 2)
  testthat::expect_true(is.list(retW))
  testthat::expect_identical(names(retW), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ncol(retW$X), 3)

  retR <- genClusteredDataWeightedRandom(n = 20, p = 3, k_unclustered = 1,
                                         cluster_size = 2, n_clusters = 1,
                                         snr = 2)
  testthat::expect_true(is.list(retR))
  testthat::expect_identical(names(retR), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ncol(retR$X), 3)
})

testthat::test_that("genZmuY sig_clusters == 0 yields a null latent model (#124 bug 2)", {
  # The zero-beta case MUST bypass genClusteredData: its validator rejects
  # beta_unclustered == 0 / beta_latent == 0, so exercise the internal helper
  # directly. sig_clusters == 0 AND beta_unclustered == 0 => mu is exactly 0
  # (no latent term, no weak-signal term).
  set.seed(53)
  g0 <- genZmuY(n = 20, p = 6, k_unclustered = 1, cluster_size = 2,
                       n_clusters = 2, sig_clusters = 0, beta_latent = 1.5,
                       beta_unclustered = 0, snr = NA, sigma_eps_sq = 0.5)
  testthat::expect_true(all(g0$mu == 0))

  # Contrast, n_clusters > 1: reset the seed before EACH call so the SAME
  # orig_feat_mat (hence the same Z and weak-signal block) is drawn; then
  # g1$mu - g0$mu isolates the pure latent term Z[, 1] * beta_latent.
  cfgA <- list(n = 20, p = 6, k_unclustered = 1, cluster_size = 2,
               n_clusters = 2, beta_latent = 1.5, beta_unclustered = 1,
               snr = NA, sigma_eps_sq = 0.5)
  set.seed(51)
  g0A <- do.call(genZmuY, c(cfgA, sig_clusters = 0))
  set.seed(51)
  g1A <- do.call(genZmuY, c(cfgA, sig_clusters = 1))
  testthat::expect_equal(g1A$Z, g0A$Z)              # same orig_feat_mat drawn
  testthat::expect_false(all(g0A$mu == 0))          # weak signal still retained
  testthat::expect_equal(g1A$mu - g0A$mu, as.numeric(g1A$Z[, 1] * 1.5))
  # Lock the n_clusters > 1 & sig_clusters == 1 branch with an explicit
  # expected value (no characterization snapshot covers it).
  testthat::expect_equal(g1A$mu[1:4], c(1.71187484261165, -1.8816097377914,
    -0.434623721956133, 0.201053072650191))

  # Contrast, n_clusters == 1 (Z is a bare vector; the else branch).
  cfgB <- list(n = 20, p = 4, k_unclustered = 1, cluster_size = 2,
               n_clusters = 1, beta_latent = 1.5, beta_unclustered = 1,
               snr = NA, sigma_eps_sq = 0.5)
  set.seed(52)
  g0B <- do.call(genZmuY, c(cfgB, sig_clusters = 0))
  set.seed(52)
  g1B <- do.call(genZmuY, c(cfgB, sig_clusters = 1))
  testthat::expect_equal(g1B$mu - g0B$mu, as.numeric(g1B$Z * 1.5))
  testthat::expect_equal(g1B$mu[1:4], c(-1.35778032140589, 0.59232273049822,
    2.44822804281879, -0.29256382836018))

  # End-to-end: genClusteredData with sig_clusters = 0 and NONZERO default
  # betas returns valid data (no error; the weak-signal features still
  # legitimately contribute, only the latent part is zeroed).
  ret <- genClusteredData(n = 20, p = 6, k_unclustered = 1, cluster_size = 2,
                          n_clusters = 2, sig_clusters = 0, sigma_eps_sq = 0.5)
  testthat::expect_true(is.list(ret))
  testthat::expect_identical(names(ret), c("X", "y", "Z", "mu"))
  testthat::expect_equal(ncol(ret$X), 6)
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
  
  # Different mixing parameter
  
   ret <- getLassoLambda(X=x, y=y, lambda_choice="min", nfolds=5, alpha=0.5)
  
  testthat::expect_true(is.numeric(ret) | is.integer(ret))
  testthat::expect_true(!is.na(ret))
  testthat::expect_equal(length(ret), 1)
  testthat::expect_true(ret >= 0)

  # data.frame X is now accepted (coerced internally), like the sibling exports (#51)
  ret_df <- getLassoLambda(X = as.data.frame(x), y = y, lambda_choice = "1se", nfolds = 4)
  testthat::expect_true(is.numeric(ret_df) | is.integer(ret_df))
  testthat::expect_true(!is.na(ret_df))
  testthat::expect_equal(length(ret_df), 1)
  testthat::expect_true(ret_df >= 0)

  # Bad inputs
  testthat::expect_error(getLassoLambda(X="x", y=y), "must be a numeric matrix",
                         fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x[1:9, ], y=y),
                         "n == length(y) is not TRUE", fixed=TRUE)
  
  testthat::expect_error(getLassoLambda(X=x, y=TRUE),
                         "must be a numeric (real-valued) vector",
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
  
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds=4, alpha=1.2),
                         "alpha <= 1 is not TRUE", fixed=TRUE)

  # alpha = 0 (degenerate ridge) is now rejected (previously allowed)
  testthat::expect_error(getLassoLambda(X=x, y=y, nfolds=4, alpha=0),
                         "alpha > 0 is not TRUE", fixed=TRUE)

  # An NA in the design matrix now yields a friendly message (#56) rather than
  # falling through to a cryptic glmnet-level error.
  x_na <- x; x_na[2, 1] <- NA
  testthat::expect_error(getLassoLambda(X=x_na, y=y),
                         "must not contain missing", fixed=TRUE)
  # A data.frame with a numeric NA must error with the same message BEFORE
  # coercion: model.matrix's na.action=na.omit would otherwise silently drop
  # the NA row and the failure would surface downstream (length mismatch), not
  # as this message. This case pins the pre-coercion placement of checkNoNAs.
  df_na <- as.data.frame(x); df_na[2, 1] <- NA
  testthat::expect_error(getLassoLambda(X=df_na, y=y),
                         "must not contain missing", fixed=TRUE)

  # (#104, L7) When n is too small for even 3-fold cross-validation, error
  # clearly here instead of crashing inside cv.glmnet ("nfolds must be bigger
  # than 3"). With n = 4 and the default nfolds = 10, n_sample rounds to 2 (< 3).
  testthat::expect_error(
    getLassoLambda(X=matrix(stats::rnorm(4*5), nrow=4, ncol=5),
                   y=stats::rnorm(4)),
    "is too small to choose lambda", fixed=TRUE)

  # (#127) A single-column X now fires the same clean "p >= 2 is not TRUE"
  # message as css(), rather than crashing later inside cv.glmnet.
  testthat::expect_error(
    getLassoLambda(X=matrix(stats::rnorm(40), nrow=40, ncol=1),
                   y=stats::rnorm(40)),
    "p >= 2 is not TRUE", fixed=TRUE)

})

testthat::test_that("out-of-range cluster indices error via css/protolasso (#104)", {
  set.seed(104105)
  X <- matrix(stats::rnorm(20*8), nrow=20, ncol=8)
  y <- stats::rnorm(20)

  # Bare-vector clusters with an index beyond p (= 5 here).
  testthat::expect_error(
    css(X[, 1:5], y, lambda=0.01, clusters=c(1, 2, 99)),
    "Cluster index 99 exceeds the number of features (p = 5).", fixed=TRUE)

  # List clusters routed through protolasso.
  testthat::expect_error(
    protolasso(X[, 1:5], y, clusters=list(c(1, 2, 99))),
    "Cluster index 99 exceeds the number of features (p = 5).", fixed=TRUE)
})

testthat::test_that("css reports a too-small sample size clearly (#104)", {
  set.seed(104104)

  # No train_inds: the message must blame the sample size, not training indices.
  testthat::expect_error(
    css(matrix(stats::rnorm(15), nrow=3, ncol=5), stats::rnorm(3), lambda=0.01),
    "is too small", fixed=TRUE)

  # When train_inds leave fewer than 4 observations for selection, the original
  # "Too many training indices" branch still fires.
  testthat::expect_error(
    css(matrix(stats::rnorm(10*5), nrow=10, ncol=5), stats::rnorm(10),
        lambda=0.01, train_inds=1:7),
    "Too many training indices provided", fixed=TRUE)
})

testthat::test_that("getModelSize works", {
  set.seed(1723)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=2, cluster_size=3,
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

  # Regression test: when prototype reduction leaves a single column (all
  # provided features in one cluster), getModelSize must return 1L rather than
  # crashing in cv.glmnet. Previously X_size[, -feats_to_drop] dropped to a
  # vector when one column remained.
  testthat::expect_equal(getModelSize(X=x[, 1:3, drop=FALSE], y=y,
                                      clusters=list(1:3)), 1L)


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
                         "must be a numeric (real-valued) vector",
                         fixed=TRUE)

  testthat::expect_error(getModelSize(X=x, y=y, clusters=list(3:7, 7:10)),
                         "Overlapping clusters detected; clusters must be non-overlapping. Overlapping clusters: 1, 2.",
                         fixed=TRUE)
  
  testthat::expect_error(getModelSize(X=x, y=y, clusters=list(5:8, 5:8)),
                         "length(clusters) == length(unique(clusters)) is not TRUE",
                         fixed=TRUE)

  # Out-of-range cluster index now caught up front by checkFormatClustersInput
  # (#104, L3); x has 11 columns, so index 50 is out of range.
  testthat::expect_error(getModelSize(X=x, y=y, clusters=6:50),
                         "Cluster index 50 exceeds the number of features (p = 11).",
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

  ## alpha (#21): the elastic-net mixing parameter now drives the model-size
  ## estimate (the internal cv.glmnet), keeping it consistent with elastic-net
  ## feature selection.

  # Validation, mirroring getLassoLambda: alpha must be in (0, 1].
  testthat::expect_error(getModelSize(X=x, y=y, clusters=good_clusters, alpha=0),
                         "alpha > 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(getModelSize(X=x, y=y, clusters=good_clusters, alpha=1.5),
                         "alpha <= 1 is not TRUE", fixed=TRUE)
  testthat::expect_error(getModelSize(X=x, y=y, clusters=good_clusters, alpha=NA),
                         "is.numeric(alpha) | is.integer(alpha) is not TRUE",
                         fixed=TRUE)

  # Back-compat: default is pure lasso (alpha = 1). cv.glmnet uses random CV
  # folds, so seed before each call to compare apples-to-apples.
  set.seed(7); a_default <- getModelSize(X=x, y=y, clusters=good_clusters)
  set.seed(7); a_alpha1 <- getModelSize(X=x, y=y, clusters=good_clusters, alpha=1)
  testthat::expect_equal(a_default, a_alpha1)

  # Headline: on a design with a block of highly correlated relevant features
  # (each feature its own singleton cluster, so cv.glmnet sees the whole block
  # with no prototype reduction), a lower alpha (elastic net) keeps more of the
  # correlated features than pure lasso, giving a strictly larger estimated
  # size. On main, getModelSize has no alpha argument, so this cannot happen.
  set.seed(2718)
  n_h <- 120
  z_h <- stats::rnorm(n_h)
  X_block <- matrix(rep(z_h, 10), nrow=n_h) +
    matrix(stats::rnorm(n_h * 10, sd=0.05), nrow=n_h)
  X_noise <- matrix(stats::rnorm(n_h * 4), nrow=n_h)
  X_h <- cbind(X_block, X_noise)
  y_h <- 2 * z_h + stats::rnorm(n_h, sd=0.5)
  singletons_h <- as.list(1:ncol(X_h))
  set.seed(11)
  size_lasso <- getModelSize(X=X_h, y=y_h, clusters=singletons_h, alpha=1)
  set.seed(11)
  size_enet <- getModelSize(X=X_h, y=y_h, clusters=singletons_h, alpha=0.2)
  testthat::expect_true(size_enet >= size_lasso)
  testthat::expect_true(size_enet > size_lasso)

})

testthat::test_that("getSelectionPrototypes works", {
  set.seed(67234)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=2, cluster_size=3,
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

  # An empty selection (empty list of selected clusters) is valid post-#107:
  # getSelectionPrototypes() returns an empty integer vector, no error (#120).
  testthat::expect_identical(getSelectionPrototypes(css_results, list()),
                             integer(0))

  testthat::expect_error(getSelectionPrototypes(css_results,
                                                list(red_cluster=1L:3L,
                                                     green_cluster=4L:6L,
                                                     bad_cluster=integer())),
                         "all(lengths(selected_clusts) >= 1) is not TRUE",
                         fixed=TRUE)

})

testthat::test_that("getSelectionPrototypes handles a constant tied column without crashing (#68)", {
  set.seed(680)
  n <- 20L; p <- 4L
  X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  X[, 2] <- 3.5                       # feature 2 is a CONSTANT column
  colnames(X) <- paste0("V", 1:p)
  y <- X[, 1] + stats::rnorm(n)       # y correlated with feature 1 (varying)
  # feat_sel_mat: features 1 and 2 tie at selection proportion 0.5 (both
  # "selected" in the same 5 of 10 subsamples), forcing the tie-break path.
  B <- 10L
  M <- matrix(0L, nrow = B, ncol = p)
  M[1:5, 1] <- 1L
  M[1:5, 2] <- 1L
  css_results <- structure(list(X = X, y = y, feat_sel_mat = M), class = "cssr")
  # Pre-fix: cor(X[, c(1,2)], y) -> c(<corr>, NA) -> max NA -> stopifnot abort.
  proto <- getSelectionPrototypes(css_results, selected_clusts = list(c(1L, 2L)))
  testthat::expect_false(is.na(proto))
  testthat::expect_identical(unname(proto), 1L)   # the varying, y-correlated member

  # All-constant tied cluster: must not crash, picks the first deterministically.
  # (suppressWarnings: the names-assignment emits a pre-existing "number of items
  # to replace..." warning when proto_i stays length>1; not introduced by this
  # fix, unrelated to #68.)
  X2 <- X; X2[, 3] <- 2.0; X2[, 4] <- 9.0
  M2 <- matrix(0L, nrow = B, ncol = p); M2[1:5, 3] <- 1L; M2[1:5, 4] <- 1L
  css2 <- structure(list(X = X2, y = y, feat_sel_mat = M2), class = "cssr")
  proto2 <- suppressWarnings(
      getSelectionPrototypes(css2, selected_clusts = list(c(3L, 4L))))
  testthat::expect_identical(unname(proto2), 3L)
})

testthat::test_that("getSelectionPrototypes skips the correlation tie-break for non-numeric y (#128)", {
  # css() accepts a non-numeric y (response validation is delegated to fitfun),
  # so getSelectionPrototypes must NOT call cor() -- which requires a numeric y --
  # when breaking a selection-proportion tie. Hand-build a cssr object with a
  # CHARACTER y and a feat_sel_mat in which members 1 and 2 tie (equal column
  # means) so the tie block IS entered (length(proto_i) > 1). Without the
  # is.numeric(y) guard, cor(X[, 1:2], <character y>) would error; the guard skips
  # it and the first tied index is returned deterministically.
  B <- 10L; p <- 2L; n <- 8L
  X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("V", 1:p)
  y <- letters[1:n]                    # non-numeric (character) response
  M <- matrix(0L, nrow = B, ncol = p)
  M[1:5, 1] <- 1L                      # features 1 and 2 tie at proportion 0.5,
  M[1:5, 2] <- 1L                      # forcing the tie-break path
  css <- structure(list(X = X, y = y, feat_sel_mat = M), class = "cssr")
  # suppressWarnings: an unbroken length-2 tie triggers a benign R "number of
  # items to replace" warning at the names-assignment (it fires for ANY persisted
  # tie, incl. the numeric #68 case) that would otherwise fail testthat edition 3.
  res <- suppressWarnings(getSelectionPrototypes(css, list(c(1L, 2L))))
  testthat::expect_identical(unname(res), 1L)   # first tied index, deterministic
})

testthat::test_that("getSelectionPrototypes emits no warning on an unbroken tie (#126)", {
  # Item-5 regression (#126): the name-assignment now indexes proto_i[1] (a single
  # element), not the possibly length>1 proto_i. On an UNBROKEN within-cluster tie
  # -- here a CHARACTER y, so the numeric correlation tie-break is skipped and
  # proto_i stays length 2 -- the pre-fix code raised R's "number of items to
  # replace is not a multiple of replacement length" warning at the names<-
  # assignment (surfacing through print.cssr / summary.cssr / printCssDf). After
  # the fix NO warning fires, and the returned prototype is still the first tied
  # index (value unchanged; only the spurious warning is removed).
  B <- 10L; p <- 2L; n <- 8L
  X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("V", 1:p)
  y <- letters[1:n]                    # non-numeric (character) response
  M <- matrix(0L, nrow = B, ncol = p)
  M[1:5, 1] <- 1L                      # features 1 and 2 tie at proportion 0.5,
  M[1:5, 2] <- 1L                      # forcing the (unbroken) tie-break path
  css <- structure(list(X = X, y = y, feat_sel_mat = M), class = "cssr")
  # No suppressWarnings here: the fix must make this call warning-free.
  testthat::expect_no_warning(
      res <- getSelectionPrototypes(css, list(c(1L, 2L))))
  testthat::expect_identical(unname(res), 1L)    # first tied index, deterministic
  testthat::expect_identical(names(res), "V1")   # name taken from proto_i[1] only
})

testthat::test_that("printCssDf works", {
  set.seed(67234)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=2, cluster_size=3,
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

testthat::test_that("print.cssr works", {
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  # print() dispatches to print.cssr, which returns its argument invisibly (the
  # standard print-method convention; see print.cssr's @return / #16).
  testthat::expect_invisible(print(css_res))
  ret <- print(css_res)
  testthat::expect_identical(ret, css_res)

  # It prints the printCssDf summary table (column headers included).
  testthat::expect_output(print(css_res), "ClustName")
  testthat::expect_output(print(css_res), "ClustSize")

  # max_num_clusts routes through to printCssDf (at most that many cluster rows).
  out_all <- utils::capture.output(print(css_res))
  out_one <- utils::capture.output(print(css_res, max_num_clusts=1))
  testthat::expect_true(length(out_one) <= length(out_all))
})

testthat::test_that("css runs with B = 1 (minimal subsampling)", {
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)

  # B = 1 is allowed (checkB requires only B > 0) but warns that small B is poor.
  testthat::expect_warning(
    css_b1 <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 1),
    "Small values of B may lead to poor results.", fixed=TRUE)
  testthat::expect_true(inherits(css_b1, "cssr"))

  # The minimal-B object is still well-formed downstream.
  sel <- getCssSelections(css_b1)
  testthat::expect_identical(names(sel),
    c("selected_clusts", "selected_feats", "weights"))
})

testthat::test_that("selected.cssr returns clusters and features", {
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  ref <- getCssSelections(css_res)

  # type = "clusters" (the default): a named list of integer vectors, identical
  # to getCssSelections()$selected_clusts at the same parameters.
  clusts <- selected(css_res)
  testthat::expect_true(is.list(clusts))
  testthat::expect_identical(clusts, ref$selected_clusts)
  testthat::expect_identical(selected(css_res, type="clusters"),
                             ref$selected_clusts)

  # type = "features": a flat integer vector, identical to
  # getCssSelections()$selected_feats.
  feats <- selected(css_res, type="features")
  testthat::expect_true(is.integer(feats))
  testthat::expect_identical(feats, ref$selected_feats)

  # S3 dispatch: selected(obj) reaches selected.cssr().
  testthat::expect_identical(selected(css_res), selected.cssr(css_res))

  # Consistency with getCssSelections() at a nondefault cutoff and weighting
  # (features path).
  ref2 <- getCssSelections(css_res, weighting="simple_avg", cutoff=0.3)
  testthat::expect_identical(
    selected(css_res, type="features", weighting="simple_avg", cutoff=0.3),
    ref2$selected_feats)

  # Invalid inputs are rejected via the reused checks (checkCutoff /
  # checkWeighting), same messages as getCssSelections().
  testthat::expect_error(selected(css_res, cutoff=-0.5),
                         "cutoff >= 0 is not TRUE", fixed=TRUE)
  testthat::expect_error(selected(css_res, weighting="spasre"),
    "Weighting must be a character and one of sparse, simple_avg, or weighted_avg",
    fixed=TRUE)
})

testthat::test_that("selected(type='clusters') is invariant to weighting", {
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B = 10)

  # weighting only affects individual-feature weights, never which clusters are
  # selected: the type = "clusters" output is identical across weighting values.
  sparse <- selected(css_res, type="clusters", weighting="sparse", cutoff=0.2)
  wavg <- selected(css_res, type="clusters", weighting="weighted_avg",
                   cutoff=0.2)
  savg <- selected(css_res, type="clusters", weighting="simple_avg",
                   cutoff=0.2)
  testthat::expect_identical(sparse, wavg)
  testthat::expect_identical(sparse, savg)

  # By contrast the features path CAN depend on weighting: "simple_avg" returns
  # every member of the selected clusters, so at least as many as "sparse".
  feats_sparse <- selected(css_res, type="features", weighting="sparse",
                           cutoff=0.2)
  feats_savg <- selected(css_res, type="features", weighting="simple_avg",
                         cutoff=0.2)
  testthat::expect_true(length(feats_savg) >= length(feats_sparse))
})

testthat::test_that("summary.cssr summarizes clusters, sizes, and header counts", {
  set.seed(67234)
  data <- genClusteredData(n=15, p=11, k_unclustered=2, cluster_size=3,
                           n_clusters=2, sig_clusters=1, sigma_eps_sq=10^(-6))
  x <- data$X
  y <- data$y
  good_clusters <- list(red_cluster=1L:3L, green_cluster=4L:6L)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B=10)

  smry <- summary(css_res)
  testthat::expect_true(inherits(smry, "summary.cssr"))

  ref <- getCssSelections(css_res)
  ref_df <- printCssDf(css_res)

  # Header counts are consistent with getCssSelections().
  testthat::expect_equal(smry$n_selected_clusts, length(ref$selected_clusts))
  testthat::expect_equal(smry$n_selected_feats, length(ref$selected_feats))
  testthat::expect_equal(smry$cutoff, 0)

  # The per-cluster table IS printCssDf()'s data.frame (Theta-hat = ClustSelProp,
  # plus ClustSize), so clusters / proportions / sizes all match.
  testthat::expect_identical(smry$table, ref_df)
  testthat::expect_equal(nrow(smry$table), 7)
  testthat::expect_equal(smry$n_selected_clusts, nrow(smry$table))
  testthat::expect_true(all(c("ClustName", "ClustSelProp", "ClustSize") %in%
                             colnames(smry$table)))
  testthat::expect_equal(smry$table[smry$table$ClustName=="red_cluster",
                                    "ClustSize"], 3)
  testthat::expect_equal(smry$table[smry$table$ClustName=="green_cluster",
                                    "ClustSize"], 3)

  # PFER placeholder present (to be filled in by #87).
  testthat::expect_true("pfer" %in% names(smry))
  testthat::expect_true(is.na(smry$pfer))

  # print.summary.cssr runs, prints the header + table, returns x invisibly.
  testthat::expect_invisible(print(smry))
  ret <- print(smry)
  testthat::expect_identical(ret, smry)
  testthat::expect_output(print(smry), "selected at cutoff")
  testthat::expect_output(print(smry), "ClustName")

  # S3 dispatch: summary(obj) reaches summary.cssr().
  testthat::expect_identical(summary(css_res), summary.cssr(css_res))

  # Invalid cutoff rejected via the reused check.
  testthat::expect_error(summary(css_res, cutoff=1.5),
                         "cutoff <= 1 is not TRUE", fixed=TRUE)
})

testthat::test_that("selected() and summary() handle an empty selection", {
  # Same empty-producing fixture as the getCssSelections #107 test: fixed seed,
  # all-singleton clusters, and a large lambda so the base lasso selects almost
  # nothing; cutoff = 1 with min_num_clusts = 0 then selects no cluster.
  set.seed(8)
  B <- 10
  x <- matrix(stats::rnorm(30*8), nrow=30, ncol=8)
  y <- stats::rnorm(30)
  css_res <- css(X=x, y=y, lambda=0.5, clusters=list(), B=B)
  testthat::expect_true(max(colMeans(css_res$clus_sel_mat)) < 1 - 1/(2*B))

  # selected(): a clean empty list / empty integer vector, no error.
  empty_clusts <- selected(css_res, cutoff=1, min_num_clusts=0)
  testthat::expect_true(is.list(empty_clusts))
  testthat::expect_length(empty_clusts, 0)
  empty_feats <- selected(css_res, type="features", cutoff=1, min_num_clusts=0)
  testthat::expect_true(is.integer(empty_feats))
  testthat::expect_length(empty_feats, 0)

  # summary(): a well-formed zero-row summary (table NULL, "0 clusters" header),
  # NOT an opaque stopifnot() failure from printCssDf/getSelectionPrototypes.
  smry <- summary(css_res, cutoff=1, min_num_clusts=0)
  testthat::expect_true(inherits(smry, "summary.cssr"))
  testthat::expect_equal(smry$n_selected_clusts, 0)
  testthat::expect_equal(smry$n_selected_feats, 0)
  testthat::expect_true(is.null(smry$table))

  # print still works and reports 0 clusters / 0 features.
  testthat::expect_invisible(print(smry))
  testthat::expect_output(print(smry), "0 clusters / 0 features")

  # printCssDf() (#120): an empty selection returns a well-formed zero-row
  # data.frame with the SAME columns as a non-empty printCssDf() on the same
  # object, rather than tripping stopifnot(nrow >= 1). Unnamed X (this fixture)
  # -> 4 columns, no ClustProtoName.
  empty_df <- printCssDf(css_res, cutoff=1, min_num_clusts=0)
  testthat::expect_true(is.data.frame(empty_df))
  testthat::expect_equal(nrow(empty_df), 0)
  testthat::expect_equal(ncol(empty_df), 4)
  nonempty_df <- printCssDf(css_res, cutoff=0)
  testthat::expect_true(nrow(nonempty_df) >= 1)
  # The invariant that matters: empty and non-empty share the same column set.
  testthat::expect_identical(colnames(empty_df), colnames(nonempty_df))
  testthat::expect_identical(
    colnames(empty_df),
    c("ClustName", "ClustProtoNum", "ClustSelProp", "ClustSize"))
  # Correct 0-length column types.
  testthat::expect_true(is.character(empty_df$ClustName))
  testthat::expect_true(is.integer(empty_df$ClustProtoNum))
  testthat::expect_true(is.numeric(empty_df$ClustSelProp))
  testthat::expect_true(is.integer(empty_df$ClustSize))

  # Named X -> the 5-column path (adds ClustProtoName). Same data + column names,
  # so the selection is still empty at cutoff = 1 with min_num_clusts = 0.
  x_named <- x
  colnames(x_named) <- paste0("V", seq_len(ncol(x_named)))
  css_named <- css(X=x_named, y=y, lambda=0.5, clusters=list(), B=B)
  empty_named_df <- printCssDf(css_named, cutoff=1, min_num_clusts=0)
  testthat::expect_true(is.data.frame(empty_named_df))
  testthat::expect_equal(nrow(empty_named_df), 0)
  testthat::expect_equal(ncol(empty_named_df), 5)
  testthat::expect_true("ClustProtoName" %in% colnames(empty_named_df))
  testthat::expect_identical(colnames(empty_named_df),
                             colnames(printCssDf(css_named, cutoff=0)))

  # getSelectionPrototypes() on an empty list of clusters -> integer(0) (#120).
  testthat::expect_identical(getSelectionPrototypes(css_res, list()),
                             integer(0))

  # print.cssr() (#120): prints a "(no clusters selected ...)" message instead
  # of a table, does not error, and returns the cssr object invisibly unchanged.
  testthat::expect_output(
    ret <- print(css_res, cutoff=1, min_num_clusts=0),
    "no clusters selected")
  testthat::expect_identical(ret, css_res)
  testthat::expect_invisible(print(css_res, cutoff=1, min_num_clusts=0))
})

testthat::test_that("plot.cssr draws cluster proportions and returns them sorted", {
  # Named X so the feature matrix (tested below) carries feature names.
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  colnames(x) <- paste0("V", seq_len(ncol(x)))
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B=10)

  grDevices::pdf(NULL)  # draw to a null device (no file, no window)

  # (a) Clusters (default): runs without error and invisibly returns the sorted
  # named cluster selection proportions (a deterministic proof of the return).
  testthat::expect_invisible(plot(css_res))
  ret <- plot(css_res)
  testthat::expect_equal(ret,
    sort(colMeans(css_res$clus_sel_mat), decreasing=TRUE))
  testthat::expect_false(is.null(names(ret)))  # cluster names guaranteed

  # (b) Features, NAMED X: returns the sorted NAMED feature selection
  # proportions, and the (index-based) highlight matches getCssSelections().
  ret_f <- plot(css_res, type="features")
  props_f <- colMeans(css_res$feat_sel_mat)
  testthat::expect_equal(ret_f, sort(props_f, decreasing=TRUE))
  testthat::expect_identical(names(ret_f),
    paste0("V", order(props_f, decreasing=TRUE)))  # named + correctly ordered
  sel_f <- getCssSelections(css_res, weighting="sparse")
  ord <- order(props_f, decreasing=TRUE)
  is_sel <- (seq_along(props_f) %in% as.integer(sel_f$selected_feats))[ord]
  testthat::expect_equal(sum(is_sel), length(sel_f$selected_feats))
  testthat::expect_setequal(ord[is_sel], as.integer(sel_f$selected_feats))

  grDevices::dev.off()
})

testthat::test_that("plot.cssr feature highlight is index-based for unnamed X (B2)", {
  # genClusteredData returns X with NULL colnames -> feat_sel_mat is UNNAMED.
  # A name-based feature highlight would silently be all-FALSE; the method uses
  # an INDEX-based highlight, which must still be non-empty. This is the B2
  # regression proof.
  set.seed(721)
  dat <- genClusteredData(n=60, p=11, k_unclustered=2, cluster_size=4,
    n_clusters=1, snr=3)
  clusters <- list(cluster1=1:4)
  css_res <- suppressWarnings(css(X=dat$X, y=dat$y, lambda=0.01,
    clusters=clusters, B=10))

  # Precondition for the B2 guard: the feature matrix really is unnamed.
  testthat::expect_null(colnames(css_res$feat_sel_mat))

  grDevices::pdf(NULL)

  props <- colMeans(css_res$feat_sel_mat)
  ret <- plot(css_res, type="features", cutoff=0.3)
  # Returns the plain (unnamed) sorted colMeans regardless of the cutoff.
  testthat::expect_equal(ret, sort(props, decreasing=TRUE))
  testthat::expect_null(names(ret))

  # Re-derive the highlight exactly as plot.cssr does: INDEX-based.
  sel <- getCssSelections(css_res, weighting="sparse", cutoff=0.3)
  testthat::expect_true(length(sel$selected_feats) > 0)  # there ARE selections
  ord <- order(props, decreasing=TRUE)
  is_sel <- (seq_along(props) %in% as.integer(sel$selected_feats))[ord]
  testthat::expect_true(any(is_sel))                     # NON-empty highlight
  testthat::expect_equal(sum(is_sel), length(sel$selected_feats))
  # The highlighted bars are exactly the selected features, by ORIGINAL index.
  testthat::expect_setequal(ord[is_sel], as.integer(sel$selected_feats))

  # Contrast: a NAME-based highlight WOULD have failed here (names are NULL, so
  # names(props) %in% ... is logical(0) -> no bar highlighted).
  testthat::expect_false(any(names(props) %in%
    as.character(sel$selected_feats)))

  grDevices::dev.off()
})

testthat::test_that("plot.cssr forwards ... to barplot without a duplicate-arg crash (B1)", {
  # The method builds a SINGLE merged barplot arg list, so graphical params on
  # ... cannot collide with col/ylim/height (which previously crashed with
  # "formal argument 'col' matched by multiple actual arguments").
  set.seed(26717)
  x <- matrix(stats::rnorm(10*7), nrow=10, ncol=7)
  colnames(x) <- paste0("V", seq_len(ncol(x)))
  y <- stats::rnorm(10)
  good_clusters <- list("apple"=1:2, "banana"=3:4, "cantaloupe"=5)
  css_res <- css(X=x, y=y, lambda=0.01, clusters=good_clusters, B=10)

  grDevices::pdf(NULL)
  testthat::expect_no_error(plot(css_res, col="red"))
  testthat::expect_no_error(plot(css_res, ylim=c(0, 0.5)))
  testthat::expect_no_error(plot(css_res, main="x"))
  testthat::expect_no_error(plot(css_res, type="features", col="red",
    main="feats"))
  grDevices::dev.off()
})

testthat::test_that("plot.cssr handles an empty selection and a single cluster", {
  # (e) Empty selection: min_num_clusts = 0 with a cutoff strictly above every
  # cluster proportion selects nothing (min_num_clusts = 1 is never empty). All
  # bars get unsel_col, there is no highlight, and there is no crash.
  set.seed(8)
  B <- 10
  x <- matrix(stats::rnorm(30*8), nrow=30, ncol=8)
  y <- stats::rnorm(30)
  css_res <- css(X=x, y=y, lambda=0.5, clusters=list(), B=B)
  max_prop <- max(colMeans(css_res$clus_sel_mat))
  testthat::expect_true(max_prop < 1)
  cut <- (max_prop + 1) / 2  # strictly between max_prop and 1 (and <= 1)
  sel <- getCssSelections(css_res, cutoff=cut, min_num_clusts=0)
  testthat::expect_length(sel$selected_clusts, 0)  # genuinely empty

  grDevices::pdf(NULL)
  # A bare call that errored would fail the test -> this asserts "no crash".
  ret <- plot(css_res, min_num_clusts=0, cutoff=cut)
  testthat::expect_equal(ret,
    sort(colMeans(css_res$clus_sel_mat), decreasing=TRUE))
  grDevices::dev.off()

  # (f) Single-cluster css: clus_sel_mat has exactly one column -> a length-1
  # barplot, no error.
  set.seed(1)
  x1 <- matrix(stats::rnorm(10*4), nrow=10, ncol=4)
  y1 <- stats::rnorm(10)
  css_1 <- css(X=x1, y=y1, lambda=0.01, clusters=list(all=1:4), B=10)
  testthat::expect_equal(ncol(css_1$clus_sel_mat), 1)

  grDevices::pdf(NULL)
  ret1 <- plot(css_1)
  testthat::expect_length(ret1, 1)
  grDevices::dev.off()
})

testthat::test_that("Opt 2 (#129): formCssDesign weights= equals internal recompute; getCssPreds finite", {
  set.seed(721)
  dat <- genClusteredData(n = 60, p = 11, k_unclustered = 2, cluster_size = 4,
      n_clusters = 1, snr = 3)
  clusters <- list(cluster1 = 1:4)
  res <- suppressWarnings(css(X = dat$X, y = dat$y, lambda = 0.01,
      clusters = clusters, B = 15))

  # The hoist: passing weights= (as getCssPreds now does) must build a design
  # matrix byte-identical to letting formCssDesign() recompute the weights via
  # getSelectedClusters(), across every weighting rule.
  for(w in c("sparse", "weighted_avg", "simple_avg")){
    wts <- getSelectedClusters(res, w, 0, 1, NA)$weights
    d_recompute <- suppressWarnings(formCssDesign(res, weighting = w,
        newx = dat$X))
    d_passed <- suppressWarnings(formCssDesign(res, weighting = w,
        newx = dat$X, weights = wts))
    testthat::expect_identical(d_passed, d_recompute)
  }

  # End-to-end getCssPreds still returns finite predictions of the right length
  # for both the provided-trainX path and the train_inds path, across weightings.
  res_ti <- suppressWarnings(css(X = dat$X, y = dat$y, lambda = 0.01,
      clusters = clusters, B = 15, train_inds = 41:60))
  for(w in c("sparse", "weighted_avg", "simple_avg")){
    p_prov <- suppressWarnings(getCssPreds(res, testX = dat$X[1:10, ],
        trainX = dat$X[11:40, ], trainY = dat$y[11:40], weighting = w))
    testthat::expect_length(p_prov, 10)
    testthat::expect_true(all(is.finite(p_prov)))
    p_ti <- suppressWarnings(getCssPreds(res_ti, testX = dat$X[1:10, ],
        weighting = w))
    testthat::expect_length(p_ti, 10)
    testthat::expect_true(all(is.finite(p_ti)))
  }
})

testthat::test_that("Opt 3 (#129): buildCssDf equals printCssDf; summary$table reuses it", {
  set.seed(824)
  dat <- genClusteredData(n = 55, p = 11, k_unclustered = 2, cluster_size = 4,
      n_clusters = 1, snr = 3)
  clusters <- list(cluster1 = 1:4)
  res <- suppressWarnings(css(X = dat$X, y = dat$y, lambda = 0.01,
      clusters = clusters, B = 12))

  # buildCssDf(obj, getCssSelections(obj, ...)$selected_clusts) reproduces
  # printCssDf(obj, ...) exactly, across cutoffs (including the empty selection
  # at cutoff = 1 with min_num_clusts = 0).
  for(cut in c(0, 0.2, 0.5, 1)){
    sc <- suppressWarnings(getCssSelections(res, cutoff = cut,
        min_num_clusts = 0))$selected_clusts
    testthat::expect_identical(
        suppressWarnings(buildCssDf(res, sc)),
        suppressWarnings(printCssDf(res, cutoff = cut, min_num_clusts = 0)))
  }

  # summary.cssr()$table (now built via buildCssDf() on summary's own
  # sel$selected_clusts) equals printCssDf() on the same object across
  # weightings -- cluster selection is weighting-invariant, so reusing summary's
  # sel$selected_clusts is byte-identical to printCssDf's internal recompute.
  ref <- suppressWarnings(printCssDf(res))
  for(w in c("sparse", "weighted_avg", "simple_avg")){
    s <- suppressWarnings(summary(res, weighting = w))
    testthat::expect_identical(s$table, ref)
  }
})

testthat::test_that("cssSelect works", {
  set.seed(73212)
  
  data <- genClusteredData(n=15, p=11, k_unclustered=2, cluster_size=3,
                        n_clusters=2, sig_clusters=1, sigma_eps_sq=1)
  
  x <- data$X
  y <- data$y
  
  # Intentionally don't provide clusters for all features, mix up formatting,
  # etc.
  good_clusters <- list(red_cluster=1L:3L, 4:6)
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
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
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
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
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  # Should get error if I try to use data.frame with clusters, since data.frame
  # has factors with more than two levels
  testthat::expect_error(cssSelect(X=df2, y=stats::rnorm(nrow(X_df)),
                                   clusters=1:3),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert the data.frame X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)
  
  # Should be fine if I don't use clusters
  res <- cssSelect(X=df2, y=stats::rnorm(nrow(X_df)))
  
  X_df_mat <- stats::model.matrix(~ ., df2)
  X_df_mat <- X_df_mat[, colnames(X_df_mat) != "(Intercept)"]
  p <- ncol(X_df_mat)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
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
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
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
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, cutoff=0.6)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, max_num_clusts=6)
  
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
  testthat::expect_true(length(res$selected_clusts) <= 6)
  
  res <- cssSelect(X=x, y=y, clusters=good_clusters, auto_select_size=FALSE)

  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 3)
  testthat::expect_identical(names(res), c("selected_clusts", "selected_feats",
                                           "weights"))
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

  # alpha = 0 (degenerate ridge) is rejected
  testthat::expect_error(cssSelect(X=x, y=y, alpha=0),
                         "alpha > 0 is not TRUE", fixed=TRUE)

  # alpha > 1 is rejected
  testthat::expect_error(cssSelect(X=x, y=y, alpha=1.5),
                         "alpha <= 1 is not TRUE", fixed=TRUE)

  # alpha = NA is rejected with a clear message (rather than a cryptic error
  # deep inside the bundling/subsampling logic). A bare (logical) NA trips the
  # type check first; a numeric NA reaches the dedicated !is.na guard.
  testthat::expect_error(cssSelect(X=x, y=y, alpha=NA),
                         "is.numeric(alpha) | is.integer(alpha) is not TRUE",
                         fixed=TRUE)
  testthat::expect_error(cssSelect(X=x, y=y, alpha=NA_real_),
                         "!is.na(alpha) is not TRUE", fixed=TRUE)

  # An NA in the design matrix yields the shared friendly message (#56).
  x_na <- x; x_na[2, 1] <- NA
  testthat::expect_error(cssSelect(X=x_na, y=y),
                         "must not contain missing", fixed=TRUE)

  # (#127) A single-column X reaches getLassoLambda as a matrix and now errors
  # with the clean "p >= 2 is not TRUE" message rather than a cryptic downstream
  # failure ("invalid 'times' argument").
  testthat::expect_error(
    cssSelect(matrix(stats::rnorm(40), nrow=40, ncol=1), stats::rnorm(40)),
    "p >= 2 is not TRUE", fixed=TRUE)
})

testthat::test_that("cssSelect threads alpha through to selection", {
  set.seed(1)
  n <- 100
  p <- 20
  rho <- 0.9
  block <- 4
  Z <- matrix(stats::rnorm(n*p), n, p)
  common <- stats::rnorm(n)
  for(j in 1:block){
    Z[, j] <- sqrt(rho)*common + sqrt(1 - rho)*Z[, j]
  }
  y <- as.numeric(Z %*% c(rep(1, block), rep(0, p - block)) + stats::rnorm(n))

  # Treat the correlated block as one cluster; the remaining features are
  # singletons. Fix lambda and the model size so the only thing that varies
  # between the two calls is alpha.
  clusters <- list(block=1:block)

  res_lasso <- cssSelect(X=Z, y=y, clusters=clusters, lambda=3.72251,
                         max_num_clusts=p - block + 1, alpha=1)
  res_enet <- cssSelect(X=Z, y=y, clusters=clusters, lambda=3.72251,
                        max_num_clusts=p - block + 1, alpha=0.5)

  # Both return a well-formed selection
  for(res in list(res_lasso, res_enet)){
    testthat::expect_true(is.list(res))
    testthat::expect_true(all(c("selected_clusts", "selected_feats") %in%
                                 names(res)))
    testthat::expect_true(is.integer(res$selected_feats))
    testthat::expect_true(all(res$selected_feats %in% 1:p))
  }

  # The elastic net selects (weakly) more features than the pure lasso on this
  # correlated design, and the two selected sets differ.
  testthat::expect_true(length(res_enet$selected_feats) >=
                          length(res_lasso$selected_feats))
  testthat::expect_false(setequal(res_enet$selected_feats,
                                  res_lasso$selected_feats))
})

testthat::test_that("cssPredict works", {
  set.seed(84231)
  
  train_data <- genClusteredData(n=30, p=11, k_unclustered=2, cluster_size=3,
                              n_clusters=2, sig_clusters=1, sigma_eps_sq=1)
  
  x <- train_data$X
  y <- train_data$y
  
  test_x <- genClusteredData(n=5, p=11, k_unclustered=2, cluster_size=3,
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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  # Should get error if clusters are provided because df2 contains factors with
  # more than two levels
  testthat::expect_error(cssPredict(X_train_selec=df2[selec_train_inds, ],
                                    y_train_selec=stats::rnorm(n_selec_train),
                                    X_test=df2[test_inds, ], clusters=1:3),
                         "When stats::model.matrix converted the provided data.frame X_train_selec to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X_train_selec to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.",
                         fixed=TRUE)
  
  # Should be fine if no clusters are provided
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

  # Non-default alpha (elastic net) threaded through end-to-end; smoke test
  # confirming it returns a well-formed prediction vector of the right length.
  res <- cssPredict(X_train_selec=x, y_train_selec=y, X_test=test_x,
                    alpha=0.5)

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

  # alpha = 0 (degenerate ridge) is rejected
  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, alpha=0),
                         "alpha > 0 is not TRUE", fixed=TRUE)

  # alpha > 1 is rejected
  testthat::expect_error(cssPredict(X_train_selec=x, y_train_selec=y,
                                    X_test=test_x, alpha=1.5),
                         "alpha <= 1 is not TRUE", fixed=TRUE)

  # (#127) A single-column training design now reaches getLassoLambda as a matrix
  # (drop = FALSE on the subsample) and errors with the clean "p >= 2 is not
  # TRUE" message rather than "is.matrix(X) is not TRUE".
  testthat::expect_error(
    cssPredict(X_train_selec=matrix(stats::rnorm(40), nrow=40, ncol=1),
               y_train_selec=stats::rnorm(40),
               X_test=matrix(stats::rnorm(10), nrow=10, ncol=1)),
    "p >= 2 is not TRUE", fixed=TRUE)
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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  # Should get error if I try to use clusters because df2 contains factors with
  # more than two levels
  testthat::expect_error(processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=1:3, nlambda=10), "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.",
                         fixed=TRUE)

  # Should be fine with no clusters
  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=list(), nlambda=10)

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  # Should get an error if clusters are provided since df2 contains factors
  # with more than two levels
  testthat::expect_error(processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=1:3, nlambda=10),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)
  
  # Should be fine if no clusters are provided 
  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=list(), nlambda=10)
  
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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  res <- processClusterLassoInputs(X=df2, y=stats::rnorm(nrow(df2)),
                                   clusters=list(), nlambda=10)

  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="protolasso",
                       prototypes=res$prototypes)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model))

  for(j in 1:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j] - X_df_model[, j]) < 10^(-9)))
  }

  ret_df <- getXglmnet(x=res$x, clusters=res$clusters, type="clusterRepLasso",
                       prototypes=res$prototypes)

  testthat::expect_true(is.matrix(ret_df))
  testthat::expect_true(is.numeric(ret_df))
  testthat::expect_true(is.null(colnames(ret_df)))
  testthat::expect_true(nrow(ret_df) == nrow(X_df))
  # Each column of ret_df should be one of the prototypes.
  testthat::expect_true(ncol(ret_df) == ncol(X_df_model))

  for(j in 1:ncol(X_df_model)){
    testthat::expect_true(all(abs(ret_df[, j] - X_df_model[, j]) < 10^(-9)))
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

  # do.call(cbind) preserves integer storage of an integer x (#58)
  x_int <- matrix(1:12, nrow = 4, ncol = 3)
  int_clusters <- list(c1 = 1L, c2 = 2L, c3 = 3L)
  res_int <- getXglmnet(x_int, int_clusters, type = "protolasso",
    prototypes = c(1L, 2L, 3L))
  testthat::expect_true(is.integer(res_int))

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  # Should throw an error if we assign clusters because df2 contains factors
  # with more than two levels
  testthat::expect_error(processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=1:3, nlambda=100),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)

  # Should be fine if no clusters are provided
  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=list(), nlambda=100)

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  # Should throw an error if we assign clusters because df2 contains factors
  # with more than two levels
  testthat::expect_error(processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=1:3, nlambda=100),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)

  # Should be fine if no clusters are provided
  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=list(), nlambda=100)

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]

  process <- processClusterLassoInputs(X=df2, y=rnorm(nrow(df2)),
                                       clusters=list(), nlambda=100)

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
  
  # An all-empty lasso path (max_length == 0) must return an empty selection,
  # not crash with "model_size > 0 is not TRUE" (#157).
  out_empty <- getClusterSelsFromGlmnet(list(integer(0)), clusters = list(1L),
      prototypes = 1L, feat_names = NA)
  testthat::expect_length(out_empty$selected_sets, 0)
  testthat::expect_length(out_empty$selected_clusts_list, 0)
})

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)

  X_df_model <- stats::model.matrix(~ ., df2)
  X_df_model <- X_df_model[, colnames(X_df_model) != "(Intercept)"]
  
  # Should get an error if we try to call protolasso on df2 with clusters
  # because df2 contains factors with more than two levels
  testthat::expect_error(protolasso(df2, y_df, 4:6, nlambda=70),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)
  
  # Should be fine if no clusters are provided
  res <- protolasso(df2, y_df, nlambda=70)
  
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))


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

  # (#127) A single all-encompassing cluster (or a genuine p < 2 input) leaves
  # glmnet with a 1-column design; clusterLassoCore now errors up front with a
  # cssr message naming both causes, instead of glmnet's "2 or more columns".
  testthat::expect_error(
    protolasso(X=x, y=y, clusters=list(all=1:11)),
    "need at least 2 cluster representatives to fit the lasso", fixed=TRUE)
  
})

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
  # cyl, gear, and carb are factors with more than 2 levels
  df2 <- X_df
  df2$cyl <- as.factor(df2$cyl)
  df2$vs <- as.factor(df2$vs)
  df2$am <- as.factor(df2$am)
  df2$gear <- as.factor(df2$gear)
  df2$carb <- as.factor(df2$carb)
  
  # Should get an error if we try to call clusterRepLasso on df2 with clusters
  # because df2 contains factors with more than two levels
  testthat::expect_error(clusterRepLasso(df2, y_df, 4:6, nlambda=70),
                         "When stats::model.matrix converted the provided data.frame X to a matrix, the number of columns changed (probably because the provided data.frame contained a factor variable with at least three levels). Please convert X to a matrix yourself using model.matrix and provide cluster assignments according to the columns of the new matrix.", fixed=TRUE)
  
  # Should be fine if no clusters are provided
  res <- clusterRepLasso(df2, y_df, nlambda=70)
  
  testthat::expect_true(is.list(res))
  testthat::expect_identical(names(res), c("selected_sets",
                                           "selected_clusts_list", "beta"))

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

  # (#127) Same degenerate-design guard as protolasso: a single all-encompassing
  # cluster yields a 1-column design, now rejected with a clear cssr message
  # rather than glmnet's "2 or more columns".
  testthat::expect_error(
    clusterRepLasso(X=x, y=y, clusters=list(all=1:11)),
    "need at least 2 cluster representatives to fit the lasso", fixed=TRUE)
  
})

testthat::test_that("protolasso/clusterRepLasso return an empty selection when the lasso path selects nothing (#157)", {
    # X'y must be EXACTLY zero to empty the whole glmnet path (see plan note:
    # floating-point orthogonality drives lambda_max->0 and selects everything).
    # Paired +1/-1 integer rows give crossprod(X, y) == 0 exactly.
    y <- as.double(c(1, 1, 1, 1, -1, -1, -1, -1))
    X <- cbind(c(1,-1,0,0,0,0,0,0), c(0,0,1,-1,0,0,0,0),
        c(0,0,0,0,1,-1,0,0), c(0,0,0,0,0,0,1,-1))
    storage.mode(X) <- "double"
    testthat::expect_identical(max(abs(crossprod(X, y))), 0)   # exactly orthogonal

    # Was: crash "model_size > 0 is not TRUE". Now: empty selection, no error.
    res_p <- protolasso(X, y)
    testthat::expect_length(res_p$selected_sets, 0)
    testthat::expect_length(res_p$selected_clusts_list, 0)

    res_c <- clusterRepLasso(X, y)
    testthat::expect_length(res_c$selected_sets, 0)
    testthat::expect_length(res_c$selected_clusts_list, 0)

    # A normal signal case still selects features (guards against over-fixing).
    set.seed(2)
    Xs <- matrix(stats::rnorm(24 * 4), 24, 4)
    ys <- Xs[, 1] * 2 + stats::rnorm(24, sd = 0.1)
    testthat::expect_gt(length(protolasso(Xs, ys)$selected_sets), 0)
})

