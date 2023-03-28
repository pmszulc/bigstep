context("Data preparations")

X <- matrix(1:12, ncol = 3)
colnames(X) <- c("a", "b", "c")
y <- 1:4
Xm <- matrix(11:18, ncol = 2)

Xlong <- matrix(1:4e6, ncol = 2)
colnames(Xlong) <- c("a", "b")
ylong <- 1:2e6

test_that("Prepare data", {
  d <- prepare_data(y, X)
  expect_s3_class(d, "big")
  expect_equal(d$X, X)
  expect_equal(d$y, y)
  expect_equal(d$type, "linear")
  expect_equal(d$candidates, 1:3)
  expect_null(d$model)
  expect_equal(d$Xm, matrix(1, nrow = 4))
  expect_false(d$na)
  expect_equal(d$maxp, 1e6)
  expect_null(d$crit)
  expect_equal(d$metric, "MSE")
  expect_null(d$metric_v)
  expect_false(d$stepwise)
  expect_equal(d$stay, 1)
  expect_null(d$model)
  expect_equal(d$fit_fun, fit_linear)
  expect_true(d$verbose)
})

test_that("Prepare data -- Xlong", {
  d <- prepare_data(ylong, Xlong, maxp = 2e6)
  expect_s3_class(d, "big")
  expect_equal(d$X, Xlong)
  expect_equal(d$y, ylong)
  expect_equal(d$type, "linear")
  expect_equal(d$candidates, 1:2)
  expect_null(d$model)
  expect_false(d$na)
  expect_equal(d$maxp, 2e6)
  expect_null(d$crit)
  expect_equal(d$metric, "MSE")
  expect_null(d$metric_v)
  expect_false(d$stepwise)
  expect_equal(d$stay, 1)
  expect_null(d$model)
  expect_equal(d$fit_fun, fit_linear)
  expect_true(d$verbose)
})

test_that("Bigmemory", {
  colnames(X) <- c("a", "b", "c")
  Xbig <- bigmemory::as.big.matrix(X)
  d <- prepare_data(y, Xbig)
  expect_true(bigmemory::is.big.matrix(d$X))
  expect_equal(d$y, y)
  expect_equal(d$type, "linear")
  expect_equal(d$candidates, 1:3)
  expect_null(d$model)
  expect_equal(d$Xm, matrix(1, nrow = 4))
  expect_false(d$na)
  expect_equal(d$maxp, 1e6)
  expect_null(d$crit)
  expect_equal(d$metric, "MSE")
  expect_null(d$metric_v)
  expect_equal(d$stepwise, FALSE)
  expect_equal(d$stay, 1)
  expect_null(d$model)
  expect_equal(d$fit_fun, fit_linear)
  expect_true(d$verbose)
  # Xbig <- bigmemory::read.big.matrix("../../smallX.csv", sep = ";")
  # expect_equal(Xbig[, ], unname(X))
  # d <- prepare_data(y, Xbig)
})

test_that("Errors", {
  expect_error(prepare_data(X = as.list(X), y = y),
    "is.numeric(X) | is.data.frame(X) | bigmemory::is.big.matrix(X) is not TRUE",
    fixed = TRUE)
  expect_error(prepare_data(y[-1], X), "nrow(X) == n is not TRUE", fixed = TRUE)
  expect_error(prepare_data(y, X, Xadd = Xm[-1, ]), "nrow(Xm) == n is not TRUE",fixed = TRUE)
  expect_error(prepare_data(X = X, y = y, candidates = 1:5),
               "all(candidates %in% 1:p) is not TRUE", fixed = TRUE)
  expect_error(prepare_data(y, X, maxp = 3), "maxp >= n is not TRUE", fixed = TRUE)
})

test_that("Check NA", {
  expect_false(check_na(X))
  X[2, 2] <- NA
  expect_true(check_na(X))
})

test_that("Create parts", {
  parts <- create_parts(ind = 1:20, n = 100, maxp = 1000)
  expect_type(parts, "list")
  expect_equal(unlist(parts, use.names = FALSE), 1:20)
  expect_equal(parts, list('1' = 1:10, '2' = 11:20))
  expect_equal(length(create_parts(1:20, 10, 1000)), 1)
  expect_equal(length(create_parts(1:20, 100, 100)), 20)
  expect_error(create_parts(1:20, 100, 99), "maxp >= n is not TRUE")
  expect_equal(create_parts(1, 1, 100), list('1' = 1))
})
