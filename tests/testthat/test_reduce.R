context("Single tests")

set.seed(1)
n <- 100
p <- 20
X <- matrix(rnorm(p*n), ncol = p)
y <- X[, 1] - 0.5*X[, 2] + rnorm(n)

test_that("Fast", {
  # cor.test(y, X[, 1])$p.v
  # cor.test(y, X[, 2])$p.v
  # cor.test(y, X[, 13])$p.v
  d <- prepare_data(y, X, na = FALSE, verbose = FALSE)
  expect_equal(reduce_matrix(d)$candidates, c(1, 2, 17, 13))
  expect_equal(reduce_matrix(d, min = 0.0245)$candidates, 1:2)
  expect_equal(reduce_matrix(d, min = 0.0244)$candidates, 1)
  expect_equal(reduce_matrix(d, min = 0)$candidates, integer(0))

  d$na <- TRUE
  expect_equal(reduce_matrix(d, min = 0.0245)$candidates, 1:2)
})

test_that("With NA", {
  X[5, 5] <- NA
  d <-  prepare_data(y, X, na = TRUE, verbose = FALSE)
  expect_equal(reduce_matrix(d)$candidates, c(1, 2, 17, 13))
  expect_equal(reduce_matrix(d, min = 0.0245)$candidates, 1:2)
  expect_equal(reduce_matrix(d, min = 0.0244)$candidates, 1)
  expect_equal(reduce_matrix(d, min = 0)$candidates, integer(0))

  y[9] <- NA
  d <- prepare_data(y, X, verbose = FALSE)
  # cor.test(y, X[, 2])$p.v
  expect_equal(reduce_matrix(d, min = 0.0277)$candidates, 1:2)
  expect_equal(reduce_matrix(d, min = 0.0276)$candidates, 1)
})
