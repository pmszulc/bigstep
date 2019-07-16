context("Criteria")

test_that("Right values", {
  expect_equal(aic(0, 0), 0)
  expect_equal(aic(-5, 2), 14)
  expect_equal(bic(-5, 0, 10), 10)
  expect_equal(maic(-1, 5, p = 8, const = 4), 12)
})

test_that("Right equalities", {
  expect_equal(mbic(1.2, 5, 10, p = 8), bic(1.2, 5, 10))
  expect_equal(mbic2(9.9, 1, 100, 20), mbic(9.9, 1, 100, 20))
  expect_equal(mbic2(-1, 151, 1e3, 1e4),
               mbic(-1, 151, 1e3, 1e4) - 2*log(factorial(151)))
  expect_equal(maic2(0.1, 1, 10, 1e5), maic(0.1, 1, 1e5))
})

test_that("Right inequalities", {
  expect_true(aic(8.5, 5) > bic(8.5, 5, 7))
  expect_true(aic(8.5, 5) < bic(8.5, 5, 8))
  expect_true(mbic(-8.5, 8, 100, 20) > bic(-8.5, 8, 100))
  expect_true(mbic2(-8.5, 8, 100, 20) < mbic(-8.5, 8, 100, 20))
  expect_true(mbic2(10, 50, 100, 1e10) < mbic2(10, 50, 101, 1e10))
  expect_true(maic2(-8.5, 8, 100, 20) < maic(-8.5, 8, 20))
  expect_true(maic2(10, 50, 100, 1e10) < maic2(10, 51, 100, 1e10))

  set.seed(1)
  n <- 30
  p <- 10
  X <- matrix(rnorm(p*n), ncol = p)
  y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
  lm1 <- lm(y ~ X[, 2] + X[, 4])
  lm2 <- lm(y ~ X[, 2] + X[, 4] + X[, 6] + X[, 10])
  expect_true(stats::BIC(lm1) < stats::BIC(lm2))
  Xm1 <- cbind(1, X[, c(2, 4)])
  loglik1 <- loglik(y, Xm1)
  Xm2 <- cbind(1, X[, c(2, 4, 6, 10)])
  loglik2 <- loglik(y, Xm2)
  expect_true(bic(loglik1, k = 3, n = n) < bic(loglik2, k = 5, n = n))
})

test_that("Errors", {
  expect_error(aic(0, -1), "k >= 0 is not TRUE")
  expect_error(bic(0, 10, -5), "n > 0 is not TRUE")
  expect_error(bic(0, -10, 20), "k >= 0 is not TRUE")
  expect_error(mbic(0, 2, 20, p = -1), "p > 0 is not TRUE")
  expect_error(mbic(0, 2, 20, p = 20, const = 20), "p/const > 1 is not TRUE")
  expect_error(mbic2(0, 2, 20, p = 3), "p/const > 1 is not TRUE")
  expect_error(mbic2(0, 6, 20, p = 5), "p/k >= 1 is not TRUE")
  expect_error(mbic2(0, 1000, 0, 300), "n > 0 is not TRUE")
  expect_error(maic2(-20, 10, 100, 0), "p > 0 is not TRUE")
  expect_equal(mbic2(-20, 80, 100, 200), Inf)
  expect_equal(maic2(-20, 80, 1000, 159), Inf)
})
