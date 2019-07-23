context("Backward")

suppressWarnings(RNGversion("3.5.0"))
set.seed(1)
n <- 30
p <- 10
X <- matrix(sample(0:2, p*n, replace = TRUE), ncol = p)
colnames(X) <- letters[1:p]
y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
d <- prepare_data(y, X)
d$Xm <- cbind(1, X[, c(2:3, 6:7)])

Xm <- cbind(1, X[, c(2, 3, 6)])
loglik <- loglik(y, Xm)
bic_v <- bic(loglik, k = 3, n = n)

test_that("Right variable to remove", {
  res <- backward(d, crit = bic)
  expect_equal(res$Xm, Xm)
  expect_equal(res$model, c("b", "c", "f"))
  expect_equal(res$crit, bic_v)
  expect_message(backward(d, crit = bic),
                 "Variable g removed with crit = ", round(bic_v, 2), ".")

  d$stay <- 1:4
  expect_message(backward(d, crit = bic),
                 "Variable g removed with crit = ", round(bic_v, 2), ".")
  d$stay <- 1:5
  expect_message(backward(d, crit = bic), "There are no variables to remove")
})

test_that("Rare situations", {
  d <- prepare_data(y, X)
  expect_equal(backward(d, crit = bic)$Xm, matrix(1, nrow = n))
})

context("Multi-backward")

test_that("Right variables to remove", {
  d$Xm <- cbind(1, X)
  res <- multi_backward(d, crit = bic)
  expect_equal(res$Xm, Xm)
  expect_equal(res$model, c("b", "c", "f"))
  expect_equal(res$crit, bic_v)

  d$Xm <- cbind(1, X[, 3:10])
  expect_equal(multi_backward(d, crit = bic)$Xm, Xm[, c(1, 3:4)])

  d$Xm <- cbind(1, X[, 6, drop = FALSE])
  expect_equal(multi_backward(d, crit = bic)$Xm, Xm[, 1, drop = FALSE])
  d$Xm <- cbind(1, X[, 2, drop = FALSE])
  expect_equal(multi_backward(d, crit = bic)$Xm, Xm[, 1:2])
  d$Xm <- cbind(1, X[, 6, drop = FALSE])
  expect_equal(multi_backward(d, crit = aic)$Xm, Xm[, c(1, 4)])

  d$Xm <- cbind(1, X)
  d$stay <- 1:5
  expect_equal(multi_backward(d, crit = bic)$Xm, cbind(1, X[, c(1:4, 6)]))
})

test_that("Rare situations", {
  d <- prepare_data(y, X)
  expect_equal(multi_backward(d, crit = bic)$Xm, matrix(1, nrow = n))
  d$Xm <- cbind(1, X)
  d$stay <- 1:11
  expect_equal(multi_backward(d, crit = bic)$Xm, cbind(1, X))
})
