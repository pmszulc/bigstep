context("Stepwise")

set.seed(1)
n <- 30
p <- 10
X <- matrix(rnorm(p*n), ncol = p)
colnames(X) <- letters[1:p]
y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
d <- prepare_data(y, X)

Xm <- cbind(1, X[, c(2, 3, 6)])
loglik <- loglik(y, Xm)
bic_v <- round(bic(loglik, k = 4, n = n), 2)

test_that("Typical data", {
  res <- stepwise(d, crit = bic)
  expect_equal(res$model, c("c", "f", "b"))

  d$Xm <- cbind(1, X)
  expect_equal(stepwise(d, crit = bic)$model, c("b", "c", "f"))
  d$Xm <- cbind(1, X[, 1:5])
  expect_equal(stepwise(d, crit = bic)$model, c("b", "c", "f"))

  d$Xm <- cbind(1, X[, c(1, 4, 9)])
  d$candidates <- (1:10)[-3]
  expect_equal(stepwise(d, crit = bic)$model, c("b", "j"))
  # is this model better than without "j"?
  lm1 <- lm(y ~ X[, 2] + X[, 10])
  lm2 <- lm(y ~ X[, 2])
  expect_true(stats::BIC(lm1) < stats::BIC(lm2))
  # is this model better than with "f" instead of "j?
  lm1 <- lm(y ~ X[, 2] + X[, 10])
  lm2 <- lm(y ~ X[, 2] + X[, 6])
  expect_true(stats::BIC(lm1) < stats::BIC(lm2)) # strange, but it is TRUE
  expect_equal(stepwise(d, crit = aic)$model, c("b", "j", "f")) # aic sees "f

  d$candidates <- 10:1
  d$Xm <- matrix(1, nrow = n)
  expect_equal(stepwise(d, crit = bic)$model, c("c", "f", "b"))
})

test_that("Rare situations", {
  d <- prepare_data(y, X)
  d$candidates <- NULL
  stepwise(d, crit = bic)
  expect_message(stepwise(d, crit = bic), "Done.")
  expect_equal(stepwise(d, crit = bic)$model, NULL)
})
