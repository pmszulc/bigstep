context("Fit model")

set.seed(1)
n <- 100
p <- 10
X <- matrix(rnorm(n*p), ncol = p)
y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
Xm <- cbind(1, X[, c(2, 3, 6, 10)])

test_that("Fit function and loglike", {
  log1 <- fit_linear(y, Xm)
  log2 <- -n/2 * log(sum(lm.fit(Xm, y)$residuals^2) / n)
  expect_equal(log1, log2)
  expect_equal(log2, loglik(y, Xm, na = FALSE))
  expect_equal(log2, loglik(y, Xm, na = TRUE))
})

test_that("Fit functions and loglike -- NA", {
  Xm[1, 3] <- NA
  Xm[10, 4] <- NA

  log1 <- loglik(y, Xm)
  rss <- sum(lm(y ~ ., data = as.data.frame(Xm))$residuals^2)
  log2 <- -(n-2)/2 * log(rss/(n-2)) # minus 2 NA
  expect_equal(log1, log2)
})

test_that("Metrics, summary, get_model", {
  d <- prepare_data(y, X)
  y_fit <- lm.fit(d$Xm, y)$fitted
  mse_v <- mean((y - y_fit)^2)
  expect_equal(mse_v, metric(d))
  expect_equal(unname(summary(d)$residuals), y - y_fit)

  m <- stepwise(d)
  mse_v <- mean(lm.fit(m$Xm, y)$residuals^2)
  expect_equal(mse_v, metric(m))
  expect_equal(unname(summary(m)$residuals), lm.fit(m$Xm, y)$residuals)

  coef1 <- unname(coef(get_model(m)))
  coef2 <- unname(lm.fit(Xm, y)$coef)
  expect_equal(sort(coef1), sort(coef2))
})
