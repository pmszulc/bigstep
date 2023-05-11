context("Fit model")

set.seed(1)
n <- 100
p <- 10
X <- matrix(rnorm(n*p), ncol = p)
y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
Xm <- cbind(1, X[, c(2, 3, 6, 10)])

test_that("Fit functions and loglike", {
  # linear
  log1 <- fit_linear(y, Xm)
  log2 <- -n/2 * log(sum(lm.fit(Xm, y)$residuals^2) / n)
  expect_equal(log1, log2)
  expect_equal(log2, loglik(y, Xm, na = FALSE))
  expect_equal(log2, loglik(y, Xm, na = TRUE))

  # poisson
  y <- round(y - min(y))
  y_fit <- glm.fit(Xm, y, family = poisson())$fitted # = exp(X*beta)
  log1 <- fit_poisson(y, Xm)
  log2 <- -sum(y_fit) + sum(y * log(y_fit)) - sum(log(factorial(y)))
  expect_equal(log1, log2)

  # logistic
  y <- as.numeric(y > mean(y))
  beta <- glm.fit(Xm, y, family = binomial())$coefficients
  y_fit <- Xm %*% beta
  log1 <- fit_logistic(y, Xm)
  log2 <- sum(-log(1 + exp(y_fit)) + y * y_fit)
  expect_equal(log1, log2)
})

test_that("Fit functions and loglike -- NA", {
  Xm[1, 3] <- NA
  Xm[10, 4] <- NA

  # linear
  log1 <- loglik(y, Xm)
  rss <- sum(lm(y ~ ., data = as.data.frame(Xm))$residuals^2)
  log2 <- -(n-2)/2 * log(rss/(n-2)) # minus 2 NA
  expect_equal(log1, log2)

  # poisson
  y <- round(y - min(y))
  y_fit <- glm(y ~ ., data = as.data.frame(Xm[, -1]), family = poisson())$fitted
  log1 <- loglik(y, Xm, fit_fun = fit_poisson)
  log2 <- -sum(y_fit) + sum(y[-c(1, 10)] * log(y_fit)) - sum(log(factorial(y[-c(1, 10)])))
  expect_equal(log1, log2)

  # logistic
  y <- as.numeric(y > mean(y))
  beta <- glm(y ~ ., data = as.data.frame(Xm[, -1]), family = binomial())$coefficients
  y_fit <- Xm[-c(1, 10), ] %*% beta
  log1 <- loglik(y, Xm, fit_fun = fit_logistic)
  log2 <- sum(-log(1 + exp(y_fit)) + y[-c(1, 10)] * y_fit)
  expect_equal(log1, log2)
})

test_that("Metrics, summary, get_model", {
  # linear
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

  # poisson
  y <- round(y - min(y))
  d <- prepare_data(y, X, type = "poisson")
  y_fit <- glm.fit(d$Xm, y, family = poisson())$fitted
  mse_v <- mean((y - y_fit)^2)
  expect_equal(mse_v, metric(d))

  m <- stepwise(d)
  mse_v <- mean(glm.fit(m$Xm, y, family = poisson())$residuals^2)
  y_pred <- exp(m$Xm %*% summary(m)$coefficients[, 1])
  y_pred2 <- exp(predict(glm(y ~ ., data = as.data.frame(m$Xm[, -1]), family = poisson())))
  expect_equal(as.numeric(y_pred), unname(y_pred2))
  expect_equal(mean((y - y_pred2)^2), metric(m))

  # logistic
  y <- as.numeric(y > mean(y))
  d <- prepare_data(y, X, type = "logistic")
  y_fit <- round(glm.fit(d$Xm, y, family = binomial())$fitted)
  acc_v <- mean(y == y_fit)
  expect_equal(acc_v, metric(d))

  m <- stepwise(d)
  acc <- round(glm.fit(m$Xm, y, family = binomial())$fitted)
  y_pred <- 1 / (1 + exp(-m$Xm %*% summary(m)$coefficients[, 1]))
  y_pred2 <- glm.fit(m$Xm, y, family = binomial())$fitted
  expect_equal(as.numeric(y_pred), unname(y_pred2))
  expect_equal(mean(y == round(y_pred2)), metric(m))

  # NA
  X[1, 6] <- NA
  y[2] <- NA
  d <- prepare_data(y, X, type = "logistic")
  m <- stepwise(d)
  y_pred <- glm(y ~ ., data = as.data.frame(X[, c(2, 3, 6, 10)]), family = binomial())$fitted
  expect_equal(mean(y[-(1:2)] == round(y_pred)), metric(m))
})
