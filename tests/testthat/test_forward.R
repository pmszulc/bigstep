context("Forward")

suppressWarnings(RNGversion("3.5.0"))
set.seed(1)
n <- 30
p <- 10
X <- matrix(sample(0:2, p*n, replace = TRUE), ncol = p)
colnames(X) <- 1:p
y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
d <- prepare_data(y, X)
d$Xm <- cbind(1, X[, 2:3])

test_that("Right variable to add", {
  res <- forward(d, crit = bic)
  expect_equal(res$model, c("2", "3", "6"))
  Xm <- cbind(1, X[, c(2, 3, 6)])
  expect_equal(res$Xm, Xm)
  loglik <- loglik(y, Xm)
  bic_v <- round(bic(loglik, k = 3, n = n), 2)
  expect_message(forward(d, crit = bic),
    paste0("Variable 6 added with crit = ", bic_v, "."))

  d <- prepare_data(y, X, Xadd = NULL)
  expect_equal(forward(d, crit = bic)$model, "3")
  d$Xm <- cbind(1, X[, c(2, 3, 6)])
  expect_equal(forward(d, crit = bic)$model, c("2", "3", "6"))

  d$Xm <- cbind(1, X[, 2:3])
  d$candidates <- (1:10)[-6]
  expect_equal(forward(d, crit = bic)$model, c("2", "3"))
  d$candidates <- 10:1
  res <- forward(d, crit = bic)
  expect_equal(res$model, c("2", "3", "6"))
  expect_equal(res$candidates, 10:1)

  Xm <- 1:30
  d <- prepare_data(y, X, Xadd = Xm)
  expect_equal(forward(d, crit = bic)$model, c("Xadd1", "3"))
  Xm <- cbind(Xm, X[, 3])
  colnames(Xm) <- c("V1", "V2")
  d <- prepare_data(y, X, Xadd = Xm)
  res <- forward(d, crit = bic)
  expect_equal(d$stay, 1:3)
  expect_equal(res$model, c("V1", "V2", "2"))
})

test_that("Rare situations", {
  d$Xm <- cbind(1, X)
  expect_message(forward(d, crit = bic), "There are no variables to add.")
  expect_equal(forward(d, crit = bic)$model, as.character(1:10))
})

context("Fast-forward")

test_that("Right variables to add", {
  d <- prepare_data(y, X)
  res <- fast_forward(d)
  expect_equal(colnames(res$Xm)[-1], c("2", "3", "6"))

  d$Xm <- cbind(1, X[, -5])
  expect_equal(fast_forward(d)$Xm, d$Xm)

  Xm <- cbind(1:30, X[, 3])
  colnames(Xm) <- c("V1", "V2")
  d <- prepare_data(y, X, Xadd = Xm)
  res <- fast_forward(d, crit = bic)
  expect_equal(res$model, c("V1", "V2", "2", "6"))
})

test_that("Rare situations", {
  d <- prepare_data(y, X)
  expect_equal(colnames(fast_forward(d, maxf = 2)$Xm)[-1], c("2", "3"))

  suppressWarnings(RNGversion("3.5.0"))
  set.seed(1)
  n <- 100
  p <- 10
  X <- matrix(rnorm(p*n), ncol = p)
  colnames(X) <- 1:p
  y <- rowSums(X) + rnorm(n)
  d <- prepare_data(y, X)
  expect_equal(fast_forward(d)$Xm[, -1], X)
})
