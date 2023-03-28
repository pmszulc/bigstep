context("All steps")

library("bigmemory")
set.seed(1)
n <- 100
p <- 10
X <- matrix(rnorm(p*n), ncol = p)
colnames(X) <- paste0("X", 1:p)
y <- X[, 2] + X[, 6] - X[, 10] + rnorm(n)

test_that("Typical data", {
  d <- prepare_data(y, X)
  expect_equal(fast_forward(d)$model, paste0("X", c(2, 6, 9, 10)))
  expect_equal(fast_forward(d, mbic, const = 0.1)$model, paste0("X", c(2, 6, 10)))
  expect_equal(fast_forward(d, mbic, const = 9)$model, paste0("X", c(1, 2, 4, 6, 9, 10)))

  expect_equal(stepwise(d)$model, paste0("X", c(10, 6, 2)))
  expect_equal(stepwise(d, aic)$model, paste0("X", c(10, 6, 2, 1)))
  expect_equal(stepwise(d, mbic2, const = 4)$model, paste0("X", c(10, 6, 2)))
  expect_equal(stepwise(d, mbic2, const = 5)$model, paste0("X", c(10, 6, 2, 1)))
  # expect_equal(stepwise(d, mbic2, const = 5)$model, paste0("X", c(10, 6, 2, 1, 9)))
  # now we don't get this model for mbic2 because k > p/2

  m <- d %>%
    fast_forward(mbic, const = 10) %>%
    multi_backward()
  expect_equal(m$model, paste0("X", c(2, 6, 10)))
  m <- d %>%
    fast_forward(mbic, const = 10) %>%
    stepwise() %>%
    forward(crit = maic) %>%
    fast_forward() %>%
    backward() %>%
    backward() %>%
    backward(mbic, const = 1e-10)
  expect_equal(m$model, paste0("X", c(6, 10)))

  d$verbose <- FALSE
  m <- d %>%
    reduce_matrix() %>%
    fast_forward(mbic2, const = 10)
  expect_equal(m$model, paste0("X", c(10, 6, 2, 1)))
  m <- d %>%
    reduce_matrix(minpv = 0.01) %>%
    fast_forward(mbic2, const = 10)
  expect_equal(m$model, paste0("X", c(10, 6, 2)))
  m <- d %>%
    reduce_matrix(minpv = 1e-10) %>%
    fast_forward(mbic2, const = 10) %>%
    forward() %>%
    stepwise()
  expect_null(m$model)
})

test_that("Typical data + Xadd", {
  set.seed(2)
  Xm <- cbind(rnorm(n), X[, 6] + rnorm(n, sd = 0.2))
  d <- prepare_data(y, X, Xadd = Xm)

  expect_equal(fast_forward(d)$model, c("Xadd1", "Xadd2", paste0("X", c(2, 6, 9, 10))))
  expect_equal(fast_forward(d, crit = mbic, const = 0.1)$model,
               c("Xadd1", "Xadd2", paste0("X", c(2, 10))))
  expect_equal(fast_forward(d, crit = mbic, const = 9)$model,
               c("Xadd1", "Xadd2", paste0("X", c(2, 6, 9, 10))))

  expect_equal(stepwise(d)$model, c("Xadd1", "Xadd2", paste0("X", c(10, 2))))
  expect_equal(stepwise(d, crit = aic)$model, c("Xadd1", "Xadd2", paste0("X", c(10, 2, 6))))
  expect_equal(stepwise(d, mbic2, const = 3)$model, c("Xadd1", "Xadd2", paste0("X", c(10, 2))))
  expect_equal(stepwise(d, mbic2, const = 6)$model,
               c("Xadd1", "Xadd2", paste0("X", c(10, 2, 6))))

  m <- d %>%
    fast_forward(mbic2, const = 10) %>%
    stepwise() %>%
    forward(crit = maic2) %>%
    fast_forward() %>%
    backward() %>%
    backward() %>%
    backward(mbic, const = 1e-10)
  expect_equal(m$model, c("Xadd1", "Xadd2", "X10"))

  d$verbose <- FALSE
  m <- d %>%
    reduce_matrix(minpv = 1e-10) %>%
    fast_forward(mbic2, const = 10) %>%
    forward() %>%
    stepwise()
  expect_equal(m$model, c("Xadd1", "Xadd2"))
})

test_that("Bigmemory", {
  Xbig <- as.big.matrix(X)
  d <- prepare_data(y, Xbig, maxp = 200)
  expect_equal(fast_forward(d)$model, paste0("X", c(2, 6, 9, 10)))
  expect_equal(fast_forward(d, mbic, const = 0.1)$model, paste0("X", c(2, 6, 10)))
  expect_equal(fast_forward(d, mbic, const = 9)$model, paste0("X", c(1, 2, 4, 6, 9, 10)))

  expect_equal(stepwise(d)$model, paste0("X", c(10, 6, 2)))
  expect_equal(stepwise(d, aic)$model, paste0("X", c(10, 6, 2, 1)))
  expect_equal(stepwise(d, mbic2, const = 4)$model, paste0("X", c(10, 6, 2)))
  expect_equal(stepwise(d, mbic2, const = 5)$model, paste0("X", c(10, 6, 2, 1)))

  m <- d %>%
    fast_forward(mbic, const = 10) %>%
    multi_backward()
  expect_equal(m$model, paste0("X", c(2, 6, 10)))
  m <- d %>%
    fast_forward(mbic, const = 10) %>%
    stepwise() %>%
    forward(crit = maic) %>%
    fast_forward() %>%
    backward() %>%
    backward() %>%
    backward(mbic, const = 1e-10)
  expect_equal(m$model, paste0("X", c(6, 10)))

  d$verbose <- FALSE
  m <- d %>%
    reduce_matrix() %>%
    fast_forward(mbic2, const = 10)
  expect_equal(m$model, paste0("X", c(10, 6, 2, 1)))
  m <- d %>%
    reduce_matrix(minpv = 0.01) %>%
    fast_forward(mbic2, const = 10)
  expect_equal(m$model, paste0("X", c(10, 6, 2)))
  m <- d %>%
    reduce_matrix(minpv = 1e-10) %>%
    fast_forward(mbic2, const = 10) %>%
    forward() %>%
    stepwise()
  expect_null(m$model)
  rm(Xbig)
})

# test_that("Big data", {
#   # Xbig <- read.big.matrix("../../X.txt", sep = " ", type = "integer", header = TRUE,
#   #                         backingfile = "X.bin", descriptorfile = "X.desc")
#   Xbig <- attach.big.matrix("../../X.desc")
#   y <- read.table("../../Trait.txt")
#   # data <- prepare_data(y, Xbig) # slow because of checking NA
#   data <- prepare_data(y, Xbig, na = FALSE)
#
#   data %>%
#     reduce_matrix(minpv = 0.001) %>%
#     fast_forward(crit = bic, maxf = 50) %>%
#     multi_backward(crit = mbic) %>%
#     stepwise(crit = mbic) -> m
#   sort(m$model)
#
#   # ch01_19810 0.363, ch01_27796 0.994, ch01_32763 0.872, ch02_22034 0.379
#   # ch02_39189 0.943, ch03_10846 0.990, ch03_02703 0.460, ch04_05127 0.993
#   # ch05_07371 0.991, ch06_25838 0.895, ch08_15190 0.377, ch10_00444 0.990
#   # ch10_08265 0.377, ch11_12611 0.807, ch11_20057 0.358, ch12_03421 0.977
#   # ch14_06999 0.996, ch15_03859 0.932, ch16_04525 0.868, ch17_04306 0.942
#   # ch18_01031 0.382, ch19_01377 0.376, ch19_06378 0.991, ch22_00033 0.947
#
#   # correlation between y and ch01_19810 is only 0.05 (p = 0.106)
#   # for minpv = 0.001 we can't find ch01_19810 and CH16_SNP4525
#
#   m <- data %>%
#     fast_forward(crit = bic, maxf = 50)
# })

test_that("NA", {
  X[1, 1] <- NA
  X[2, 6] <- NA
  y[3] <- NA
  d <- prepare_data(y, X, verbose = FALSE)
  d %>%
    reduce_matrix(minpv = 1) %>%
    fast_forward(aic) %>%
    multi_backward() %>%
    stepwise() -> m
  expect_equal(m$model, c("X10", "X6", "X2"))

  Xbig <- as.big.matrix(X)
  y <- as.numeric(y > mean(y, na.rm = TRUE))
  d <- prepare_data(y, Xbig, maxp = 200, verbose = FALSE, type = "logistic")
  d %>%
    reduce_matrix(minpv = 1) %>%
    fast_forward(aic) %>%
    multi_backward() %>%
    stepwise() -> m
  expect_equal(m$model, c("X10", "X2", "X6"))
  rm(Xbig)
})
