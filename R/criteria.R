#' AIC
#'
#' Calculate AIC (Akaike Information Criterion).
#'
#' @param loglik A numeric, the log-likelihood.
#' @param k An integer >= 0, the number of selected variables.
#' @return A number, a value of AIC.
#' @examples
#' aic(10, 5)
#' @export

aic <- function(loglik, k) {
  stopifnot(k >= 0)
  aic_v <- -2*loglik + 2*k
  return(aic_v)
}

#' BIC
#'
#' Calculate BIC (Bayesian Information Criterion).
#'
#' @inheritParams aic
#' @param n An integer > 0, the number of observations.
#' @return A number, a value of BIC.
#' @examples
#' bic(10, 5, 100)
#' @export

bic <- function(loglik, k, n) {
  stopifnot(n > 0, k >= 0)
  bic_v <- -2*loglik + k*log(n)
  return(bic_v)
}

#' mBIC
#'
#' Calculate mBIC (modified Bayesian Information Criterion).
#'
#' @inheritParams bic
#' @param p An integer > 0, the number of all variables or a weight.
#' @param const A numeric > 0, the expected number of significant variables.
#' @return A number, a value of mBIC.
#' @examples
#' mbic(10, 5, 100, 50)
#' @export

mbic <- function(loglik, k, n, p, const = 4) {
  stopifnot(n > 0, k >= 0, p > 0, p/const > 1, p/k >= 1)
  mbic_v <- bic(loglik, k, n) + 2*k*log(p/const - 1)
  return(mbic_v)
}

#' mBIC2
#'
#' Calculate mBIC2 (the second version of modified Bayesian Information
#' Criterion).
#'
#' @inheritParams mbic
#' @return A number, a value of mBIC2.
#' @examples
#' mbic2(10, 5, 100, 50)
#' @export

mbic2 <- function(loglik, k, n, p, const = 4) {
  stopifnot(n > 0, k >= 0, p > 0, p/const > 1, p/k >= 1)
  penalty <- ifelse(k < 150, 2*log(factorial(k)), 2*sum(log(1:k)))
  # if k > 170, factorial(k) = Inf
  mbic2_v <- mbic(loglik, k, n, p, const) - penalty
  return(mbic2_v)
}

#' mAIC
#'
#' Calculate mAIC (modified Akaike Information Criterion).
#'
#' @inheritParams aic
#' @param p An integer > 0, the number of all variables or a weight.
#' @param const A numeric > 0, the expected number of significant variables.
#' @return A number, a value of mAIC.
#' @examples
#' maic(10, 5, 100, 50)
#' @export

maic <- function(loglik, k, p, const = 4) {
  stopifnot(p > 0, p/const > 1, p/k >= 1)
  maic_v <- aic(loglik, k) + 2*k*log(p/const - 1)
  return(maic_v)
}

#' mAIC2
#'
#' Calculate mAIC2 (the second version of modified Akaike Information
#' Criterion).
#'
#' @inheritParams maic
#' @return A number, a value of mAIC2.
#' @examples
#' maic2(10, 5, 100, 50)
#' @export

maic2 <- function(loglik, k, p, const = 4) {
  stopifnot(p > 0, p/const > 1, k >= 0, p/k >= 1)
  penalty <- ifelse(k < 150, 2*log(factorial(k)), 2*sum(log(1:k)))
  # if k > 170, factorial(k) = Inf
  maic2_v <- maic(loglik, k, p, const) - penalty
  return(maic2_v)
}
