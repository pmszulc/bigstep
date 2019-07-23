#' Backward step
#'
#' Remove the worst variable from a model according to the given criterion.
#' @inheritParams forward
#'
#' @return An object of class \code{big}.
#'
#' @details Type \code{browseVignettes("bigstep")} for more details.
#'
#' @examples
#' set.seed(1)
#' n <- 30
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
#' d <- prepare_data(y, X)
#' d %>%
#'   fast_forward(crit = aic) %>%
#'   backward() %>%
#'   backward()
#'
#' @export

backward <- function(data, crit = mbic, ...) {
  stopifnot(class(data) == "big")
  y <- data$y
  fit_fun <- data$fit_fun
  Xm <- data$Xm
  na <- data$na
  stay <- data$stay
  verb <- data$verbose
  metric <- data$metric

  n <- length(y)
  k <- ncol(Xm)
  s <- length(stay)
  p <- ncol(data$X) + s
  data$model <- colnames(Xm)[-1]

  loglik <- loglik(y, Xm, fit_fun, na)
  crit_v <- R.utils::doCall(crit, loglik = loglik, n = n, k = k - s, p = p,
                            Xm = Xm, ...)
  data$crit <- crit_v

  if (k == s) {
    if (verb & !data$stepwise) message("There are no variables to remove.")
    data$crit <- crit_v
    return(invisible(data))
  }

  crit_v_new <- numeric(k - s)
  for (i in (s + 1):k) {
    Xm_new <- Xm[, -i, drop = FALSE]
    loglik <- loglik(y, Xm_new, fit_fun, na)
    crit_v_new[i - s] <- R.utils::doCall(crit, loglik = loglik, n = n,
                                         k = k - 1 - s, p = p, Xm = Xm_new, ...)
  }

  remove <- which.min(crit_v_new)
  if (crit_v_new[remove] < crit_v) {
    data$Xm <- Xm[, -(remove + s), drop = FALSE]
    data$crit <- crit_v_new[remove]
    data$metric_v <- metric(data)
    if (verb & !data$stepwise) message("Variable ", colnames(Xm)[remove + s],
      " removed with crit = ", round(crit_v_new[remove], 2), ", ", metric, " = ",
      round(data$metric_v, 3), ".")
  } else {
    if (verb & !data$stepwise) message("There are no variables reducing crit.")
    data$crit <- crit_v
  }

  data$model <- colnames(data$Xm)[-1]
  return(invisible(data))
}
