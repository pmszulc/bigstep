#' Multi-backward step
#'
#' Remove the worst variables from a model as long as they reduce the given
#' criterion (backward elimination).
#'
#' @inheritParams forward
#'
#' @details Type \code{browseVignettes("bigstep")} for more details.
#'
#' @return An object of class \code{big}.
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
#'   multi_backward(crit = bic)
#'
#' @export

multi_backward <- function(data, crit = mbic, ...) {
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

  if (k == s) {
    if (verb) message("There are no variables to remove.\n")
    return(invisible(data))
  }

  loglik <- loglik(y, Xm, fit_fun, na)
  crit_v <- R.utils::doCall(crit, loglik = loglik, n = n, k = k - s, p = p,
                            Xm = Xm, ...)
  data$crit <- crit_v

  if (verb)
    message("Starting the multi-backward, ", k - 1, " variables, crit = ",
            round(crit_v, 2), ", ", metric, " = ", round(metric(data), 3), ".")

  while (k > s) {
    data_new <- backward(data, crit, ...)
    crit_v_new <- data_new$crit
    if (crit_v_new < crit_v) {
      crit_v <- crit_v_new
      data <- data_new
    } else break
  }

  if (verb) message("Done.\n")
  return(invisible(data))
}
