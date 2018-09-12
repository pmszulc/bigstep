#' Stepwise
#'
#' Build a model according to the stepwise procedure (bidirectional) and the
#' given criterion.
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
#' stepwise(d)
#' d %>%
#'   fast_forward(crit = aic) %>%
#'   stepwise(crit = bic)
#'
#' @export

stepwise <- function(data, crit = mbic, ...) {
  stopifnot(class(data) == "big")
  y <- data$y
  fit_fun <- data$fit_fun
  Xm <- data$Xm
  na <- data$na
  maxp <- data$maxp
  verb <- data$verbose
  metric <- data$metric

  n <- length(y)
  k <- ncol(Xm)
  p <- ncol(data$X) + length(data$stay)
  data$stepwise <- TRUE
  data$model <- colnames(Xm)[-1]

  loglik <- loglik(y, Xm, fit_fun, na)
  crit_v <- R.utils::doCall(crit, loglik = loglik, n = n, k = k, p = p, Xm = Xm, ...)
  data$crit <- crit_v

  if (verb) message("Starting stepwise, ", k - 1, " variables, crit = ",
                    round(crit_v, 2), ", ", metric, " = ", round(metric(data), 2), ".")

  repeat {
    model <- data$model
    data_f <- forward(data, crit, ...)
    data_b <- backward(data, crit, ...)
    crit_v_new <- min(data_f$crit, data_b$crit)
    if (crit_v_new < crit_v) {
      crit_v <- crit_v_new
      if (data_f$crit < data_b$crit) {
        data <- data_f
        if (verb) message("Variable ", setdiff(data$model, model),
          " added with crit = ", round(data$crit, 2), ", ", metric, " = ",
          round(data$metric_v, 2), ".")
      } else {
        data <- data_b
        if (verb) message("Variable ", setdiff(model, data$model),
          " removed with crit = ", round(data$crit, 2), ", ", metric, " = ",
          round(data$metric_v, 2), ".")
      }
    } else break
  }

  if (verb) message("Done.\n")
  data$stepwise <- FALSE
  return(invisible(data))
}
