#' Forward step
#'
#' Add the best variable to a model according to the given criterion.
#'
#' @param data an object of class \code{big}.
#'
#' @param crit a function defining the model selection criterion. You can use
#'   your own function or one of these: \code{bic}, \code{mbic}, \code{mbic2},
#'   \code{aic}, \code{maic}, \code{maic2}.
#'
#' @param ... optional arguments to \code{crit}.
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
#' forward(d, crit = bic)
#' d %>%
#'   forward() %>%
#'   forward() %>%
#'   forward()
#'
#' @export

forward <- function(data, crit = mbic, ...) {
  stopifnot(class(data) == "big")
  y <- data$y
  X <- data$X
  fit_fun <- data$fit_fun
  candidates <- data$candidates
  Xm <- data$Xm
  na <- data$na
  maxp <- data$maxp
  verb <- data$verbose
  metric <- data$metric

  n <- length(y)
  k <- ncol(Xm)
  s <- length(data$stay)
  p <- ncol(X) + s
  data$model <- colnames(Xm)[-1]

  loglik <- loglik(y, Xm, fit_fun, na)
  crit_v <- R.utils::doCall(crit, loglik = loglik, n = n, k = k - s, p = p,
                            Xm = Xm, ...)
  data$crit <- crit_v

  if (all(colnames(X)[candidates] %in% colnames(Xm))) {
    if (verb & !data$stepwise) message("There are no variables to add.")
    data$crit <- crit_v
    return(invisible(data))
  }

  crit_v_new <- rep(Inf, length(candidates))
  parts <- create_parts(candidates, n, maxp)
  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- X[, vars, drop = FALSE]
    for (i in seq_along(vars)) {
      Xm_new <- cbind(Xm, XX[, i, drop = FALSE])
      loglik <- loglik(y, Xm_new, fit_fun, na)
      crit_v_new[vars[i]] <- R.utils::doCall(crit, loglik = loglik, n = n,
                                             k = k + 1 - s, p = p, Xm = Xm_new,
                                             ...)
    }
  }

  add <- which.min(crit_v_new)
  if (crit_v_new[add] < crit_v) {
    data$Xm <- cbind(Xm, X[, add, drop = FALSE])
    data$crit <- crit_v_new[add]
    data$metric_v <- metric(data)
    if (verb & !data$stepwise)
      message("Variable ", colnames(X)[add], " added with crit = ",
              round(crit_v_new[add], 2), ", ", metric, " = ",
              round(data$metric_v, 3), ".")
  } else {
    if (verb & !data$stepwise) message("There are no variables reducing crit.")
    data$crit <- crit_v
  }

  data$model <- colnames(data$Xm)[-1]
  return(invisible(data))
}
