#' Fast-forward step
#'
#' Add variables to a model as long as they reduce the given criterion.
#' Variables are searched according to \code{candidates} and every one which
#' reduces the criterion is added (not necessarily the best one).
#'
#' @inheritParams forward
#' @param maxf a numeric, a maximal number of variables in the final model.
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
#' fast_forward(d)
#' @export

fast_forward <- function(data, crit = bic, ..., maxf = 70) {
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
  lcan <- length(candidates)
  data$model <- colnames(Xm)[-1]

  if (all(colnames(X)[candidates] %in% colnames(Xm))) {
    if (verb) message("There are no variables to add.\n")
    return(invisible(data))
  }

  if (maxf > lcan) maxf <- lcan
  loglik <- loglik(y, Xm, fit_fun, na)
  crit_v <- R.utils::doCall(crit, loglik = loglik, n = n, k = k - s, p = p,
                            Xm = Xm, ...)
  data$crit <- crit_v

  if (verb)
    message("Starting the fast-forward, ", k - 1, " variables, crit = ",
            round(crit_v, 2), ", ", metric, " = ", round(metric(data), 3), ".")
  add <- NULL
  parts <- create_parts(candidates, n, maxp)
  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- X[, vars, drop = FALSE]
    for (i in seq_along(vars)) {
      Xm_new <- cbind(Xm, XX[, i, drop = FALSE])
      loglik <- loglik(y, Xm_new, fit_fun, na)
      crit_v_new <- R.utils::doCall(crit, loglik = loglik, n = n, k = k + 1 - s,
                                    p = p, Xm = Xm, ...)
      if (crit_v_new < crit_v) {
        add <- c(add, vars[i])
        k <- k + 1
        crit_v <- crit_v_new
        Xm <- Xm_new
        data$Xm <- Xm
        data$model <- colnames(Xm)[-1]
        data$metric_v <- metric(data)
        if (verb)
          message("Variable ", colnames(Xm)[k], " added with crit = ",
                  round(crit_v, 2), ", ", metric, " = ",
                  round(data$metric_v, 3), ".")
        if (length(add) == maxf) {
          if (verb) message("Done.\n")
          return(invisible(data))
        }
      }
    }
  }

  if (verb) message("Done.\n")
  return(invisible(data))
}
