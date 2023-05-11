# Loglik
loglik <- function(y, X, fit_fun = fit_linear, na = TRUE) {
  if (na) {
    compl <- complete.cases(X)
    X <- X[compl, , drop = FALSE]
    y <- y[compl]
  }
  loglik_v <- fit_fun(y, X)
  return(loglik_v)
}

# Metrics: MSE and accuracy
metric <- function(data) {
  stopifnot(class(data) == "big")
  y <- data$y
  Xm <- data$Xm
  if (data$na) {
    compl <- complete.cases(Xm)
    Xm <- Xm[compl, , drop = FALSE]
    y <- y[compl]
  }

  if (data$type == "logistic") {
    model <- speedglm.wfit(y, Xm, family = binomial())
    y_pred <- Xm %*% model$coefficients
    y_pred <- 1 / (1 + exp(-y_pred))
    y_pred <- round(y_pred)
    metric_v <- mean(y == y_pred)

  } else if (data$type == "poisson") {
    model <- speedglm.wfit(y, Xm, family = poisson())
    y_pred <- Xm %*% model$coefficients
    y_pred <- exp(y_pred)
    metric_v <- mean((y - y_pred)^2)

  } else {
    model <- fastLmPure(Xm, y)
    y_pred <- model$fitted.values
    metric_v <- mean((y - y_pred)^2)
  }

  return(metric_v)
}

#' Get the model
#'
#' Extract the model (an object of class \code{lm}) from an object of class \code{big}.
#'
#' @param object an object of class \code{big}.
#'
#' @param ... Further arguments to be passed to or from other methods. They are
#'   ignored in this function.
#'
#' @return An object of class \code{lm}.
#'
#' @examples
#' set.seed(1)
#' n <- 30
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
#' d <- prepare_data(y, X)
#' m <- stepwise(d)
#' get_model(m)
#' @export

get_model <- function(object, ...) {
  stopifnot(class(object) == "big")
  y <- object$y
  Xm <- object$Xm

  if (object$type == "logistic") {
    if (ncol(Xm) == 1) {
      model <- glm(y ~ 1, family = binomial())
    } else {
      model <- glm(y ~ ., data = as.data.frame(Xm[, -1]), family = binomial())
    }

  } else if (object$type == "poisson") {
    if (ncol(Xm) == 1) {
      model <- glm(y ~ 1, family = poisson())
    } else {
      model <- glm(y ~ ., data = as.data.frame(Xm[, -1]), family = poisson())
    }

  } else {
    if (ncol(Xm) == 1) {
      model <- lm(y ~ 1)
    } else {
      model <- lm(y ~ ., data = as.data.frame(Xm[, -1]))
    }
  }
  return(model)
}

#' Summarizing model fit
#'
#' \code{summary} method for class \code{big}.
#'
#' @param object an object of class \code{big}.
#'
#' @param ... Further arguments to be passed to or from other methods. They are
#'   ignored in this function.
#'
#' @return An object of class \code{summary.lm}.
#'
#' @examples
#' set.seed(1)
#' n <- 30
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
#' d <- prepare_data(y, X)
#' m <- stepwise(d)
#' summary(m)
#' @export

summary.big <- function(object, ...) {
  model <- get_model(object, ...)
  summary_v <- summary(model)
  return(summary_v)
}

# Fitting functions
fit_linear <- function(y, X) {
  model <- fastLmPure(X, y, method = 3)
  rss <- sum(model$residuals^2)
  n <- length(y)
  loglik <- -n / 2 * log(rss / n)
  return(loglik)
}

fit_logistic <- function(y, X) {
  model <- speedglm.wfit(y, X, family = binomial())
  loglik <- model$logLik
  return(loglik)
}

fit_poisson <- function(y, X) {
  model <- speedglm.wfit(y, X, family = poisson())
  loglik <- model$logLik
  return(loglik)
}
