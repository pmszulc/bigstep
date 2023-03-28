#' Data preparation
#'
#' Create an object of class \code{big} which is needed to perform the selection
#' procedure.
#'
#' @param y a numeric vector of dependent (target) variable.
#'
#' @param X a numeric matrix or an object of class \code{big.matrix}. The
#'   columns of \code{X} should contain dependent variables (predictors).
#'
#' @param type a string, type of the regression model you want to fit. You can
#'   use one of these: \code{"linear"}, \code{"logistic"}, \code{"poisson"}.
#'
#' @param candidates a numeric vector, columns from \code{X} which will be used
#'   in the selection procedure. The order is important. If \code{NULL}, every
#'   column will be used.
#'
#' @param Xadd a numeric matrix, additional variables which will be included in
#'   the model selection procedure (they will not be removed in any step). If
#'   \code{NULL}, \code{Xadd} will contain only a column of ones (the
#'   intercept). If you specify \code{Xadd}, a column of ones will be
#'   automatically added (it is impossible to not include the intercept).
#'
#' @param na a logical. There are any missing values in \code{X}? If
#'   \code{NULL}, it will be checked (it can take some time if \code{X} is big,
#'   so it is reasonable to set it).
#'
#' @param maxp a numeric. The matrix \code{X} will be splitted into parts with
#'   \code{maxp} elements. It will not change results, but it is necessary if
#'   your computer does not have enough RAM. Set to a lower value if you still
#'   have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#'   information during the selection procedure.
#'
#' @details The function automatically removes observations which have missing
#'   values in \code{y}. Type \code{browseVignettes("bigstep")} for more
#'   details.
#'
#' @return An object of class \code{big}.
#'
#' @examples
#' X <- matrix(rnorm(20), ncol = 4)
#' y <- X[, 2] + rnorm(5)
#' data <- prepare_data(y, X)
#'
#' @export

prepare_data <- function(y, X, type = "linear", candidates = NULL, Xadd = NULL,
                         na = NULL, maxp = 1e6, verbose = TRUE) {
  # y
  if (is.data.frame(y)) {
    stopifnot(length(y) == 1)
    y <- unlist(y, use.names = FALSE)
  }
  if (is.matrix(y)) {
    stopifnot(min(dim(y)) == 1)
    y <- as.numeric(y)
  }
  stopifnot(is.numeric(y), is.null(dim(y)))
  na_y <- is.na(y)
  if (any(na_y)) y <- y[!na_y]
  n <- length(y)

  # X
  stopifnot(is.numeric(X) | is.data.frame(X) | bigmemory::is.big.matrix(X))
  p <- ncol(X)
  if (!bigmemory::is.big.matrix(X)) {
    X <- as.matrix(X)
    if (is.null(colnames(X))) colnames(X) <- 1:p
  } else {
    if (is.null(colnames(X))) stop("Matrix X must have column names.")
  }
  if (any(na_y)) X <- X[!na_y, ]
  stopifnot(nrow(X) == n)

  # type
  stopifnot(type %in% c("linear", "logistic", "poisson"))
  fit_fun <- fit_linear

  # candidates
  if (is.null(candidates)) candidates <- 1:p
  stopifnot(is.numeric(candidates), all(candidates %in% 1:p))

  # Xadd
  if (is.null(Xadd)){
    Xm <- matrix(1, nrow = n)
  } else {
    stopifnot(is.numeric(Xadd) | is.data.frame(Xadd))
    Xm <- as.matrix(Xadd)
    if (any(na_y)) Xm <- Xm[!na_y, ]
    stopifnot(nrow(Xm) == n)
    if (is.null(colnames(Xm))) colnames(Xm) <- paste0("Xadd", 1:ncol(Xm))
    Xm <- cbind(1, Xm)
  }

  # X NA
  if (is.null(na)) na <- check_na(X, maxp)
  stopifnot(is.logical(na), length(na) == 1)

  # maxp
  stopifnot(is.numeric(maxp), length(maxp) == 1, maxp >= n)

  # others
  crit <- NULL
  metric <- "MSE"
  metric_v <- NULL
  stepwise <- FALSE
  stay <- 1:ncol(Xm)
  model <- colnames(Xm)[-1]

  data <- list(
    y = y,
    X = X,
    type = type,
    candidates = candidates,
    Xm = Xm,
    na = na,
    maxp = maxp,
    crit = crit,
    fit_fun = fit_fun,
    metric = metric,
    metric_v = metric_v,
    stepwise = stepwise,
    stay = stay,
    model = model,
    verbose = verbose
  )
  class(data) <- "big"
  return(data)
}

# checking if there are any NA values in X matrix
check_na <- function(X, maxp = 1e6) {
  parts <- create_parts(1:ncol(X), nrow(X), maxp)
  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- X[, vars, drop = FALSE]
    if (any(is.na(XX))) return(TRUE)
  }
  return(FALSE)
}

# creating parts
create_parts <- function(ind, n, maxp = 1e6) {
  stopifnot(is.numeric(ind), n > 0, maxp >= n)
  part <- round(maxp/n)
  parts <- split(ind, ceiling(seq_along(ind)/part))
  return(parts)
}
