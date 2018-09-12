#' Reducing number of variables
#'
#' Perform the Pearson correlation tests between a vector \code{y} and every
#' variable from a matrix \code{X} (separately) and remove uncorrelated
#' variables. The funcion is much faster when you do not have any missing values
#' (set \code{na = FALSE} in \code{prepare_data} in that case).
#'
#' @param data an object of class \code{big}.
#'
#' @param minpv a numeric. Variables with p-values for the Pearson correlation
#'   tests larger than \code{minpv} will be removed from \code{candidates}.
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
#' reduce_matrix(d)
#'
#' @export

reduce_matrix <- function(data, minpv = 0.15) {
  stopifnot(class(data) == "big")
  y <- data$y
  X <- data$X
  candidates <- data$candidates
  na <- data$na
  maxp <- data$maxp
  verb <- data$verbose

  n <- length(y)
  p <- ncol(X)
  parts <- create_parts(candidates, n, maxp)
  lp <- length(parts)

  if (verb) {
    message("Performing single tests...")
    pb <- utils::txtProgressBar(min = 0, max = lp, style = 3)
  }

  r <- numeric(p)
  if (na) {
    y_na <- is.na(y)
    y <- y[!y_na]
    nn <- length(y)
    n <- numeric(p) # for each variable n can be different
    for (j in 1:lp) {
      vars <- parts[[j]]
      XX <- X[!y_na, vars, drop = FALSE]

      for (i in seq_along(vars)) {
        X_na <- is.na(XX[, i])
        r[vars[i]] <- cor(y[!X_na], XX[!X_na, i])
        n[vars[i]] <- nn - sum(X_na)
      }
      if (verb) utils::setTxtProgressBar(pb, j)
    }
    if (verb) close(pb)

  } else {
    # fast way to calculate p-values of Pearson correlation
    y <- (y - mean(y)) / sd(y)
    for (j in seq_along(parts)) {
      vars <- parts[[j]]
      XX <- X[, vars, drop = FALSE]
      means <- colMeans(XX)
      sds <- matrixStats::colSds(XX, center = means)
      XX <- t((t(XX) - means) / sds)
      for (i in seq_along(vars))
        r[vars[i]] <- crossprod(y, XX[, i])
      if (verb) utils::setTxtProgressBar(pb, j)
    }
    if (verb) close(pb)
    r <- r/(n - 1)
  }

  t <- r*sqrt((n - 2)/(1 - r^2)) # n is a vector (NA)
  pv <- 2*stats::pt(abs(t), n - 2, lower = FALSE)

  ord <- order(pv)
  pv <- pv[ord]
  candidates <- ord[pv < minpv & !is.na(pv)]

  if (verb) message("Number of variables reduced to ", length(candidates), ".\n")

  data$candidates <- candidates
  return(invisible(data))
}
