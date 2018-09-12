#' Model selection
#'
#' Model selection using the stepwise procedure and the chosen criterion.
#'
#' The main goal of the package \code{bigstep} is to allow you to select a
#' regression model using the stepwise procedure when data is very big,
#' potentially larger than available RAM in your computer. What is more, the
#' package gives you a lot of control over how this procedure should look like.
#' At this moment, you can use one of these functions: \code{stepwise},
#' \code{forward}, \code{backward}, \code{fast_forward}, \code{multi_backward}
#' and combinations of them. They can be treated as blocks from which the whole
#' procedure of finding the best model is built.
#'
#' When your data is larger than RAM you have in your computer, it is
#' impossible to read it in a normal way. Fortunately, in a process of building
#' a regression model it is not necessary to have access to all predictors at the
#' same time. Instead, you can read only a part of the matrix \code{X}, check
#' all variables from that part and then read another one. To do that with this
#' package, you only need to read the matrix \code{X} using
#' \code{read.big.matrix} from \code{bigmemory} package. The \code{prepare_data}
#' function has a parameter \code{maxp} which represents the maximum size (that
#' is the number of elements) of one part. If \code{X} is bigger, it will be
#' splitted. It will be done even if your matrix is big but you have enough RAM
#' to read it in a normal way. It may seem unnecessary, but it is worth to do
#' because R is not very efficient in dealing with big matrices.
#'
#' Another problem with a large number of predictors is choosing an appropriate
#' criterion. Classical ones like AIC or BIC are bad choice because they will
#' almost certainly select a model with two many variables [1]. You can use
#' modifications of them like mBIC [2], mBIC2 [3], mAIC or mAIC2. In brief,
#' these criteria have much heavier penalty for the number of parameters, so
#' they prefer smaller models than their classic versions.
#'
#' If you want to read more, type \code{browseVignettes("bigstep")}
#'
#' @author Piotr Szulc
#'
#' @references
#' [1] M. Bogdan, J.K. Ghosh, M. Zak-Szatkowska. Selecting explanatory
#' variables with the modified version of Bayesian Information Criterion.
#' Quality and Reliability Engineering International, 24:989-999, 2008.
#'
#' [2] M. Bogdan, J.K. Ghosh, R.W. Doerge. Modifying the Schwarz Bayesian
#' Information Criterion to locate multiple interacting quantitative trait loci.
#' Genetics, 167:989-999, 2004.
#'
#' [3] F. Frommlet, A. Chakrabarti, M. Murawska, M. Bogdan. Asymptotic Bayes
#' optimality under sparsity for general distributions under the alternative,
#' Technical report, arXiv:1005.4753v2, 2011.
#'
#' @examples
#' \dontrun{
#' library(bigstep)
#'
#' ### small data
#' set.seed(1)
#' n <- 200
#' p <- 20
#' X <- matrix(rnorm(n * p), ncol = p)
#' colnames(X) <- paste0("X", 1:p)
#' y <- 1 + 0.4 * rowSums(X[, c(5, 10, 15, 20)]) + rnorm(n)
#'
#' data <- prepare_data(y, X)
#' results <- stepwise(data, crit = aic)
#' results$model
#' summary(results)
#'
#' ### bigger data
#' set.seed(1)
#' n <- 1e3
#' p <- 1e4
#' X <- matrix(rnorm(p * n), ncol = p)
#' colnames(X) <- paste0("X", 1:p)
#' Xadd <- matrix(rnorm(5 * n), n, 5)  # additional variables
#' colnames(Xadd) <- paste0("Xadd", 1:5)
#' y <- 0.2 * rowSums(X[, 1000 * (1:10)]) + Xadd[, 1] - 0.1 * Xadd[, 3] + rnorm(n)
#'
#' data <- prepare_data(y, X, Xadd = Xadd)
#' data %>%
#'   reduce_matrix(minpv = 0.15) %>%
#'   stepwise(mbic) ->
#'   results
#' summary(results)
#'
#' ### big data
#' Xbig <- read.big.matrix("X.txt", sep = " ", header = TRUE,
#'                         backingfile = "X.bin", descriptorfile = "X.desc")
#' # Xbig <- attach.big.matrix("X.desc") # much faster
#' y <- read.table("y.txt")
#' # data <- prepare_data(y, Xbig) # slow because of checking NA
#' data <- prepare_data(y, Xbig, na = FALSE) # set if you know that you do not have NA
#' data %>%
#'   reduce_matrix(minpv = 0.001) %>%
#'   fast_forward(crit = bic, maxf = 50) %>%
#'   multi_backward(crit = mbic) %>%
#'   stepwise(crit = mbic) -> m
#' summary(m)
#'
#' # more examples: type browseVignettes("bigstep")
#' }
#'
#' @docType package
#' @name bigstep
#' @importFrom RcppEigen fastLmPure
#' @importFrom speedglm speedglm.wfit
#' @importFrom stats complete.cases binomial poisson
#' @importFrom stats cor glm lm sd

NULL
