
<!-- README.md is generated from README.Rmd. Please edit that file -->
bigstep
=======

The main goal of the package `bigstep` is to allow you to select a regression model using the stepwise procedure when data is very big, potentially larger than available RAM in your computer. What is more, the package gives you a lot of control over how this procedure should look like. At this moment, you can use one of these functions: `stepwise`, `forward`, `backward`, `fast_forward`, `multi_backward` and combinations of them. They can be treated as blocks from which the whole procedure of finding the best model is built.

Small data
----------

``` r
# generate data
set.seed(1)
n <- 200
p <- 20
X <- matrix(rnorm(n * p), ncol = p)
colnames(X) <- paste0("X", 1:p)
y <- 1 + 0.4 * rowSums(X[, c(5, 10, 15, 20)]) + rnorm(n)
```

First, you have to convert your data to a proper format, an object of class `big`. It can be done using the function `prepare_data`. In most cases you will only need to specify a vector of responses, `y`, and a matrix of predictors, `X`.

``` r
library(bigstep)
data <- prepare_data(y, X)
```

Then, you can use the stepwise procedure with, for example, Akaike Information Criterion and `summary` function to get more information about the final model.

``` r
results <- stepwise(data, crit = aic)
results$model
summary(results)
```

You can use only one forward step (for example if you want to choose the best predictor).

``` r
forward(data, crit = aic)
```

What is important, results are in the same format as input data (class `big`), so you can use `forward` again or in combination with other functions (with different criteria if you like). The pipe (`%>%`) operator will be helpful. For every step the actual number of variables in a model, mean squared error (*MSE*) or accuracy (*ACC*; if you use the logistic regression) and a value of the chosen criterion (*crit*) are given.

``` r
data %>%
  forward(aic) %>% 
  forward(aic) %>% 
  forward(aic) %>% 
  backward(bic)
```

It may seem unnecessary or even unjustified for small data, but can be useful if you have a lot of predictors (see the next paragraph).

Bigger data
-----------

``` r
# generate data
set.seed(1)
n <- 1e3
p <- 1e4
X <- matrix(rnorm(p * n), ncol = p)
colnames(X) <- paste0("X", 1:p)
Xadd <- matrix(rnorm(5 * n), n, 5)  # additional variables
colnames(Xadd) <- paste0("Xadd", 1:5)
y <- 0.2 * rowSums(X[, 1000 * (1:10)]) + Xadd[, 1] - 0.1 * Xadd[, 3] + rnorm(n)
```

If you have a lot o predictors, it can be a good idea to remove those that are not related with `y`. You can do that using `reduce_matrix`. This function calculates p-values for the Pearson correlation test and every variable from `X` (separately). Variables with p-values larger than `minpv` will not be considered in the next steps (formally, they are removed from `candidates`, one of components of class `big`). Thanks to that, the whole stepwise procedure will be much quicker. What is more, `reduce_matrix` changes the order of predictors in such a way that at the beginning there will be variables with the smallest p-values. It is important if you want to use `fast_forward` function.

Another problem is choosing an appropriate criterion to such data. Classical ones like AIC or BIC are bad choice because they will almost certainly select a model with too many variables[1]. You can use modifications of them like mBIC[2], mBIC2[3], mAIC or mAIC2. In brief, these criteria have much heavier penalty for the number of parameters, so they prefer smaller models than their classic versions.

Additionally, in the example below we add variables from other matrix to a model (`Xadd`). It can be a good idea if there are predictors which are important for us and want them to remain at every stage of building the model.

``` r
data <- prepare_data(y, X, Xadd = Xadd)
data %>%
  reduce_matrix(minpv = 0.15) %>%
  stepwise(mbic) ->
  results
summary(results)

data %>%
  reduce_matrix(0.15) %>%
  stepwise(bic) # bad idea...
```

Sometimes it will be reasonable to start with a model with some good predictors and then use the stepwise procedure. It can be achieved if we use `fast_forward` which adds every variable that reduces a criterion (not necessarily the best one). It is important for that function to search for variables in a reasonable order (first, the most correlated with `y`), so you should use `fast_forward` after `reduce_matrix` (you can set `minpv = 1` if you do not want to remove any predictor, just change the order). It is good idea to run `fast_forward` with a criterion which does not have a heavy penalty for the size of a model, so for example BIC is better than mBIC. After adding a lot of variables, most of them will be useless, so it can be a good idea to perform the backward elimination---as long as there are variables reducing a criterion. Run `multi_bacward` to do that.

``` r
data %>%
  reduce_matrix() %>%
  fast_forward() %>% 
  multi_backward() %>% 
  stepwise()
```

If you are lucky, you do not have to run `stepwise` after `fast_forward` and `multi_backward` because you will already have the best model. It is important because `stepwise` consists of potentially many `backward` and `forward` steps and `forward` takes most time in whole procedure of building a model (we have to check every predictor to find the best one). So the fewer such steps, the faster you will get the model. It can be crucial if you have big data.

Big data
--------

Now, let consider data which is larger than RAM you have in your computer. It is impossible to read it in a normal way, but in a process of building regression model it is not necessary to have access to all predictors at the same time. Instead, you can read only a part of the matrix `X`, check all variables from that part and then read another one. To do that, you only need to read the matrix `X` using `read.big.matrix` from `bigmemory` package. The `prepare_data` function has a parameter `maxp` which represents the maximum size (that is the number of elements) of one part. If `X` is bigger, it will be splitted. It will be done even if your matrix is big but you have enough RAM to read it in a normal way. It may seem unnecessary, but it is worth to do because R is not very efficient in dealing with big matrices. Remember that `maxp` cannot be smaller than the number of observations (rows in `X`), by default it is 1e6.

In the code below we assume that you have a big matrix in a file *X.txt*. Reading such matrix can be slow, but if you set `backingfile` and `descriptorfile` you have to do that once and next time you can use `attach.big.matrix` which is much faster.

``` r
Xbig <- read.big.matrix("X.txt", sep = " ", header = TRUE,
                        backingfile = "X.bin", descriptorfile = "X.desc")
# Xbig <- attach.big.matrix("X.desc") # much faster
y <- read.table("y.txt")
# data <- prepare_data(y, Xbig) # slow because of checking NA
data <- prepare_data(y, Xbig, na = FALSE) # set if you know that you do not have NA
data %>%
  reduce_matrix(minpv = 0.001) %>%
  fast_forward(crit = bic, maxf = 50) %>%
  multi_backward(crit = mbic) %>%
  stepwise(crit = mbic) -> m
summary(m)
```

We set `minpv` to a low value to speed up the whole procedure, but be careful because you can lose some important predictors.

Own criteria
------------

You can easily define your own criterion. It can depend on log-likelihood (`loglik`), the number of observations (`n`), the number of all variables (`p`), the number of variables currently in a model (`k`), the matrix with variables which are currently in a model (`Xm`) and constants. These parameters will be sent to your criterion, exactly with such names (but you do not have to use all).

``` r
my_crit <- function(loglik, k, n, c1 = 0.5, c2 = 8) {
  -c1*loglik + 10*sqrt(k*c2)
}
m <- reduce_matrix(data, minpv = 0.15) # data from the paragraph "Bigger data"
stepwise(m, crit = my_crit)
stepwise(m, crit = function(loglik, k, n) -0.4*loglik + 10*sqrt(k*8))
```

Because your criterion has access to a model matrix (`Xm`), you can use untypical ones which depend, for example, on the average correlation between variables which are in a model.

General linear models
---------------------

The package allows you to fit logistic and Poisson models. All you need to do is setting the parameter `type` when you prepare data. Take note that the `reduce_matrix` function always calculates the Pearson correlation, even if you use general linear models.

``` r
# Poisson model
set.seed(1)
n <- 50
p <- 1000
X <- matrix(runif(n * p), ncol = p)
colnames(X) <- paste0("X", 1:p)
mu <- rowSums(X[, 100 * (1:5)])
y <- rpois(n, exp(mu))
data1 <- prepare_data(y, X, type = "linear")
data2 <- prepare_data(y, X, type = "poisson")
data1 %>%
  reduce_matrix() %>%
  stepwise() # did not see any variables
data2 %>%
  reduce_matrix() %>%
  stepwise()

# logistic model
set.seed(2)
n <- 100
X <- matrix(runif(n * p, -5, 5), ncol = p)
colnames(X) <- paste0("X", 1:p)
mu <- 0.8 * rowSums(X[, 100 * (1:5)])
prob <- 1 /( 1 + exp(-mu))
y <- rbinom(n, 1, prob)
data1 <- prepare_data(y, X, type = "linear")
data2 <- prepare_data(y, X, type = "logistic")
data1 %>%
  reduce_matrix() %>%
  stepwise()
data2 %>%
  reduce_matrix() %>%
  stepwise()
```

[1] M. Bogdan, J.K. Ghosh, M. Zak-Szatkowska. *Selecting explanatory variables with the modified version of Bayesian Information Criterion*. Quality and Reliability Engineering International, 24:989–999, 2008.

[2] M. Bogdan, J.K. Ghosh, R.W. Doerge. *Modifying the Schwarz Bayesian Information Criterion to locate multiple interacting quantitative trait loci*. Genetics, 167:989–999, 2004.

[3] F. Frommlet, A. Chakrabarti, M. Murawska, M. Bogdan. *Asymptotic Bayes optimality under sparsity for general distributions under the alternative*, Technical report, arXiv:1005.4753v2, 2011.
