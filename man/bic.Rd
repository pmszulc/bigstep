% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criteria.R
\name{bic}
\alias{bic}
\title{BIC}
\usage{
bic(loglik, k, n)
}
\arguments{
\item{loglik}{A numeric, the log-likelihood.}

\item{k}{An integer >= 0, the number of selected variables.}

\item{n}{An integer > 0, the number of observations.}
}
\value{
A number, a value of BIC.
}
\description{
Calculate BIC (Bayesian Information Criterion).
}
\examples{
bic(10, 5, 100)
}
