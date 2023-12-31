% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binommixEstep.R
\name{binommixEstep}
\alias{binommixEstep}
\title{E step estimating Mixture Binomial}
\usage{
binommixEstep(x, n, pi, p)
}
\arguments{
\item{x}{a numeric vector of observed successes in each set of Bernoulli trails}

\item{n}{a numeric vector of total Bernoulli trails corresponding to x}

\item{pi}{a numeric vector of current probability assigned to each binomial distribution}

\item{p}{a numeric vector of probability of success in each of the binomial distribution}
}
\value{
a numeric matrix of assignment probability for each observation
}
\description{
E step estimating Mixture Binomial
}
\examples{
u_x <- rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30); u_n <- rep(30, 1000)
binommixEstep(x=u_x, n=u_n, pi=c(0.5, 0.5), p=c(0.4, 0.2))
}
