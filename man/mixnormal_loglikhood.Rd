% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixnormal_loglikhood.R
\name{mixnormal_loglikhood}
\alias{mixnormal_loglikhood}
\title{Calculate loglikelihood of mixture normal model}
\usage{
mixnormal_loglikhood(y, theta_vector, mu_vector, sigma_vector)
}
\arguments{
\item{y}{A numeric vector of mixture normal observations}

\item{theta_vector}{A numeric vector indicating proportions of mixing components.}

\item{mu_vector}{A numeric vector indicating mean of mixing components.}

\item{sigma_vector}{A numeric vector indicating standard deviation of mixing components.}
}
\value{
A number indicating the value of loglikelihood of mixture normal observations
}
\description{
Calculate loglikelihood of mixture normal model
}
\examples{
y <- c(8.1, 8.2, 8.1, 8.2, 8.2, 7.4, 7.3, 7.4, 8.1, 8.1, 7.9, 7.8, 8.2, 7.9, 7.9, 8.1, 8.1)
mixnormal_loglikhood(y,theta_vector = c(0.2, 0.8),mu_vector = c(0.5, 2), sigma_vector = c(2, 1))
}
