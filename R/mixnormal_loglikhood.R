#' Calculate loglikelihood of mixture normal model
#'
#' @param y A numeric vector of mixture normal observations
#' @param theta_vector A numeric vector indicating proportions of mixing components.
#' @param mu_vector A numeric vector indicating mean of mixing components.
#' @param sigma_vector A numeric vector indicating standard deviation of mixing components.
#'
#' @return A number indicating the value of loglikelihood of mixture normal observations
#' @export
#' @importFrom stats dnorm
#'
#' @examples
#' y <- c(8.1, 8.2, 8.1, 8.2, 8.2, 7.4, 7.3, 7.4, 8.1, 8.1, 7.9, 7.8, 8.2, 7.9, 7.9, 8.1, 8.1)
#' mixnormal_loglikhood(y, theta_vector = c(0.2, 0.8), mu_vector = c(0.5, 2), sigma_vector = c(2, 1))
mixnormal_loglikhood <- function(y, theta_vector, mu_vector, sigma_vector) {

  # Return sum of finite element of a vector --------------------------------
  sum_finite <- function(x) {
    sum(x[is.finite(x)])
  }

  loglik <- 0
  clusters <- length(theta_vector)
  for (k in 1:clusters) {
    loglik <- loglik + sum_finite(log(theta_vector[k]) +
      log(dnorm(
        y, mu_vector[k],
        sqrt(sigma_vector[k])
      )))
  }
  return(loglik)
}
