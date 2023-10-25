#' EM algorithm of mixture normal distribution
#'
#' @param y A numeric vector of mixture normal observations.
#' @param theta_vector A numeric vector indicating proportions of mixing components.
#' @param mu_vector A numeric vector indicating mean of mixing components.
#' @param sigma_vector A numeric vector indicating standard deviation of mixing components.
#' @param small A number indicating the criterion for defining convergence. The default value is 1e-8.
#' @param max_iter A number indicating max iteration count. The default value is 1000.
#'
#' @return
#' \itemize{
#'   \item y - The mixture normal observations.
#'   \item theta - The vector of proportions of mixing components at the last iteration.
#'   \item mu - The vector of mean of mixing components at the last iteration.
#'   \item sigma - The vector of standard deviation of mixing components at the last iteration.
#'   \item iteration.count - The number of iterations performed.
#'   \item converge - A boolean indicator of whether the algorithm reach converged.
#' }
#'
#' @export
#' @importFrom stats dnorm
#'
#' @examples
#' y <- c(8.1, 8.2, 8.1, 8.2, 8.2, 7.4, 7.3, 7.4, 8.1, 8.1, 7.9, 7.8, 8.2, 7.9, 7.9, 8.1, 8.1)
#' mixnormal_EM(y, theta_vector = c(0.2, 0.8), mu_vector = c(0.5, 2), sigma_vector = c(2, 1))
mixnormal_EM <- function(y, theta_vector = NULL, mu_vector = NULL,
                         sigma_vector = NULL, small = 1e-5, max_iter = 1000) {
  # y: data
  # theta_vector: initial value of weights of normal distributions.
  # mu_vector: initial value of mean of normal distributions.
  # sigma_vector: initial value of sd of normal distributions.
  # small: decide when the iteration should stop.
  # max_iter: max iteration count.
  if ((is.null(theta_vector) || is.null(mu_vector) ||
    is.null(sigma_vector))) {
    stop("User must supply initial value of mixing proportions (theta_vector), 
         mean (mu_vector) and variance (sigma_vector).")
  }

  if ((length(theta_vector) != length(mu_vector)) ||
    (length(theta_vector) != length(sigma_vector)) ||
    (length(mu_vector) != length(sigma_vector))) {
    stop("Length of theta_vector, mu_vector and sigma_vectortures must be same.")
  }


  if (sum(theta_vector) != 1) {
    stop("sum of mixing proportions (theta_vector) should be equal to 1")
  }

  clusters <- length(mu_vector)
  N <- length(y)

  ri <- matrix(0, ncol = clusters, nrow = N)

  # initialize posterior prob of latent variable

  for (iter in 1:max_iter) {
    theta_old <- theta_vector
    mu_old <- mu_vector
    sigma_old <- sigma_vector
    loglikhood_old <- mixnormal_loglikhood(y, theta_old, mu_old, sigma_old)

    ### E-step
    ### compute posterior probability
    for (k in 1:clusters) {
      ri[, k] <- theta_vector[k] * dnorm(y, mu_vector[k],
        sd = sigma_vector[k]
      )
    }
    ri <- ri / rowSums(ri)

    ### M-step
    rk <- colSums(ri)
    theta_vector <- rk / N
    mu_vector <- (y %*% ri) / rk
    for (k in 1:clusters) {
      sigma_vector[k] <- sqrt(sum(ri[, k] * (y - mu_vector[k])^2) / rk[k])
    }

    loglikhood_new <- mixnormal_loglikhood(
      y, theta_vector, mu_vector,
      sigma_vector
    )

    if (max(abs(loglikhood_old - loglikhood_new)) <= small) {
      break
    }
  }
  out <- list(
    y, theta_vector, mu_vector, sigma_vector,
    iter, iter != max_iter
  )
  names(out) <- c("y", "theta", "mu", "sigma", "iteration.count", "converge")
  out
}
