#' Simulate Mixtures of Normal Distribution
#'
#' @param n A number indicating number of observations you want to simulate
#' @param ncluster A number indicating number of mixing components. The default value is 2
#' @param theta_vector A numeric vector indicating proportions of mixing components. The default value is (0.3,0.7)
#' @param mu_vector  A numeric vector indicating mean of mixing components. The default value is (1,3)
#' @param sigma_vector A numeric vector indicating standard deviation of mixing components. The default value is (3,0.4)
#'
#' @return A data frame with parameters and simulated observations
#' @export
#'
#' @examples
#' simu_mixnormal(100)
simu_mixnormal <- function(n, ncluster = NULL, theta_vector = NULL,
                           mu_vector = NULL, sigma_vector = NULL) {
  if (is.null(ncluster)) {
    ncluster <- 2
  }

  if (ncluster == 2) {
    if (is.null(theta_vector)) {
      theta_vector <- c(0.3, 0.7)
    }
    if (is.null(mu_vector)) {
      mu_vector <- c(1, 3)
    }
    if (is.null(sigma_vector)) {
      sigma_vector <- c(3, .4)
    }
  }

  if ((ncluster != 2) && (is.null(theta_vector) || is.null(mu_vector) ||
    is.null(sigma_vector))) {
    stop("If number of mixtures (ncluster) is not 2, user must supply mixing 
           proportions (theta_vector), mean (mu_vector) and variance (sigma_vector).")
  }

  if ((!is.null(theta_vector) && (ncluster != length(theta_vector))) ||
    (!is.null(mu_vector) && (ncluster != length(mu_vector))) ||
    (!is.null(sigma_vector) && (ncluster != length(sigma_vector)))) {
    stop("Length of theta_vector, mu_vector and sigma_vectortures must be same as ncluster.")
  }

  trueParams <- data.frame(
    "theta_vector" = theta_vector, "mu_vector" = mu_vector,
    "sigma_vector" = sigma_vector
  )
  z <- sample(1:ncluster, n, replace = TRUE, prob = theta_vector)
  d <- array(MASS::mvrnorm(n, mu = c(mu_vector), Sigma = diag(sigma_vector)),
    dim = c(n, length(mu_vector))
  )

  normObs <- array(0, dim = n)
  for (i in 1:ncluster) {
    normObs[z == i] <- d[z == i, i]
  }
  simObs <- data.frame("trueMix" = z, "normObs" = normObs)

  list("trueParams" = trueParams, "simObs" = simObs)
}
