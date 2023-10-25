
#' Calculating the Q function and log-likelihood of Mixture Poisson for every step 
#'
#' @param data a vector that stored the observed value data.
#' @param pi a vector that stored the probability of each component Poisson model from the last iteration.
#' @param lambda a vector that stored the mean parameter of each component Poisson model from the last iteration.
#'
#' @return 
#' \itemize{
#' \item{z} The 'z' value matrix which will be used in M-step.
#' \item{llike} The vector of log likelihood value
#' }
#' @export
#'
#' @importFrom stats dpois
#' 
#' @examples x = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28)
#' @examples poissonEstep(data = x, pi = c(0.5, 0.5), lambda = c(80,30))

poissonEstep <- function(data, pi, lambda) {
  G <- length(pi)
  n <- length(data)
  z <- matrix(0, nrow=n, ncol=G)
  llike <- 0
  for (i in 1:n) {
    like <- 0
    for (g in 1:G) {
      z[i, g] <- dpois(data[i], lambda[g], log=FALSE) * pi[g]
      like <- like + z[i, g]
    }
    z[i, ] <- z[i, ] / like
    llike <- llike + log(like)
  }
  out <- list(z, llike)
  names(out) <- c("z","llike")
  return(out)
  
}

###poissonEstep(data = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28), pi = c(0.5, 0.5), lambda = c(80,30))
