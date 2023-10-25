
#' E step estimating Mixture Binomial
#'
#' @param x a numeric vector of observed successes in each set of Bernoulli trails
#' @param n a numeric vector of total Bernoulli trails corresponding to x 
#' @param pi a numeric vector of current probability assigned to each binomial distribution 
#' @param p a numeric vector of probability of success in each of the binomial distribution 
#'
#' @return a numeric matrix of assignment probability for each observation
#' @export
#'
#' @examples u_x <- rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30); u_n <- rep(30, 1000)
#' binommixEstep(x=u_x, n=u_n, pi=c(0.5, 0.5), p=c(0.4, 0.2))
 
binommixEstep <- function(x, n, pi, p){
  K <- length(pi)
  N <- length(x)
  pi_est_vec <- matrix(0, nrow=N, ncol=K)
  
  for (j in 1:K)
  {
    pi_est_vec[,j] <- pi[j]*dbinom(x, n, p[j])
  }
  
  pi_est <-pi_est_vec/rowSums(pi_est_vec)
  return(pi_est)
}