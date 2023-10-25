
#' Simulate mixture Binomial data
#'
#' @param n a numeric value specify the total sample size
#' @param alpha a numeric vector to specify assigned probabilities for each of the binomial distribution, should sum to 1
#' @param theta a numeric vector of p parameters specified for each of the binomial distributions
#' @param size a numeric value specify the total Bernoulli trails  
#'
#' @return a vector of n simulated successes following a mixture binomial distribution
#' @export
#' @import stats
#'
#' @examples rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30)

rBinomMix <- function(n, alpha, theta, size) {
  nindex <- rmultinom(1, size=n, prob=alpha)
  rbinom(n, size, rep(theta, nindex))
}
