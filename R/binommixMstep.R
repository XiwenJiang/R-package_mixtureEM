
#' M step in estimating mixture binomial
#'
#' @param x a numeric vector of observed successes in each set of Bernoulli trails
#' @param n a numeric vector of total Bernoulli trails corresponding to x 
#' @param e.step output from binommixEstep, a numeric matrix of assignment probability for observed successes in each set of Bernoulli trails
#'
#' @return 
#' \itemize{
#'   \item{pi} {The expected assignment probability for mixture binomial distributions.}
#'   \item{p} {The probability parameters for mixture binomial distributions.}
#' }
#' 
#' @export
#'
#' @examples u_x <- rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30); u_n <- rep(30, 1000)
#' estep <- binommixEstep(x=u_x, n=u_n, pi=c(0.5, 0.5), p=c(0.2, 0.4))
#' binommixMstep(x=u_x, n=u_n, e.step=estep)

binommixMstep <- function(x, n, e.step){
  
  # estimate pis
  pi_temp <- colMeans(e.step)
  
  # estimate ps
  p_temp <- colSums(e.step * x) / colSums(e.step * n)
  
  output <- list(pi_temp, p_temp)
  names(output) <- c("pi", "p")
  
  return(output)   
}
