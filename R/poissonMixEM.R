
#' Finding MLE of mixture Poisson models
#'
#' @param data a vector that stored observed data
#' @param pi a vector that stored the probability of each component from the last iteration
#' @param lambda a vector that stored the mean parameter of each component from the last iteration
#' @param small a numerical value that controlling the stopping criteria
#' @param maxIter a numerical value that limiting the maximum iterations
#'
#' @return 
#' \itemize{
#' \item{pi} {The assignment probability for mixture poisson distributions.}
#' \item{lambda} {The lambda parameter in the component poisson distributions.}
#' \item{x} {The mixture poisson observed successes.}
#' \item{z} {The 'z' value matrix which will be used in M-step.}
#' \item{iteration} {The number of iterations performed.}
#' \item{log-likelihood} {The vector of log likelihood value.}
#' }
#' @export
#'
#' @examples x = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28)
#' @examples pi.init = pois_pi_init
#' @examples lambda.init = pois_lambda_init
#' @examples poissonMixEM(x, pi.init,lambda.init)

poissonMixEM <- function(data, pi, lambda, small = 1e-4, maxIter = 1e3) {
  if(sum(pi) != 1) {
    stop("sum of pi should be equal to 1")
  } else if(length(unique(lambda)) == 1) {
    warning("Same initial value for lambda paratmer for all poisson distributions would lead to same final lambda parameter for all poisson distributions")
  }
  flag <- 0
  llike <- NA
  for (iter in 1:maxIter) {
    estep <- poissonEstep(data, pi, lambda)
    z <- estep$z
    llike[iter] <- estep$llike
    pi.current <- poissonMpi(z)
    lam.current <- poissonMlambda(z, data)
    if (sum(abs(lam.current-lambda)) < small){
      flag <- 1
      break
    }else{
      pi = pi.current
      lambda = lam.current
    }
  }
  if(!flag) warning("Did not converge\n")
  out = list(round(pi.current, 3), lam.current, data, z, iter, llike)
  names(out) = c("pi", "lambda", "x", "z", "iteration", "log_likelihood")
  return(out)
}