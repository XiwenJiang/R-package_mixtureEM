# source("poissonEstep.R")

#' Optimize M step with parameter Lambda 
#'
#' @param z a matrix got from E step
#' @param data a vector that stored the observed value data
#'
#' @return lambda The vector of lambda for the components in the mixture poisson distribution.  
#' @export
#'
#' @examples X = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28)
#' @examples Estep = poissonEstep(data = X, pi = c(0.5, 0.5), lambda = c(80,30))$z
#' @examples poissonMlambda(Estep , data = X)

poissonMlambda <- function(z,data) {
  G = dim(z)[2]
  lambda = rep(0,G)
  for (g in 1:G) {lambda[g] = sum(z[,g]*data)/sum(z[,g])}
  return(lambda)
}

# poissonMlambda(poissonEstep(data = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28), pi = c(0.5, 0.5), lambda = c(80,30))$z, data = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28))
# > 92.33333 29.13333
