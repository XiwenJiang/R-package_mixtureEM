# source("poissonEstep.R")

#' Optimize M step with parameter Pi 
#'
#' @param z a matrix got from E step
#'
#' @return pi {The assignment probability for mixture poisson distributions.} 
#' @export
#' @examples x = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28)
#' @examples Estep = poissonEstep(data = x, pi = c(0.5, 0.5), lambda = c(80,30))$z
#' @examples poissonMpi(Estep)

poissonMpi <- function(z) {
  n = dim(z)[1]
  pi = colSums(z)/n
  return(pi)
  
}

# poissonMpi(poissonEstep(data = c(94, 89, 94, 28, 25, 24, 34, 33, 34, 24, 33, 29, 35, 30, 29, 27, 24, 28), pi = c(0.5, 0.5), lambda = c(80,30))$z)
# > 0.1666667 0.8333333
