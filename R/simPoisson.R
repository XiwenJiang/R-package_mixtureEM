#' Title simulating a mixture poisson model
#'
#' @param n number of observations
#' @param k number of components
#' @param sep amount of overlap between components (small sep = well-separated clusters)
#'
#' @return dataP a vector of simulated mixture poisson distribution
#' @export
#'
#' @examples simPoisson(35, 3)
#' 
simPoisson <- function(n=1000, k = 4, sep = 0.01){
  samp <- sample(c(1 : 100),k,replace = T)
  pi <- samp / sum(samp)
  lambda = sample(x = seq(from = 10,to = 50 * k,by = 20), size = k, replace = FALSE)
  gen.pois <- function(n,pi,lambda) {
    G <- length(pi)
    group <- sample(1:G, prob = pi, size = n, replace = TRUE)
    gtemp <- rep(0, G)
    for (g in 1:G) {gtemp[g] = length(group[group == g])}
    data <- c()
    for (g in 1:G) {data = c(data, rpois(n = gtemp[g], lambda = lambda[g]))}
    return(data)
  }
  dataP <- gen.pois(n, pi, lambda)
  return(dataP)
}

# > simPoisson(35, 3)
# [1]  83 105  93  91  83  85  86  81  94  82 101  76  80  85  89 101
# [17]  95  57  55  57  41  55  54  50  58  72  45  40  32  40  23  28
# [33]  32  31  28