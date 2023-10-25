#' EM algorithm to estimate mixture binomial
#'
#' @param x a numeric vector of observed successes in each set of Bernoulli trails
#' @param n a numeric vector of total Bernoulli trails corresponding to x 
#' @param k a numeric value specific the number of binomial mixtures or clusters 
#' @param pi_inits a numeric vector of initial values in probability assigned to each binomial distribution 
#' @param p_inits a numeric vector of initial values in probability of success in each of the binomial distribution 
#' @param maxit max iterations 
#' @param tol stopping criteria
#'
#' @return 
#' \itemize{
#'   \item{pi} {The assignment probability for mixture binomial distributions.}
#'   \item{p} {The probability parameters for mixture binomial distributions.}
#'   \item{cluster_ind} {The cluster assignment matrix, 0 or 1.}
#'   \item{iter} {The number of iterations performed.}
#'   \item{loglikelihood} {The vector of log-likelihood in each iteration.}
#'   \item{x} {The mixture binomial observed successes.}
#'   \item{size} {The mixture binomial Bernoulli tails.}
#'   \item{soft_label} {The vector of numeric matrix of final assignment probability for each observation.}
#' }
#' @export
#' 
#' @examples u_x <- rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30); u_n <- rep(30, 1000)
#' binommixEM(x=u_x, n=u_n, k=2, pi_inits=c(0.3, 0.7), p_inits=c(0.4, 0.2))

binommixEM <- function(x, n, k,
                       pi_inits=rep(1/k, k), p_inits=runif(k, 0, 1), 
                       maxit=1000, tol=.Machine$double.eps) {
  
  if(sum(pi_inits)!=1) {
    stop("sum of pi should be equal to 1")
  } else if(length(unique(p_inits))==1) {
    warning("Same initial value for p paratmer for all binominal distributions would lead to same final p parameter for all binominal distributions")
  }
  
  # Initial parameter estimates
  flag <- 0
  pi_cur <- pi_inits
  p_cur <- p_inits
  llike <- NA
  
  K <- length(pi_inits)
  N <- length(x)
  pi_est_vec <- matrix(0, nrow=N, ncol=K)
  cluster_id <- matrix(0, nrow=N, ncol=K)
  
  
  for(iter in 1:maxit){
    cur <- cbind(pi_cur, p_cur)
    estep <- binommixEstep(x, n, pi_cur, p_cur)
    new <- binommixMstep(x, n, estep)
    pi_new <- new$pi
    p_new <- new$p
    new_step <- cbind(pi_new, p_new)
    
    for (j in 1:K)
    { pi_est_vec[,j] <- pi_new[j]*dbinom(x, n, p_new[j]) }
    
    llike[iter] <- sum( log(rowSums(pi_est_vec)) )
    
    
    if( sum(abs(cur - new_step) < tol) )
    { flag <- 1
    break}
    
    pi_cur <- pi_new
    p_cur <- p_new
  }
  
  if(!flag) warning("Did not converge\n")
  
  ids <- apply(pi_est_vec, MARGIN=1, FUN=which.max)
  for (j in 1:K)
  {
    cluster_id[ids==j, j] <-1
  }
  
  
  output <- list(pi_cur, p_cur, cluster_id, iter, llike[1:iter], x, n, pi_est_vec/rowSums(pi_est_vec))
  names(output) <- c("pi", "p", "cluster_ind", "iter", "loglikelihood", "x", "size", "soft_label")
  
  return(output)
}
