# EM and parameter update functions for Poisson mixtures

# E-step updates
poissonEstep <- fun
ction(data, pi, lambda) {
  G <- length(pi)
  n <- length(data)
  z <- matrix(0, nrow=n, ncol=G)
  llike <- 0
  for (i in 1:n) {
    like <- 0
    for (g in 1:G) {
      z[i, g] = dpois(data[i], lambda[g], log=FALSE) * pi[g]
      like = like + z[i, g]
    }
    z[i, ] = z[i, ] / like
    llike = llike + log(like)
  }
  out <- list(z, llike)
  names(out) <- c("z","llike")
  return(out)
}

# M-step updates

poissonMpi <- function(z) {
  n = dim(z)[1]
  pi = colSums(z)/n
  return(pi)
}

poissonMlambda <- function(z,data) {
  G = dim(z)[2]
  lambda = rep(0,G)
  for (g in 1:G) {lambda[g] = sum(z[,g]*data)/sum(z[,g])}
  return(lambda)
}

poissonMixEM <- function(data,pi,lambda,small = 1e-4, maxIter = 1e3) {
  if(sum(pi)!=1) {
    stop("sum of pi should be equal to 1")
  } else if(length(unique(lambda))==1) {
    warning("Same initial value for lambda paratmer for all poisson distributions would lead to same final lambda parameter for all poisson distributions")
  }
  flag = 0
  llike <- NA
  for (iter in 1:maxIter) {
    estep = poissonEstep(data, pi, lambda)
    z = estep$z
    llike[iter] = estep$llike
    pi.current = poissonMpi(z)
    lam.current = poissonMlambda(z, data)
    if (sum(abs(lam.current-lambda)) < small){
      flag = 1
      break
    }else{
      pi = pi.current
      lambda = lam.current
    }
  }
  if(!flag) warning("Didn’t converge\n")
  out = list(round(pi.current,3),lam.current, data, z, iter,llike)
  names(out) = c("pi", "lambda", "x", "z", "iteration","log_likelihood")
  return(out)
}


# visualization function --------------------------------------------------

poismixPlot <- function(poismixem) {
  
  component_colors <- c(
    "red",
    "blue",
    "green",
    "yellow",
    "orange",
    "purple",
    "darksalmon",
    "goldenrod2",
    "dodgerblue",
    "darkorange3",
    "burlywood4",
    "darkmagenta",
    "firebrick",
    "deeppink2",
    "darkseagreen1"
  )
  simudata <- c()
  label <- c()
  for (i in 1:length(poismixem$pi)) {
    simudata <- c(simudata, rpois(n = length(poismixem$x)*poismixem$pi[i], 
                                  lambda = poismixem$lambda[i]))
    label <- c(label, rep(i, length(poismixem$x)*poismixem$pi[i]))
  }
  sim <- data.frame(cbind(simudata, label))
  
  dat<- data.frame(poismixem$x)
  colnames(dat) <- "lambda"
  
  out_plot <- ggplot2::ggplot(dat) +
    ggplot2::geom_density(ggplot2::aes(x = lambda),
                          colour = "darkgray",
                          fill = "lightgray"
    )
  
  K = length(poismixem$pi)
  for (j in 1:K) {
    out_x <- sim[label==j,1]
    out_y <- dpois(out_x, lambda = poismixem$lambda[j])/K
    out_plot <- out_plot + 
      ggplot2::geom_line(data=data.frame(out_x, out_y), 
                         ggplot2::aes(x=out_x, y=out_y), 
                color=component_colors[j]) +
      ggplot2::theme_minimal()
  }
  return(out_plot)
}



# simulating mix poisson data ---------------------------------------------

simPoisson <- function(n=1000, k = 4, sep = 0.01){
  ## n is total number of observations
  ## k is number of components
  ## sep is amount of overlap between components (small sep = well-separated clusters)
  samp <- sample(c(1:100),k,replace = T)
  pi <- samp/sum(samp)
  lambda = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)
  gen.pois <- function(n,pi,lambda) {
    G = length(pi)
    group = sample(1:G, prob=pi, size=n, replace=TRUE)
    gtemp = rep(0,G)
    for (g in 1:G) {gtemp[g] = length(group[group==g])}
    data = c()
    for (g in 1:G) {data = c(data, rpois(n = gtemp[g], lambda = lambda[g]))}
    return(data)
  }
  dataP = gen.pois(n,pi,lambda)
  return(dataP)
}

# testing -----------------------------------------------------------------

## suppose we want to a mixture Poisson data with 3 lambdas and 1000 observations
n = 1000
k = 3
testing <- simPoisson(n, k) # simulating data

vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)

test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data

poismixPlot(test_pois)# visualization 

## suppose we want to a mixture Poisson data with 2 lambdas and 850 observations
n = 850
k = 2
testing <- simPoisson(n, k) # simulating data

vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)

test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data

poismixPlot(test_pois)# visualization 


## suppose we want to a mixture Poisson data with 4 lambdas and 5230 observations
n = 5230
k = 4
testing <- simPoisson(n, k) # simulating data

vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)

test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data

poismixPlot(test_pois)# visualization 

