source("poissonEstep.R")
source("poissonMstepLambda.R")
source("poissonMstepPi.R")
source("simPoisson.R")
source("poissonMixPlot.R")

# 3 lambdas and 1000 observations -----------------------------------------
n = 1000
k = 3
testing <- simPoisson(n, k) # simulating data
vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)
test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data
poismixPlot(test_pois)# visualization 


# 2 lambdas and 850 observations -----------------------------------------
n = 850
k = 2
testing <- simPoisson(n, k) # simulating data

vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)

test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data

poismixPlot(test_pois)# visualization 


# 4 lambdas and 5230 observations -----------------------------------------
n = 5230
k = 4
testing <- simPoisson(n, k) # simulating data

vec = runif(k,min=0,max=1)# setting initial value of EM
pi.init = vec/sum(vec)
lambda.init = sample(x=seq(from=10,to=50*k,by=20), size=k, replace=FALSE)

test_pois<-poissonMixEM(testing,pi.init,lambda.init) # EM algorithm to cluster mixture data

poismixPlot(test_pois)# visualization 

