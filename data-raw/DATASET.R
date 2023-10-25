## code to prepare `DATASET` dataset goes here

set.seed(1234)
vec = runif(3, min=0, max=1)# setting initial value of EM
pois_pi_init = vec / sum(vec)
usethis::use_data(pois_pi_init, overwrite = TRUE)

set.seed(1234)
pois_lambda_init = sample(x=seq(from=10,to=50*3, by=20), size=3, replace=FALSE)
usethis::use_data(pois_lambda_init, overwrite = TRUE)

set.seed(123)
n = 10000
k = 3
simu_pois_dat <- simPoisson(n, k) # simulating data
test_pois<-poissonMixEM(simu_pois_dat,pi.init,lambda.init) # EM algorithm to cluster mixture data
poismixPlot(test_pois)# visualization 
usethis::use_data(test_pois, overwrite = TRUE)
