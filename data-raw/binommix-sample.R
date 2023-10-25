## code to prepare `binommix_sample` dataset goes here

set.seed(2020)
nindex_sample <- rmultinom(1, size=100, prob=c(0.3, 0.7))
set.seed(2020)
x_sample <- rbinom(100, size=10, rep(c(0.4, 0.7), nindex_sample))
binommix_sample <- data.frame(cbind(x_sample, rep(10, 100)))  
colnames(binommix_sample) <- c("x", "n")
usethis::use_data(binommix_sample, overwrite = TRUE)

