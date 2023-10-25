## code to prepare `mixnormal_sample` dataset goes here

set.seed(295)
mixnormal_sample <- simu_mixnormal(500)
usethis::use_data(mixnormal_sample, overwrite = TRUE)
