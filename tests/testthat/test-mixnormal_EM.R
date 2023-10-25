context("testing whether normal mixture EM function works")

test_that("checking normal mixture EM", {
  normalEM_out <- mixnormal_EM(mixnormal_sample$simObs$normObs,
    theta_vector = c(0.1, 0.9),
    mu_vector = c(0.4, 1),
    sigma_vector = c(2, 1)
  )
  theta_test <- round(normalEM_out$theta, 2)
  mu_test <- round(normalEM_out$mu, 2)
  sigma_test <- round(normalEM_out$sigma, 2)
  r_out <- mixtools::normalmixEM(mixnormal_sample$simObs$normObs,
    k = 2,
    lambda = c(0.2, 0.8),
    mu = c(0.5, 2),
    sigma = c(2, 1)
  )
  theta_r <- round(r_out$lambda, 2)
  mu_r <- round(r_out$mu, 2)
  sigma_r <- round(r_out$sigma, 2)
  expect_setequal(theta_test, theta_r)
  expect_setequal(mu_test, mu_r)
  expect_setequal(sigma_test, sigma_r)
})
