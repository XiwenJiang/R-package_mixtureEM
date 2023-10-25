test_that("Poisson mixture EM", {
  poismixEM_out <- poissonMixEM(test_pois$x, pi = pois_pi_init, lambda = pois_lambda_init)
  pi_test <- round(poismixEM_out$pi, 2)
  lambda_test <- round(poismixEM_out$lambda, 1)
  expect_setequal(pi_test, c(0.16, 0.35,0.50))
  expect_setequal(lambda_test, c(67.7, 49.4, 90.7))
})
