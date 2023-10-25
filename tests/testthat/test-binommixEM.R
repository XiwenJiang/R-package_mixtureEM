
test_that("binomial mixture EM", {
  binommixEM_out <- binommixEM(binommix_sample$x, binommix_sample$n, k=2, pi_inits=c(0.2, 0.8), p_inits=c(0.3, 0.4))
  pi_test <- round(binommixEM_out$pi, 2)
  p_test <- round(binommixEM_out$p, 2)
  expect_setequal(pi_test, c(0.27, 0.73))
  expect_setequal(p_test, c(0.38, 0.69))
})

