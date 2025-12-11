test_that("psi works", {
  expect_equal(psi(5,distribution = "poisson",n=10)[[1]](0.3,2), 1)
  expect_equal(psi("GM",distribution = "poisson",n=100)[[1]](0.3,90),0)
})
