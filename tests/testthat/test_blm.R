context("blm")

test_that("blm", {
  alpha=1
  beta=1
  x <- rnorm(40)
  y <- rnorm(40, x)
  z <- x/y
  model=y~x+z

  fit=blm(model,alpha,beta)
})

context("prior")

test_that("alpha is zero", {
  #alpha=1
  beta=1
  x <- rnorm(40)
  y <- rnorm(40, x)
  z <- x/y
  model=y~x+z

  expect_error(make_prior(mode,alpha=0))
  expect_error(make_prior(mode,alpha=-3))
})



