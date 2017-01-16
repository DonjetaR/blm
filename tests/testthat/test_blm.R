context("blm")

test_that("test blm", {
  alpha=1
  beta=1
  x <- rnorm(5)
  #y <- rnorm(5)
  #z <- x/y
  a=1
  y=a+x
  model=y~x

  fit=blm(model,alpha,beta)
  coef_blm=coefficients(fit)
  #names(coef_blm)=NULL
  #expect_equal(coef_blm[1],1, tolerance=0.1)
  expect_error(blm(model,alpha=0,beta))
  expect_error(blm(model,alpha,beta=-1))
  expect_true(all(!sapply(blm, is.null)))
})



test_that("test confint", {
  alpha=1
  beta=1
  x <- rnorm(4)
  y <- rnorm(4, x)
  z <- x/y
  model=y~x+z
  fit_blm=blm(model,alpha,beta)
  fit_lm=lm(model)

  confint_fit_blm=blm::confint(fit_blm,level = 0.95)

  expect_error(confint(fit_blm,level=-1))
  expect_error(confint(fit_blm,level=2))

  expect_true(all(!sapply(confint, is.null)))


})

test_that("test prior", {
  #alpha=1
  beta=1
  x <- rnorm(40)
  y <- rnorm(40, x)
  z <- x/y
  model=y~x+z

  expect_error(make_prior(mode,alpha=0))
  expect_error(make_prior(mode,alpha=-3))
})

test_that("test coefficients", {
  alpha=1
  beta=1
  x=rnorm(5)
  y=rnorm(5)
  model=y~x
  fit_blm=blm(model,alpha,beta)
  fit_lm= lm(y~x)
  coef_fit_blm= coefficients(fit_blm)
  coef_fit_lm= coef(fit_lm)
  #expect_equal(coef_fit_blm[1], coef_fit_lm[1], tolerance=1)
  #expect_equal(coef_fit_blm[2], coef_fit_lm[2], tolerance=1)
  expect_true(all(!sapply(coefficients, is.null)))


})

test_that("test deviance", {
  alpha=1
  beta=1
  x <- stats::rnorm(4)
  y <- stats::rnorm(4, x)
  model=y~ x
  fit_blm=blm(model,alpha,beta)
  fit_lm=lm(model)

  deviance_blm=deviance(fit_blm)
  deviance_lm=stats::deviance(fit_lm)

  #expect_equal(deviance_blm[1],deviance_lm[1], tolerance=1)
})

test_that("test plot", {
  alpha=1
  beta=1
  x=rnorm(4)
  y=rnorm(4)
  model=y~x
  fit=blm(model,alpha,beta)

  plot(fit)

})

test_that("test predict", {
  alpha=1
  beta=1
  x=stats::rnorm(5)
  y=stats::rnorm(5)
  model=y~x
  fit_blm=blm(model,alpha,beta)
  fit_lm= lm(y~x)
  predict_fit_blm= predict(fit_blm)
  predict_fit_lm= stats::predict(fit_lm)
  #expect_equal(predict_fit_blm, predict_fit_lm, tolerance=0.1)

})

test_that("test print", {
  alpha=1
  beta=1
  x=rnorm(4)
  y=rnorm(4)
  model=y~x
  fit=blm(model,alpha,beta)

  print(fit)

})

test_that("test summary", {
  alpha=1
  beta=1
  x=rnorm(4)
  y=rnorm(4)
  model=y~x
  fit=blm(model,alpha,beta)

  summary(fit)

})


