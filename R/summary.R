#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @export

summary<- function(x, ...){
  cat('\nCall:\n')
  print(x$func_call)

  cat('\nCoefficients:\n')
  print(coefficients.blm(x))

  cat('\nResiduals:\n')
  print(residuals(x))

  cat('\nDeviance:\n')
  print(deviance(x))
}