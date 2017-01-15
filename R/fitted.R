
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
#fitted.blm <- function(x, ...) UseMethod("fitted")
fitted= function(x,...){

  predict(x,...)
}


