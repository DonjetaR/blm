#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
predict<- function(x, ...){


  responseless.formula <- delete.response(terms(x$formula))
  frame <- model.frame(responseless.formula, ...)
  phi <- model.matrix(responseless.formula, frame)


  m=x$mean
  #(t(m))
  means=vector(length=nrow(phi ))


  for (i in seq_along(means)){
    means[i]=t(m)%*%(phi[i,])
  }

  return(means)


}


