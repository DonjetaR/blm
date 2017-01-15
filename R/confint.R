
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @param level   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
confint <- function(x, level = 0.95, ...){
  a=c((1-level)/2, 1-(1-level)/2)

  variables = names(coefficients(x))
  m <- matrix(0, nrow=length(variables), ncol=2)
  colnames(m) <- c(paste(a[1]*100, "%"), paste(a[2]*100, "%"))
  rownames(m) = variables

  for(i in variables){
    m[i,1] = qnorm(a[1], mean=x$mean[i,1], sd=sqrt(x$covar[i,i]))
    m[i,2] = qnorm(a[2], mean=x$mean[i,1], sd=sqrt(x$covar[i,i]))

  }
  m
}



