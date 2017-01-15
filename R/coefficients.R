
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @return A fitted model.
#' @export
coefficients.blm= function(x){
  row_names=rownames(x$mean)
  mean=as.vector(x$mean)
  names(mean)=row_names
  mean
}



