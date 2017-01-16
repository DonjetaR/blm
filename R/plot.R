
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param x   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @import graphics
#' @export
plot.blm = function(x,...){
  variables = names(coefficients(x))

  plot(x$formula[,2],x$formula[,1],xlab=variables[2], ylab="y",main="Bayesian Regression")
  abline(x$mean[1],x$mean[2], col="red")
  points(x$formula[,2],fitted(x),col="blue",pch=15)
  legend("bottomright", c("Regression data","Regession Line", "Fitted data"),
         col=c("black", "red","blue"),
         pch = 1 )
}

