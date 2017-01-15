
#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param alpha   Alpha
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
make_prior <- function(alpha,model){
  if (alpha<1){
    print("Invalid alpha")
    break
  }
  O= ncol(model.matrix(model)) #mean
  S=1/alpha*diag(O) #Covariance, sigma

  return(list(mean=vector("numeric", length = O),Sigma=S))
}

#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param prior   Prior
#' @param beta   Beta
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
update <- function(model, prior, beta ,...) {
  if (!is.numeric(model.matrix(model))){
    print("non-numerical Sigma")
    break
  }

  model_frame=model.frame(model)
  model_matrix=model.matrix(model)


  #temp_beta = c(beta)

  S_inv=(prior$Sigma) + (beta * t(model_matrix) %*% model_matrix)
  #S=solve(S_inv)*S_inv
  S=solve(S_inv)

  m=(beta)*S%*%t(model_matrix)%*%model_frame[,1]

  return(list(mean=m,Sigma=S))
}


#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param alpha   Alpha
#' @param beta    Beta
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
blm <- function(model, alpha,beta,...) {

  prior <- make_prior(alpha, model)
  posterior <- update(model, prior, beta)


  structure(list(formula=model.frame(model),
                 prior=prior,
                 posterior=posterior,
                 mean=posterior$mean,
                 covar=posterior$Sigma,
                 func_call = sys.call()),
                 class="blm")
}




#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
coefficients.blm= function(x){
  row_names=rownames(x$mean)
  mean=as.vector(x$mean)
  names(mean)=row_names
  mean
}



#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
confint.blm <- function(object, level = 0.95, ...){
  a=c((1-level)/2, 1-(1-level)/2)

  variables = names(coefficients(object))
  m <- matrix(0, nrow=length(variables), ncol=2)
  colnames(m) <- c(paste(a[1]*100, "%"), paste(a[2]*100, "%"))
  rownames(m) = variables

  for(i in variables){
    m[i,1] = qnorm(a[1], mean=object$mean[i,1], sd=sqrt(object$covar[i,i]))
    m[i,2] = qnorm(a[2], mean=object$mean[i,1], sd=sqrt(object$covar[i,i]))

  }
  m
}


#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
deviance.blm= function(x,...){

  sum((x$formula[,1]-fitted(x))^2)

}

#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
predict.blm<- function(object, ...){


  responseless.formula <- delete.response(terms(object$formula))
  frame <- model.frame(responseless.formula, ...)
  phi <- model.matrix(responseless.formula, frame)


  m=object$mean
  #(t(m))
  means=vector(length=nrow(phi ))


  for (i in seq_along(means)){
    means[i]=t(m)%*%(phi[i,])
  }

  return(means)


}


#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export

fitted.blm= function(x,...){

  predict(x,...)
}


#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
plots.blm = function(x,...){
  variables = names(coefficients(x))

  plot(x$formula[,2],x$formula[,1],xlab=variables[2], ylab="y")
  abline(x$mean[1],x$mean[2], col="red")

}



#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
print.blm <- function(x,...){
  cat('\nCall:\n')
  print(x$func_call)

  cat('\nCoefficients:\n')
  print(x$mean)

}


#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
residuals.blm = function(x,...){

  residuals=(x$formula[,1]-fitted(x))
  residuals
}



#' Bayesian linear model.
#'
#' Fits a model, given as a formula, optionally with data provided through the "..." parameter.
#'
#' @param model   A formula describing the model.
#' @param ...     Additional data, for example a data frame. Feel free to add other options.
#'
#' @return A fitted model.
#' @export
summary.blm <- function(x, ...){
  cat('\nCall:\n')
  print(x$func_call)

  cat('\nCoefficients:\n')
  print(coefficients.blm(x))

  cat('\nResiduals:\n')
  print(residuals.blm(x))

  cat('\nDeviance:\n')
  print(deviance.blm(x))
}
