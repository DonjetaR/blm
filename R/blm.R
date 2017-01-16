
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
#' @import stats
#' @return A fitted model.
#' @export
blm <- function(model, alpha,beta,...) {
  if (alpha<1 || beta<1){
    print("Invalid alpha or beta")
    break
  }


  prior <- make_prior(alpha, model)
  posterior <- update(model, prior, beta)


  structure(list(formula=model.frame(model),
                 prior=prior,
                 alpha=alpha,
                 beta=beta,
                 posterior=posterior,
                 mean=posterior$mean,
                 covar=posterior$Sigma,
                 func_call = sys.call()),
                 class="blm")
}





