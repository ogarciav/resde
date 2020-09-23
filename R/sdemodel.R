# File contents: sdemodel, sdemodel_display


#' Model specification
#'
#' Specify transformation and re-parametrizations for reducible SDE model.
#'
#' @param phi       Transformation formula \eqn{y = \varphi(x, parameters)}.
#' @param phiprime  Optional formula for derivative of \code{phi}.
#' @param beta0,beta1     Optional formulas or constants, possibly giving a re-parameterization,.
#' @param t0,x0        Formulas or constants for the initial condition.
#' @param mu0       Formula or constant for the initial condition \eqn{\sigma_0}{\sigma0} multiplier.
#' @param mup,mum       Formulas or constants for the process and measurement \eqn{\sigma} multipliers.
#'
#' @return  List with model specification, to be used by \code{\link{sdefit}()}.
#' @export
#'
#' @examples
#'    richards <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b, mum=0)
#'
#' @usage sdemodel(phi=~x, phiprime=NULL, beta0=~beta0, beta1=~beta1,
#'    t0=0, x0=0, mu0=0, mup=1, mum=1)

sdemodel <- function(phi=~x, phiprime=NULL, beta0=~beta0, beta1=~beta1,
                     t0=0, x0=0, mu0=0, mup=1, mum=1)
{
  # Checks
  if(methods::is(t0, "formula") && methods::is(x0, "formula")){
    stop("At least one of t0 and x0 must be known.")
  }
  vars <- all.vars(phi)
  if(sum(vars != "x") != length(vars) - 1){
    stop("No 'x' or some other problem in the transformation ", phi)
  }
  if(is.null(phiprime)){
    phiprime <- Deriv::Deriv(phi, "x")
  }
  # Parameters
  m <- list(beta0=beta0, beta1=beta1, t0=t0, x0=x0, mu0=mu0, mup=mup, mum=mum)
  pars <- Reduce(union, lapply(m, all.vars))  # parameters, excluding phi
  lambda <- vars[vars != "x"]  # parameters in phi
  pars <- sort(union(pars, lambda))  # all the pameters
  # List of strings
  m <- list(beta0=beta0, beta1=beta1, t0=t0, x0=x0, mu0=mu0, mup=mup,
            mum=mum, phi=phi, phiprime=phiprime)
  m <- lapply(m, function(x) gsub("~", "", deparse1(x, collapse=";"))) # strings
  m <- list(m=m, pars=pars, lambda=lambda) # output to be passed to sdefit()
  sdemodel_display(m)
  return(m)
}


#' Display model specification
#'
#' @param model  SDE model specification, as produced by sdemodel()
#'
#' @return  Invisibly returns its argument
#' @export
#'
#' @examples
#'   mod <- sdemodel(); sdemodel_display(mod)
sdemodel_display <- function(model){
  m <- model$m; pars <- model$pars
  cat("Model:\n")
  vars <- all.vars(str2lang(m$phi))
  vars <- vars[vars != "x"]
  if(length(vars) > 0){
    vars <- paste(",", vars, collapse="")
  }
  cat("       y = phi(x", vars, ") = ", m$phi, sep="", fill=TRUE)
  cat("       y' = phiprime(x", vars, ") = ", m$phiprime, sep="", fill=TRUE)
  if(m$mup == "1"){
    cat("       dY = (", m$beta0, " + ", m$beta1, " * Y) dt + sigmap * dW",
        sep="", fill=TRUE)
  } else {
    cat("       dY = (", m$beta0, " + ", m$beta1, " * Y) dt + ", m$mup,
        " * sigmap * dW", sep="", fill=TRUE)
  }
  if(m$mu0 == "0"){
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ")",
        sep="", fill=TRUE)
  } else if(m$mu0 == "1"){
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ") + sigma0 * e0",
        sep="", fill=TRUE)
  } else {
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ") + ", m$mu0,
        " * sigma0 * e0", sep="", fill=TRUE)
  }
  if(m$mum == "0"){
    cat("       yi = Y(ti)", sep="", fill=TRUE)
  } else if(m$mum == "1"){
    cat("       yi = Y(ti) + sigmam * ei", sep="", fill=TRUE)
  } else {
    cat("       yi = Y(ti) + ", m$mum, " * sigmam * ei", sep="", fill=TRUE)
  }
  cat("Parameters:\n")
  cat("       ")
  cat(sort(pars), sep=", ", fill=TRUE)
  invisible(model)
}
