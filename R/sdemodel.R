#' Model specification
#'
#' Specify transformation and re-parametrizations for reducible SDE model.
#'
#' @param phi       Transformation formula \eqn{y = \varphi(x, parameters)}.
#' @param phiprime  Optional formula for derivative of \code{phi}.
#' @param beta0,beta1     Optional formulas or constants, possibly giving a re-parameterization,.
#' @param t0,x0        Formulas or constants for the initial condition.
#' @param mu0       Formula or constant for the initial condition \eq{\sigma_0}{\sigma0} multiplier.
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
