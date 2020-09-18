#' Derivative of the unified transformation
#'
#' @describeIn unitran Derivative of \code{unitran()} with respect to \code{x}.
#'
#' @return \code{unitran_prime()}: Derivative of \code{unitran()},  \eqn{y' = \varphi'(x)}.
#' @export
#'
#' @examples
#' curve(unitran_prime(x, "logistic"))
#' @usage unitran_prime(x, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
#' 
unitran_prime <- function(x, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
{
  if(!is.null(name)) switch(tolower(name),
    richards = {alpha=par; beta=0; reverse="no"},
    monomolecular=,
    mitscherlich = {alpha=1; beta=0; reverse="no"},
    bertalanffy = {alpha=1/3; beta=0; reverse="no"},
    gompertz = {alpha=0; beta=0; reverse="no"},
    logistic = {alpha=-1; beta=0; reverse="no"},
    levacovic = {alpha=par; beta=-0.5; reverse="no"},
    weibull = {alpha=0; beta=par; reverse="yes"},
    korf = {alpha=0; beta=-par; reverse="no"},
    exponential = {alpha=0; beta=1; reverse="yes"},
    schumacher = {alpha=0; beta=-1; reverse="no"},
    hosfeld = {alpha=-1; beta=par; reverse="auto"},
    stop("Unknown name", call.=FALSE)
  )
  if(identical(reverse, "auto")){
      if(beta > 0) reverse <- "yes"
           else reverse <- "no"
  }
  switch(reverse,
         no = (-bc(x, alpha))^(beta-1) * x^(alpha-1),
         yes = (-bc(1 - x, alpha))^(beta-1) * (1-x)^(alpha-1),
         stop("Bad 'reverse' value", call.=FALSE)
  )

}
