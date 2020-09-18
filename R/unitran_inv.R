#' Inverse of the unified transformation
#'
#' @describeIn unitran Inverse of unitran().
#'
#' @return \code{unitran_inv()}: Inverse of \code{unitran()},  \eqn{x = \varphi^{-1}(y)}{x = \varphi^-1(y)}.
#' @export
#'
#' @examples
#' curve(unitran_inv(y, "logistic"), xname="y", from=-4, to=4)
#' @usage unitran_inv(y, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
#' 
unitran_inv <- function(y, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
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
         no = bc_inv(-bc_inv(-y, beta), alpha),
         yes = 1 - bc_inv(-bc_inv(y, beta), alpha),
         stop("Bad 'reverse' value", call.=FALSE)
  )

}
