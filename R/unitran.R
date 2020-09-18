#' Unified transformation
#'
#' Calculates a variable transformation that produces various growth curve models, depending on the
#' values of two shape parameters, \code{alpha} and \code{beta}. Models can also
#' be specified by name.
#'
#' @param x,y Variable to be transformed, \code{x} must be between 0 and 1.
#' @param name Optional model name, case-insensitive, in quotes. One of \code{Richards},
#'    \code{monomolecular}, \code{Mitscherlich}, \code{Bertalanffy}, \code{Gompertz},
#'    \code{logistic}, \code{Levacovic}, \code{Weibull}, \code{Korf}, \code{exponential},
#'    \code{Schumacher}, \code{Hosfeld}.
#' @param par Model parameter, if needed and model name supplied.
#' @param alpha,beta Shape parameters, if the model is not specified by name.
#' @param reverse Reverse \code{x} and \code{t} axes? One of \code{"yes"}, \code{"no"}, \code{"auto"}.
#'   With \code{"auto"}, axes are reversed as necessary for an upper asymptote.
#'   (i.e., if \code{beta} > 0).
#'
#' @return \code{unitran()}: Transformed \code{x}, \eqn{y = \varphi(x)}.
#' @export
#' @describeIn unitran Unified transformation.
#'
#' @examples
#' curve(unitran(x, "Gompertz"))  # same as unitran(x, alpha=0, beta=0)
#' @usage unitran(x, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
#' 
unitran <- function(x, name=NULL, par=NULL, alpha=NULL, beta=NULL, reverse="auto")
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
         no = -bc(-bc(x, alpha), beta),
         yes = bc(-bc(1 - x, alpha), beta),
         stop("Bad 'reverse' value", call.=FALSE)
  )

}
