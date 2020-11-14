# File contents: bc, bc_inv, bc_prime


#' Box-Cox transformation
#'
#' @description These functions calculate the Box-Cox transformation, its inverse, and derivative.
#' 
#' @describeIn bc  The Box-Cox transformation
#' 
#' @param x,y Numeric vector (\code{x} must be >= 0).
#' @param lambda Numeric scalar, power parameter.
#'
#' @return \code{bc()}: Returns the transform value(s).
#' @export
#'
#' @details \code{bc()} uses \code{expm1()}, wich is more accurate
#'  for small \code{lambda} than a more "obvious" alternative like
#'  \preformatted{  if (abs(lambda) < 6e-9) log(y)
#'  else (y^lambda - 1) / lambda} The difference might be important
#'  in optimization applications. See example below. Similarly,
#'  \code{bc_inv()} uses \code{log1p()}.
#'    
#' @examples
#' bc(0.5, 1.5)
#' bc(1, 0)
#' obvious <- function(lambda){(0.6^lambda - 1) / lambda} # at y = 0.6
#' plot(obvious, xlab="lambda", xlim=c(1e-6, 1e-9), log="x")
#' 
bc <- function(x, lambda){
  stopifnot(length(lambda) == 1)
  if (abs(lambda) < 1e-300) log(x)
  else (expm1(lambda * log(x))) / lambda
}


#' Inverse of the Box-Cox transformation
#' 
#' @describeIn bc  Inverse of the Box-Cox transformation
#' @return \code{bc_inv()}: Computes the inverse of \code{bc()}. 
#' @export
#' @examples
#' bc_inv(-0.4, 1.5)
#' bc_inv(0, 0)
#' 
bc_inv <- function(y, lambda){
  stopifnot(length(lambda) == 1)
  if (abs(lambda) < 1e-300) exp(y)
  else exp(log1p(lambda * y) / lambda)
}


#' Derivative of the Box-Cox transformation
#' 
#' @describeIn bc  Derivative of the Box-Cox transformation
#' @return \code{bc_prime()}: Gives the derivative of \code{bc()}
#'   with respect to \code{y}.
#' @export
#' @examples
#' bc_prime(0.5, 1.5)
#' bc_prime(1, 0)
#' 
bc_prime <- function(y, lambda){
  y^(lambda - 1)
}
