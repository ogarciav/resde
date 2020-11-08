# File contents: bc, bc_inv, bc_prime


#' Box-Cox transformation
#'
#' @description These functions calculate the Box-Cox transformation, its inverse, and derivative.
#' 
#' @describeIn bc  The Box-Cox transformation
#' 
#' @param y Numeric vector (must be >= 0).
#' @param lambda Numeric scalar, power parameter.
#'
#' @return \code{bc()}: Returns the transform value(s).
#' @export
#'
#' @details Using \code{expm1()} is more accurate than a more "obvious"
#'    alternative like \preformatted{
#'      if (abs(lambda) < 6e-9) log(y)
#'      else (y^lambda - 1) / lambda} The difference might be important in
#'    optimization applications.
#'    
#' @examples
#' bc(0.5, 1.5)
#' bc(1, 0)
#' 
bc <- function(y, lambda){
  stopifnot(length(lambda) == 1)
  if (abs(lambda) < 1e-300) log(y)
  else (expm1(lambda * log(y))) / lambda
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
