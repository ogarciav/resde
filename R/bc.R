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
#' @examples
#' bc(0.5, 1.5)
#' bc(1, 0)
#' 
bc <- function(y, lambda){
  stopifnot(length(lambda) == 1)
  if (abs(lambda) < 6e-9) log(y)
  else (y^lambda - 1) / lambda
}
