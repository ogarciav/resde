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
