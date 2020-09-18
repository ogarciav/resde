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
  if (abs(lambda) < 6e-9) exp(y)
  else (1 + lambda*y)^(1/lambda)
}
