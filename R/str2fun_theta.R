#' String to function, with parameters in theta
#'
#' Normally not called by the user directly, used by \code{\link{sdefit}()}.
#'   Converts an expression, in a character string, to a function. 
#'
#' @param s String representation of a function of \code{x} and parameters
#'
#' @return Function of \code{x} and \code{theta}, \code{theta} being a named vector or list of parameters.
#' @export
#'
#' @examples str2fun_theta("x^c / a")

str2fun_theta <- function(s){
  t <- paste("alist(x=, theta=, with(theta, ", s, "))")
  return(as.function(eval(str2lang(t))))
}
