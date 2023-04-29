# File contents: phi, phiprime

#' External transformation and derivative functions
#'
#' Dummy external transformation definitions, called by \code{sdefit()}
#'   if \code{trfuns == TRUE}. To be completed by the user.
#'
#' @describeIn phi Transformation
#'
#' @param x     Numeric vector, variable to be transformed. 
#' @param theta Named list of transformation parameters
#'
#' @return \code{phi}:  Transformed variable
#'
#' @keywords internal
#' @examples
#' \dontrun{   phi(20, list(a=70, c=0.5))   }

phi <- function(x, theta){
  f <- get0("phi", .GlobalEnv, mode="function", inherits=FALSE)
  if(is.null(f))
    stop("phi() not found", call. = FALSE)
  f(x, theta)
}

#' @describeIn phi Derivative
#'
#' @return \code{phiprime}:  Transformation derivative
#'
#' @keywords internal
#' @examples
#' \dontrun{   phiprime(20, list(a=70, c=0.5))   }
phiprime <- function(x, theta){
  f <- get0("phiprime", .GlobalEnv, mode="function", inherits=FALSE)
  if(is.null(f))
    stop("phiprime() not found", call. = FALSE)
  f(x, theta)
}
