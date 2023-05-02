# File contents: userphi, userphiprime

#' Optional external transformation and derivative functions
#'
#' Skeletons for user-supplied transformation and drivative functions, used by
#'   \code{sdefit()} if specified in parameters \code{phi} and/or
#'   \code{phiprime}. To be completed by the user.
#'
#' @describeIn phi Transformation
#'
#' @param x     Numeric vector, variable to be transformed. 
#' @param theta Named list of transformation parameters
#'
#' @return:  Transformed variable
#'
#' @keywords internal
#' @examples
#' \dontrun{   userphi(20, list(a=70, c=0.5))   }

userphi <- function(x, theta){
  # Insert code for the transformation here
}

#' @describeIn phi Derivative
#'
#' @return:  Transformation derivative
#'
#' @keywords internal
#' @examples
#' \dontrun{   userphiprime(20, list(a=70, c=0.5))   }
userphiprime <- function(x, theta){
  # Insert code for the derivative here
}
