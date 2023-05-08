# File contents: userphi, userphiprime

#' Optional external transformation and derivative functions
#'
#' Templates for user-supplied transformation and drivative functions, used by
#'   \code{sdefit()} if specified in parameters \code{phi} and/or
#'   \code{phiprime}. To be completed by the user.
#'
#' @describeIn phi transformation
#'
#' @param x     Numeric vector, variable to be transformed. 
#' @param theta Named list of transformation parameters
#'
#' @return  Transformed variable
#'
#' @keywords internal
#' @examples
#' \dontrun{   userphi(20, list(a=70, c=0.5))   }

userphi <- function(x, theta){
  with(theta,
    (x/a)^c # substitute the code for your transformation here
  )
}

#' @describeIn phi derivative
#'
#' @return  Transformation derivative
#'
#' @keywords internal
#' @examples
#' \dontrun{   userphiprime(20, list(a=70, c=0.5))   }
#' 
userphiprime <- function(x, theta){
  with(theta,
    c*(x/a)^(c-1)/a # substitute the code for your derivative here
  )
}
