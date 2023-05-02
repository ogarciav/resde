#' \pkg{resde} - Parameter estimation in reducible SDE models.
#'  
#' @description
#' The main functions for model fitting are \code{\link{sdemodel}()}
#' and \code{\link{sdefit}()}. First, specify the model structure in
#' \code{sdemodel()}, including the variable transformation, any
#' re-parameterizations, initial condition, and the presence or not of process,
#' measurement, and initial condition noise. Then, fit the model with
#' \code{sdefit()}, indicating the data to be used, and starting parameter
#' values for the iterations. For hierarchical models, one must also
#' indicate which are the global and local parameters, and if fixed
#' locals or a mixed effects method should be used.
#'  
#' Some auxilliary functions include the Box-Cox transformation \code{\link{bc}()},
#' and the \emph{unified transformation} \code{\link{unitran}()}.
#' 
#' For detailed usage see the vignette.
#' 
#' @docType package
#' @name resde
#' @examples
#' # Richards model  dH^c = b(a^c - H^c) dt + s dW for tree heights
#' tree1 <- subset(Loblolly, Seed == Seed[1]) # first tree
#' m <- sdemodel(~x^c, beta0=~b*a^c, beta1=~-b, mum=0) # no measurement error
#' sdefit(m, x="height", t="age", data=tree1, start=c(a=70, b=0.1, c=0.5))
#' 
#' @references Garcia, O. (2019) "Estimating reducible stochastic differential
#'  equations by conversion to a least-squares problem". \emph{Computational
#'  Statistics 34}(1), 23-46. \doi{10.1007/s00180-018-0837-4}
NULL
