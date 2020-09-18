#' Log of determinant and v vector
#'
#' @describeIn uvector Logarithm of determinant, and \eqn{v} vector
#' @order 3

logdet.and.v <- function(cdiag, csub=NULL, z)
{
  if(is.null(csub) || all(cdiag == 0)){ # diagonal
    return(list(logdet = sum(log(cdiag))/2, v = z / sqrt(cdiag)))
  }
  v <- z
  ldiag <- sqrt(cdiag[1])
  logdet <- log(ldiag)
  v[1] <- z[1] / ldiag
  for (i in 2:length(z)) {
    lsub <- csub[i] / ldiag
    ldiag <- sqrt(cdiag[i] - lsub ^ 2)
    logdet <- logdet + log(ldiag)
    v[i] <- (z[i] - lsub * v[i - 1]) / ldiag
  }
  list(logdet = logdet, v = v)
}
