#' Display model specification
#'
#' @param model  SDE model specification, as produced by sdemodel()
#'
#' @return  Invisibly returns its argument
#' @export
#'
#' @examples
#'   mod <- sdemodel(); sdemodel_display(mod)
sdemodel_display <- function(model){
  m <- model$m; pars <- model$pars
  cat("Model:\n")
  vars <- all.vars(str2lang(m$phi))
  vars <- vars[vars != "x"]
  if(length(vars) > 0){
    vars <- paste(",", vars, collapse="")
  }
  cat("       y = phi(x", vars, ") = ", m$phi, sep="", fill=TRUE)
  cat("       y' = phiprime(x", vars, ") = ", m$phiprime, sep="", fill=TRUE)
  if(m$mup == "1"){
    cat("       dY = (", m$beta0, " + ", m$beta1, " * Y) dt + sigmap * dW",
        sep="", fill=TRUE)
  } else {
    cat("       dY = (", m$beta0, " + ", m$beta1, " * Y) dt + ", m$mup,
        " * sigmap * dW", sep="", fill=TRUE)
  }
  if(m$mu0 == "0"){
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ")",
        sep="", fill=TRUE)
  } else if(m$mu0 == "1"){
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ") + sigma0 * e0",
        sep="", fill=TRUE)
  } else {
    cat("       Y(", m$t0, ") = phi(", m$x0, vars, ") + ", m$mu0,
        " * sigma0 * e0", sep="", fill=TRUE)
  }
  if(m$mum == "0"){
    cat("       yi = Y(ti)", sep="", fill=TRUE)
  } else if(m$mum == "1"){
    cat("       yi = Y(ti) + sigmam * ei", sep="", fill=TRUE)
  } else {
    cat("       yi = Y(ti) + ", m$mum, " * sigmam * ei", sep="", fill=TRUE)
  }
  cat("Parameters:\n")
  cat("       ")
  cat(sort(pars), sep=", ", fill=TRUE)
  invisible(model)
}
