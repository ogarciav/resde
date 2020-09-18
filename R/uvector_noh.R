#' ML estimation vector for reducible SDEs
#'
#' @describeIn uvector Estimation vector, non-hierarchical
#' @order 2

uvector_noh <- function(x, t, beta0, beta1, eta, eta0, x0, t0, lambda, final = FALSE)
{
  theta <- c(list(beta0 = beta0, beta1 = beta1, eta = eta, eta0 = eta0,
                  x0 = x0, t0 = t0), lambda)
  s <- order(t); t <- t[s]; x <- x[s] # ensure increasing t
  n <- length(x)
  y <- phi(x, theta)
  y0 <- phi(x0, theta)
  Dt <- diff(c(t0, t))
  if (beta1 != 0) {
    ex <- exp(beta1 * Dt)
    ex2 <- ex ^ 2
    z <- y + beta0 / beta1 - ex * (c(y0, y[-n]) + beta0 / beta1)
    cdiag <- ex2 * eta + eta + (1 - eta) * (ex2 - 1) / (2 * beta1)
    cdiag[1] <- cdiag[1] - ex2[1] * (eta - eta0)
    csub <- -ex * eta
  } else { # beta1 == 0
    z <- y - c(y0, y[-n]) - beta0 * Dt
    cdiag <- 2 * eta + (1 - eta) * Dt
    cdiag[1] <- cdiag[1] - eta + eta0
    csub <- -rep(eta, n)
  }
  ld.v <- logdet.and.v(cdiag, csub, z)
  logJ <- sum(log(abs(phiprime(x, theta)))) - ld.v$logdet
  Jn <- exp(logJ / n)
  # J^(1/n)
  u <- ld.v$v / Jn
  if (!final) return (u) # "normal" exit
  # Else, at optimum, calculate sigma.p, sigma.m and sigma.0
  #   estimates, and the log-likelihood:
  ms <- sum(u ^ 2) / n # mean square
  sigma2 <- Jn ^ 2 * ms # estimate for sigma^2
  list(sigma.p = sqrt((1 - eta) * sigma2),
       sigma.m = sqrt(eta * sigma2),
       sigma.0 = sqrt(eta0 * sigma2),
       loglikelihood = -(n / 2) * (log(ms) + log(2 * pi) + 1))
}
