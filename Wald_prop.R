equal.prop <- function(theta1, n1, theta2, n2) {
  # Wald statistic
  k <- length(theta1)
  if (length(theta2) != k)
    stop("theta1 and theta2 must have the same length")
  #
  diff  <- theta1 - theta2
  aCov1 <- diag(theta1) - outer(theta1, theta1)
  aCov2 <- diag(theta2) - outer(theta2, theta2)
  aCov  <- aCov1 / n1 + aCov2 / n2
  s <- solve(aCov, diff)
  Wald <- sum(s * diff)
  #
  pval <- 1 - pchisq(Wald, df = k)

  ## output object
  z <- list(Wald = Wald, df = k, p.value = pval)
  z
}
