CI.prop <- function(p, n, level = 0.95, method = "asymp") {
  # confidence intervals for multinomial proportions
  k <- length(p)
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  qz <- qnorm(a)
  qc <- qchisq(level, df = k - 1)
  a <- 1 - level
  qb <- qchisq(1 - a / k, df = 1) # Bonferroni correction
  switch(method,
         "asymp" = { # asymptotic interval (default)
           SE <- sqrt(p * (1 - p) / n)
           ci <- p + SE %o% qz
         },
         "QH" = { # Quesenberg and Hurst (1964)
           m <- n * p
           lh <- qc + 2 * m
           SE <- sqrt(qc + 4 * m * (n - m) / n)
           ci <- lh + SE %o% qz
           ci <- ci / (2 * n + qc)
         },
         "sqrt" = { # square root transformation (Bailey, 1980)
           m <- n * p
           y <- sqrt((m + 0.375) / (n + 0.125))
           b <- 0.25 * qb / n
           SE <- 0.5 * sqrt((b + 1 - y^2) / n)
           qb <- c(-sqrt(qb), sqrt(qb))
           ci <- y + SE %o% qb
           ci <- (ci / (b + 1))^2
         })
  ci
}
