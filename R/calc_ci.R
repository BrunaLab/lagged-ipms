calc_ci <- function(x, alpha = 0.05) {
  est <- mean(x)
  ci <- quantile(x, c(0 + (alpha/2), 1 - (alpha/2)))
  tibble(estimate = est, lower = ci[1], upper = ci[2])
}
