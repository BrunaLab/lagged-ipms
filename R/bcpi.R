#' Bias-correction for bootstrap percentile intervals
#'
#' Method described in Caswell (2008) ch. 12. (Eqs 12.19--12.22).
#'
#' @param t0 the parameter estimate
#' @param t a vector of bootstrapped parameter estimates
#' @param alpha confidence level of the interval
#'
#' @return a tibble with the original estimate, `t0`, and the lower and upper
#'   CIs
#'
#' @references Caswell, Hal. Matrix Population Models: Construction, Analysis,
#'   and Interpretation. 2. ed., Sunderland, Mass: Sinauer Associates, 2008.

#' 
#'
#' @examples
#' tar_load(c(lambda_bt_det_ff, ipm_det_ff))
#' bcpi(t0 = lambda(ipm_det_ff), t = lambda_bt_det_ff)
bcpi <- function(t0, t, alpha = 0.05) {
  B <- length(t)
  z0 <- qnorm(sum(t < t0)/B)
  a1 <- pnorm(2 * z0 + qnorm(alpha/2))
  a2 <- pnorm(2 * z0 + qnorm(1 - alpha/2))
  c1 <- quantile(t, a1)
  c2 <- quantile(t, a2)
  return(tibble::tibble(est = t0, lower = c1, upper = c2))
}
