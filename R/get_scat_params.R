#' Retrieve parameters for scaled t distribution family GAMs
#' 
#' A convenience function to extract nu and sd from GAMs with 
#' `family = scat`.  These parameters can be plugged into the `_t.scaled()`
#' functions from the `metRology` package.
#'
#' @param model 
#'
#' @return a named vector
#' 
get_scat_params <- function(model) {
  thetas <- model$family$getTheta()
  
  params <- c(exp(thetas[1]) + 3, exp(thetas[2]))
  names(params) <- c('nu', 'sd')
  params
}

