get_scat_params <- function(model) {
  thetas <- model$family$getTheta()
  
  params <- c(exp(thetas[1]) + 3, exp(thetas[2]))
  names(params) <- c('nu', 'sd')
  params
}

