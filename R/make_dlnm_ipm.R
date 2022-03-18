make_dlnm_ipm <- function(proto_ipm, clim, seed, iterations, ...) {
  
  sample_env <- function(env_states, iteration) {
    as.list(env_states[iteration, ])
  }
  
  env_states <- make_env_states(clim, seed = seed, maxlag = 36, iterations = iterations)
  n_it <- nrow(env_states) #should be the same as iterations, but just in case, since 
  
  proto_ipm %>% 
    define_env_state(
      env_param = sample_env(env_states,
                             iteration = t),
      data_list = list(
        env_states = env_states,
        sample_env = sample_env
      )
    ) %>% 
    make_ipm(iterations = n_it, ...)
}