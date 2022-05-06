#' Add lagged SPEI history and iterate a DLNM-based IPM
#' 
#' Takes a `proto_ipm` object and the `clim` dataset, calculates lagged SPEI
#' history, adds those environmental states to the `proto_ipm`, then iterates
#' it. Takes *either* a specific sequence of years found in the `clim` dataset
#' (`year_seq`) *or* a number of `iterations`.
#'
#' @param proto_ipm a proto_ipm built using DLNM vital rates models
#' @param clim the `clim` target climate dataset
#' @param year_seq a vector of years passed to `make_env_states()`
#' @param seed random seed passed to `make_env_states()`
#' @param iterations number of iterations passed to `make_env_states()`
#' @param ... additional arguments passed to `ipmr::make_ipm()`
#'
#' @details When `year_seq` is specified, that exact sequence of years is drawn
#'   from the climate dataset.  However, because lags are calculated, there is
#'   incomplete data for the first few years.  Therefore, a number of years are
#'   added to the beginning of the sequence so that lagged SPEI can be
#'   calculated, but then trimmed from the final dataset so that the total
#'   number of iterations is the same as the length of `year_seq`.  These few
#'   "burn-in" years are chosen randomly, and when `year_seq` is specified, the
#'   behavior of `seed` is to make these "burn-in" years reproducible.  In the
#'   case when `year_seq` is not specified, years are chosen at random
#'   (reproducibly if `seed` is set), with a "burn-in" so the total number of
#'   environmental states is `iterations`.
#'   
#' @return an iterated ipm
#' 
make_dlnm_ipm <- function(proto_ipm, clim, year_seq = NULL, seed = NULL, iterations = NULL, ...) {
  
  if(is.null(year_seq) & is.null(iterations)) {
    stop("One of `year_seq` or `iterations` must be specified")
  }
  
  if(!is.null(year_seq) & !is.null(iterations)) {
    stop("Only `year_seq` or `iterations` can be specified, not both")
  }
  
  # if iterations are specified, draw random years with make_env_states()
  env_states <- make_env_states(clim, seed = seed, maxlag = 36, iterations = iterations, year_seq = year_seq)
  n_it <- nrow(env_states) #should be the same as iterations, but just in case.
  
  # if year_seq is specified, use that instead.
  
  #simple function for sampling from the env_states to pass to define_env_state()
  sample_env <- function(env_states, iteration) {
    as.list(env_states[iteration, ])
  }
  
  #define environmental states and iterate IPM
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