#' Create a data frame of environmental states for use in stochastic IPMs
#'
#' Takes the `clim` target and either a specific sequence of years (`year_seq`)
#' *or* a number of `iterations` and produces a tibble with everything required
#' to be used for the stochastic, parameter sampled IPMs (the DLNM IPMs).  Used
#' by `make_dlnm_ipm()`.
#'
#' @param clim the `clim` target
#' @param seed a random seed for reproducibility, optional.
#' @param maxlag the maximum lag time.  Should be 36 months.
#' @param iterations a number of iterations if you want years chosen randomly
#' @param year_seq a vector of years to use to sample from `clim`
#'
#' @return a tibble with columns `t` (iteration number, used internally by
#'   `ipmr`), and `spei_history` (a matrix column with 36 columns)
#' 
make_env_states <- function(clim, seed = NULL, maxlag = 36, iterations = NULL, year_seq = NULL) {
  env_df <- 
    clim %>% 
    #remove leading NAs and any Inf or -Inf values that might exist in spei
    dplyr::filter(is.finite(spei)) %>% 
    #average spatially
    group_by(year, month) %>% 
    summarise(spei = mean(spei), .groups = "drop")
  
  burnin <- ceiling(maxlag/12)
  
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1) #just to set a random seed if there isn't one yet
  }
  if (is.null(seed)) 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  
  #pick random years
  yrs <- unique(env_df$year)
  
  if(!is.null(year_seq)) {
  #check that years in year_seq are in env_df
    if(any(!unique(year_seq) %in% yrs)) {
      stop("Not all values of year_seq are in the clim dataset")
    }
    iterations <- length(year_seq)
    #add burnin years to the beginning
    year_seq <- c(sample(yrs, burnin, replace = TRUE), year_seq)
  } else {
    #samples n_years years plus necessary "burn-in" years to calculate lags.
    #These will be removed later.
    if (is.null(iterations)) {
      iterations <- length(yrs)
    }
    year_seq <- sample(yrs, iterations + burnin, replace = TRUE)
  }
  
  # pull those random years from the dataset to construct a new dataset in the
  # order of year_seq
  env_states <- 
    map_dfr(year_seq, ~ env_df %>% 
               dplyr::filter(year == .x)) %>% 
    # then calculate lagged `spei_history` starting in February of each year
    # going back `maxlag` months
    mutate(spei_history = tsModel::Lag(spei, 0:maxlag)) %>% 
    dplyr::filter(month == 2) %>%
    slice(-c(1:burnin)) %>% 
    mutate(t = 1:n()) %>% #add row index
    #return just what's needed to iterate
    select(t, spei_history)
  
  #limit SPEI in randomly sampled data to limits in observed data. Even though
  #its the same data, there might not have been observed data as extreme for
  #particular lag times.
  spei_max <- clim %>% 
    dplyr::filter(month == 2, year %in% 1998:2009) %>% 
    pull(spei_history) %>% 
    apply(MARGIN = 2, FUN = max, na.rm = TRUE)
  
  spei_min <- clim %>% 
    dplyr::filter(month == 2, year %in% 1998:2009) %>% 
    pull(spei_history) %>% 
    apply(MARGIN = 2, FUN = min, na.rm = TRUE)
  
  #crop extremes
  for (i in 1:ncol(env_states$spei_history)) {
    env_states$spei_history[, i][env_states$spei_history[, i] < spei_min[i]] <-
      spei_min[i]
    env_states$spei_history[, i][env_states$spei_history[, i] > spei_max[i]] <-
      spei_max[i]
  }
  
  #return:
  env_states
}
# x <- make_env_states(clim, seed = 123, iterations = 1000)

