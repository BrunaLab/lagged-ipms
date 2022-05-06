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
    #samples n_years years plus necessary "burn-in" years to calculate lags.  These will be removed later.
    if (is.null(iterations)) {
      iterations <- length(yrs)
    }
    year_seq <- sample(yrs, iterations + burnin, replace = TRUE)
  }
  
  # pull those random years from the dataset to construct a new dataset in the order of year_seq
  env_states <- 
    map_dfr(year_seq, ~ env_df %>% 
               dplyr::filter(year == .x)) %>% 
    # then calculate lagged `spei_history` starting in February of each year going back `maxlag` months
    mutate(spei_history = tsModel::Lag(spei, 0:maxlag)) %>% 
    dplyr::filter(month == 2) %>%
    slice(-c(1:burnin)) %>% 
    mutate(t = 1:n()) %>% #add row index
    #return just what's needed to iterate
    select(year, t, spei_history)
  
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
  
  crop_extremes <- function(x) {
    for(i in 1:ncol(x)) {
      x[,i][x[,i] < spei_min[i]] <- spei_min[i]
      x[,i][x[,i] > spei_max[i]] <- spei_max[i]
    }
    x
  }
  
  env_states %>% 
    mutate(spei_history = crop_extremes(spei_history))
  
}
# x <- make_env_states(clim, seed = 123, iterations = 1000)

