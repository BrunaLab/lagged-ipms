lag_clim <- function(clim, maxlag = 36) {
  clim %>% 
    group_by(latlon) %>% 
    mutate(spei_history = tsModel::Lag(spei, 0:maxlag),
           L = matrix(0:maxlag, nrow = n(), ncol = maxlag+1, byrow = TRUE)) %>% 
    separate(latlon, into = c("lat", "lon"), sep = "_") %>% 
    #clean up 
    select(-month) %>% 
    ungroup()
}