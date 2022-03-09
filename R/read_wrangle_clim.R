read_wrangle_clim <- function(path, scale = 3, maxlag = 36) {
  xa_raw <- read_csv(here("data", "xavier_daily_0.25x0.25.csv"))
  
  # Calculate SPEI
  xa_raw %>% 
    unite(latlon, lat, lon) %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    group_by(latlon, year, month) %>% 
    #convert to monthly
    summarize(precip = sum(precip), eto = sum(eto)) %>% 
    mutate(date = make_date(year = year, month = month, day = 15),
           .after = year) %>% 
    group_by(latlon) %>% 
    mutate(spei = as.numeric(
      spei(
        ts(precip - eto, freq = 12, start = c(year(min(date)), month(min(date)))),
        scale = scale
      )$fitted
    )) %>% 
    #make lags
    mutate(spei_history = tsModel::Lag(spei, 0:maxlag),
           L = matrix(0:maxlag, nrow = n(), ncol = maxlag+1, byrow = TRUE)) %>% 
    separate(latlon, into = c("lat", "lon"), sep = "_") %>% 
    ungroup()
  
}