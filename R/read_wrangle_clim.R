#' Read and wrangle data from Xavier et al. 2016
#'
#' The .csv file can be found in the data from Scott et al. 2022 where it has
#' already been wrangled from spatial .nc files.  This function calculates SPEI
#' and calculates monthly lags for each year.
#'
#' @param path file path
#' @param scale passed to `SPEI::spei()`
#' @param maxlag the number of months to lag SPEI by.  Should be 36 for this
#'   analysis.
#'
#' @return a tibble with two matrix columns: `spei_history`, containing the
#'   lagged SPEI values and `L`, a matrix indexing the lags.
#' 
#' @references 
#' Scott, Eric R., María Uriarte, and Emilio M. Bruna. “Delayed Effects of
#' Climate on Vital Rates Lead to Demographic Divergence in Amazonian Forest
#' Fragments.” Global Change Biology 28, no. 2 (2022): 463–79.
#' https://doi.org/10.1111/gcb.15900.
#'
#' Xavier, Alexandre C., Carey W. King, and Bridget R. Scanlon. “Daily Gridded
#' Meteorological Variables in Brazil (1980–2013).” International Journal of
#' Climatology 36, no. 6 (2016): 2644–59. https://doi.org/10.1002/joc.4518.
#' 
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