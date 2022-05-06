# library(targets)
# library(ipmr)
# library(dplyr)
# tar_load(data_full)

#' Make starting population vector for IPMs
#'
#' @param data the data_full target
#' @param n_mesh number of meshpoints to use when discretizing log_size.  Passed to `ipmr::discretize_pop_vector()`.
#' @param n total individuals. The sum of `n_log_size` and `n_sdlg` will equal this.
#'
#' @return a list of two elements, `n_log_size` (length = n_mesh) and `n_sdlg` (length 1)
#' 
make_pop_vec <- function(data, n_mesh = 100, n = 1) {
  
  #filter data to only include years with seedlings
  pop_data <- data %>% filter(year>1999, !is.na(log_size))
  
  #established plants structured by log_size
  log_size <- pop_data %>% filter(!sdlg) %>% pull(log_size)
  n_established <- length(log_size)
  
  #use ipmr function to discretize log_size.  No padding because we are assuming we've captured the entire range of plant sizes
  established_vec <- ipmr::discretize_pop_vector(log_size, n_mesh = n_mesh, pad_low = 1, pad_high = 1)
  
  #count seedlings
  n_sdlg <- pop_data %>% filter(sdlg) %>% nrow()
  
  #total observations
  n_total <- n_established + n_sdlg
  
  # sum(established_vec$n_log_size * n_established) == n_established
  
  prop_established <- (established_vec$n_log_size * n_established) / n_total
  prop_sdlg <- n_sdlg / n_total
  
  # sum(prop_established) + prop_sdlg == 1
  
  #return:
  list(n_log_size = prop_established * n, n_sdlg = prop_sdlg * n)
}