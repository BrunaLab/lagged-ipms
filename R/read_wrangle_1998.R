#' Read and wrangle data from 1998 experiment
#'
#' @param path path to dataset
#'
#' @return a tibble
#' 
read_wrangle_1998 <- function(path){
  df_raw <- read_csv(path)
  df_raw %>% 
    #confirmed with Emilio that NAs should be 0s
    mutate(ripe_fruits = frts_collected %>% replace_na(0),
           log_size_prev = log(shoots_jan * ht_jan))
}
