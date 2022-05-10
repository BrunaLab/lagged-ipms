#' Simple deterministic models for Heliconia vital rates
#' 
#' These functions fit models for survival, growth, flowering, seedling
#' survival, and seedling recruitment using GAMs.  I've erred on the side of too
#' many knots so that models have enough knots for both CF and FF data and I
#' don't have to write separate functions.


#for checking knots, load data and run k.check() for the models
# library(mgcv)
# library(tidyverse)
# tar_load(data_ff)
# tar_load(data_cf)

surv_det <- function(data) {

  df <- data %>%
    filter(sdlg_prev == FALSE)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 20),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_det(data_ff))
# k.check(surv_det(data_cf))

size_det <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == FALSE) %>% 
    #only plants that survived get to grow
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ s(log_size_prev, bs = "cr", k = 20),
    family = scat,
    data = df,
    method = "fREML"
  )
}
# k.check(size_det(data_ff))
# k.check(size_det(data_cf))

flwr_det <- function(data) {
  df <- data %>% 
    dplyr::filter(sdlg == FALSE) %>% 
    #current seedlings are excluded, but not plants that were seedlings in the previous year
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    flwr ~ s(log_size_prev, bs = "cr", k = 20),
    family = binomial, 
    data = df,
    method = "fREML"
  )
}
# k.check(flwr_det(data_ff))
# k.check(flwr_det(data_cf))

size_sdlg_det <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == TRUE) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  gam(
    log_size ~ 1,
    family = scat,
    data = df,
    method = "REML"
  )
}

surv_sdlg_det <- function(data) {
  
  df <- data %>%
    filter(sdlg_prev == TRUE)
  
  gam(
    surv ~ 1,
    family = binomial,
    data = df,
    method = "REML"
  )
}
