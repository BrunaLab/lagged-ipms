#' Distributed lag non-linear models for Heliconia vital rates
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

surv_dlnm <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == FALSE)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 10) +
      te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_dlnm(data_ff))
# k.check(surv_dlnm(data_cf))


size_dlnm <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == FALSE) %>% 
    #only plants that survived get to grow
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ s(log_size_prev, bs = "cr", k = 25) +
      te(spei_history, L, bs = "cr", k = c(5,15)),
    family = scat,
    data = df,
    method = "fREML"
  )
}
# k.check(size_dlnm(data_ff))
# k.check(size_dlnm(data_cf))


flwr_dlnm <- function(data) {
  df <- data %>% 
    dplyr::filter(sdlg == FALSE) %>% 
    #current seedlings are excluded, but not plants that were seedlings in the previous year
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    flwr ~ s(log_size_prev, bs = "cr", k = 15) +
      te(spei_history, L, bs = "cr", k = c(15, 18)),
    family = binomial, 
    data = df,
    method = "fREML"
  )
}
# k.check(flwr_dlnm(data_ff))
# k.check(flwr_dlnm(data_cf))

size_sdlg_dlnm <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == TRUE) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  gam(
    log_size ~ 1 + te(spei_history, L, bs = "cr", k = c(10, 15)),
    family = scat,
    data = df,
    method = "REML"
  )
}
#TODO: double-check k for seedling size and survival
# k.check(size_sdlg_dlnm(data_ff))

surv_sdlg_dlnm <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == TRUE)
  
  gam(
    surv ~ 1 + te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "REML"
  )
}
# k.check(surv_sdlg_dlnm(data_ff))
# k.check(surv_sdlg_dlnm(data_cf))

