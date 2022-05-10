#' Models for Heliconia vital rates including environmental stochasticity as a
#' random effect of year
#'
#' These functions fit models for survival, growth, flowering, seedling
#' survival, and seedling recruitment using GAMs.  I've erred on the side of too
#' many knots so that models have enough knots for both CF and FF data and I
#' don't have to write separate functions. The random effect of year can be
#' modeled several ways with GAMs.  These are explored in
#' notes/environmental-stochasticity.Rmd


#for checking knots, load data and run k.check() for the models
# library(mgcv)
# library(tidyverse)
# tar_load(data_ff)
# tar_load(data_cf)

surv_raneff <- function(data) {
  
  df <- data %>%
    filter(sdlg_prev == FALSE)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 20, m = 2) +
      s(log_size_prev, year_fct, bs = "fs", m = 2),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_raneff(data_ff))
# k.check(surv_raneff(data_cf))


size_raneff <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == FALSE) %>% 
    #only plants that survived get to grow
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ s(log_size_prev, bs = "cr", k = 20, m = 2) +
      s(log_size_prev, year_fct, bs = "fs", m = 2),
    family = scat,
    data = df,
    method = "fREML"
  )
}
# k.check(size_raneff(data_ff))#no diff between k = 20 and k = 25
# k.check(size_raneff(data_cf)) 


flwr_raneff <- function(data) {
  
  df <- data %>% 
    dplyr::filter(sdlg == FALSE) %>% 
    #current seedlings are excluded, but not plants that were seedlings in the previous year
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    flwr ~ s(log_size_prev, bs = "cr", k = 20, m = 2) +
      s(log_size_prev, year_fct, bs = "fs", m = 2),
    family = binomial, 
    data = df,
    method = "fREML"
  )
}
# k.check(flwr_raneff(data_ff))
# k.check(flwr_raneff(data_cf))

size_sdlg_raneff <- function(data) {
  
  df <- data %>%
    dplyr::filter(sdlg_prev == TRUE) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  gam(
    log_size ~ 1 + s(year_fct, bs = "re"),
    family = scat,
    data = df,
    method = "REML"
  )
}

surv_sdlg_raneff <- function(data) {
  
  df <- data %>%
    filter(sdlg_prev == TRUE)
  
  gam(
    surv ~ 1 + s(year_fct, bs = "re"),
    family = binomial,
    data = df,
    method = "REML"
  )
}

