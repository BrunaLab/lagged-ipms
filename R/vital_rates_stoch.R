#for checking knots, load data and run k.check() for the models
# library(mgcv)
# library(tidyverse)
# data <- tar_read(data_full)

surv_raneff <- function(data, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == FALSE, habitat == habitat_choice)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 20) +
      s(year_fct, bs = "re"),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_raneff(data))
# k.check(surv_raneff(data, habitat = "1-ha"))


size_raneff <- function(data, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    dplyr::filter(sdlg_prev == FALSE, habitat == habitat_choice) %>% 
    #only plants that survived get to grow
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ s(log_size_prev, bs = "cr", k = 20) +
      s(year_fct, bs = "re"),
    family = scat,
    data = df,
    method = "fREML"
  )
}
# k.check(size_raneff(data))
# k.check(size_raneff(data, habitat = "1-ha")) #no diff between k = 20 and k = 25


flwr_raneff <- function(data, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>% 
    dplyr::filter(habitat == habitat_choice, sdlg == FALSE) %>% 
    #current seedlings are excluded, but not plants that were seedlings in the previous year
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    flwr ~ s(log_size_prev, bs = "cr", k = 20) +
      s(year_fct, bs = "re"),
    family = binomial, 
    data = df,
    method = "fREML"
  )
}
# k.check(flwr_raneff(data))
# k.check(flwr_raneff(data, habitat = "1-ha"))

size_sdlg_raneff <- function(data, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  sdlg_choice <- sdlg
  
  df <- data %>%
    dplyr::filter(sdlg_prev == TRUE, habitat == habitat_choice) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  gam(
    log_size ~ 1 + s(year_fct, bs = "re"),
    family = scat,
    data = df,
    method = "REML"
  )
}

surv_sdlg_raneff <- function(data, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == TRUE, habitat == habitat_choice)
  
  gam(
    surv ~ 1 + s(year_fct, bs = "re"),
    family = binomial,
    data = df,
    method = "REML"
  )
}

