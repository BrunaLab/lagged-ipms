#for checking knots, load data and run k.check() for the models
# library(mgcv)
# library(tidyverse)
# data <- tar_read(data_full)

surv_det <- function(data, sdlg = FALSE, habitat = c("CF", "1-ha")) {

  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == sdlg, habitat == habitat_choice)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 20),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_det(data))
# k.check(surv_det(data, habitat = "1-ha"))


size_det <- function(data, sdlg = FALSE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    dplyr::filter(sdlg_prev == sdlg, habitat == habitat_choice) %>% 
    #only plants that survived get to grow
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ s(log_size_prev, bs = "cr", k = 20),
    family = scat,
    data = df,
    method = "fREML"
  )
}
# k.check(size_det(data))
# k.check(size_det(data, habitat = "1-ha")) #no diff between k = 20 and k = 25


flwr_det <- function(data, habitat = c("CF", "1-ha")) {
  habitat_choice <- match.arg(habitat)
  df <- data %>% 
    dplyr::filter(habitat == habitat_choice, sdlg == FALSE) %>% 
    #current seedlings are excluded, but not plants that were seedlings in the previous year
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    flwr ~ s(log_size_prev, bs = "cr", k = 15),
    family = binomial, 
    data = df,
    method = "fREML"
  )
}
# k.check(flwr_det(data))
# k.check(flwr_det(data, habitat = "1-ha"))

size_sdlg_det <- function(data, sdlg = TRUE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    dplyr::filter(sdlg_prev == sdlg, habitat == habitat_choice) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  gam(
    log_size ~ 1,
    family = scat,
    data = df,
    method = "REML"
  )
}

surv_sdlg_det <- function(data, sdlg = TRUE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == sdlg, habitat == habitat_choice)
  
  gam(
    surv ~ 1,
    family = binomial,
    data = df,
    method = "REML"
  )
}
