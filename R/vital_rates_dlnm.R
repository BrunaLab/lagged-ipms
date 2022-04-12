#for checking knots, load data and run k.check() for the models
# library(mgcv)
# library(tidyverse)
# data <- tar_read(data_full)

surv_dlnm <- function(data, sdlg = FALSE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == sdlg, habitat == habitat_choice)
  
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 10) +
      te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_dlnm(data))
# k.check(surv_dlnm(data, habitat = "1-ha"))


size_dlnm <- function(data, sdlg = FALSE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    dplyr::filter(sdlg_prev == sdlg, habitat == habitat_choice) %>% 
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
# k.check(size_dlnm(data))
# k.check(size_dlnm(data, habitat = "1-ha"))


flwr_dlnm <- function(data, habitat = c("CF", "1-ha")) {
  habitat_choice <- match.arg(habitat)
  df <- data %>% 
    dplyr::filter(habitat == habitat_choice, sdlg == FALSE) %>% 
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
# k.check(flwr_dlnm(data))
# k.check(flwr_dlnm(data, habitat = "1-ha"))

size_sdlg_dlnm <- function(data, sdlg = TRUE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    dplyr::filter(sdlg_prev == sdlg, habitat == habitat_choice) %>% 
    dplyr::filter(surv == 1, !is.na(log_size))
  
  bam(
    log_size ~ 1 + te(spei_history, L, bs = "cr", k = c(10, 15)),
    family = scat,
    data = df,
    method = "fREML"
  )
}
#TODO: double-check k for seedling size and survival
# k.check(size_sdlg_dlnm(data))

surv_sdlg_dlnm <- function(data, sdlg = TRUE, habitat = c("CF", "1-ha")) {
  
  habitat_choice <- match.arg(habitat)
  
  df <- data %>%
    filter(sdlg_prev == sdlg, habitat == habitat_choice)
  
  bam(
    surv ~ 1 + te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "fREML"
  )
}
# k.check(surv_sdlg_dlnm(data))
# k.check(surv_sdlg_dlnm(data, habitat = "1-ha"))

