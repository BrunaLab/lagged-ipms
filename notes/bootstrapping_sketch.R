library(targets)
library(tidyverse)
library(progress)
library(tictoc)
library(ipmr)
library(metRology)
source("packages.R")
lapply(list.files("./R", full.names = TRUE), source)
conflict_prefer("filter", "dplyr")

tar_load(data_full)
tar_load(data_1998)
tar_load(data_2008)

#start with just 1-ha data
ha <- data_full %>% filter(habitat == "1-ha")

# using rsample package ---------------------------------------------------
library(rsample)

#possibly works best with targets since it is a dataframe that we can iterate over with dynamic branching.

ha_bt <- ha %>% bootstraps(10, apparent = TRUE) 
ha_bt

# IPM mega-function -------------------------------------------------------
ipm_det <- function(boots, habitat, ...) {
  splits <- boots[["splits"]] %>% map(analysis)
  map_dbl(splits, ~{
    
    vit_list_det_ff <- list(
      vit_surv = surv_det(.x, habitat = habitat),
      vit_size = size_det(.x, habitat = habitat),
      vit_flwr = flwr_det(.x, habitat = habitat),
      vit_fruits = fruits_gam(data_1998),
      vit_seeds = seeds_gam(data_1998, data_2008),
      vit_germ_est = 0.018921527, #germination and establishment
      vit_size_sdlg = size_sdlg_det(.x, habitat = habitat),
      vit_surv_sdlg = surv_sdlg_det(.x, habitat = habitat)
    )
    
    make_proto_ipm_det(vit_list_det_ff) %>% 
      make_ipm(iterations = 100,  #only needs 100 to converge
               normalize_pop_size = FALSE, # to run as PVA
               usr_funs = list(get_scat_params = get_scat_params)) %>% 
      ipmr::lambda(log = FALSE)
  })
}

lambda_bt <- ipm_det(ha_bt, habitat = "1-ha")


quantile(lambda_bt, c(0.025, 0.975))
mean(lambda_bt)


