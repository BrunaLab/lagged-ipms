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

#create list of bootstrapped samples.  Not really useable because of the overhead of the data object it will create with 1000 bootstraps.
ha_bt <- list(1:10)
for(i in 1:10) {
  ha_bt[[i]] <- ha %>% sample_n(nrow(.), replace = TRUE)
}
# ha_bt


# IPM mega-function -------------------------------------------------------
ipm_det <- function(data, habitat, ...) {
  vit_list_det_ff <- list(
    vit_surv = surv_det(data, habitat = habitat),
    vit_size = size_det(data, habitat = habitat),
    vit_flwr = flwr_det(data, habitat = habitat),
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527, #germination and establishment
    vit_size_sdlg = size_sdlg_det(data, habitat = habitat),
    vit_surv_sdlg = surv_sdlg_det(data, habitat = habitat)
  )
  
  make_proto_ipm_det(vit_list_det_ff) %>% 
    make_ipm(iterations = 100,  #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)) %>% 
    ipmr::lambda(log = FALSE)
}


#run on original data
ipm_det(ha, habitat = "1-ha")

#run on a single bootstrap
ipm_det(ha_bt[[1]], habitat = "1-ha")

#map to all bootstraps
#with progress bar
pb <- progress_bar$new(total = 10)
tic()
lambda_bt <- ha_bt %>%
  map_dbl(~{
    pb$tick()
    ipm_det(.x, habitat = "1-ha")
  })

#95% CI (I think?)
quantile(lambda_bt, c(0.025, 0.975))
toc() #33 seconds with 100 iterations and 10 samples
# using boot package ------------------------------------------------------

library(boot)

tic()
res <- boot(ha, statistic = ipm_det, habitat = "1-ha", R = 10)
boot.ci(res, type = "perc")
toc() #27 seconds

# using rsample package ---------------------------------------------------
library(rsample)

#possibly works best with targets since it is a dataframe that we can iterate over with dynamic branching.

ha_bt <- ha %>% bootstraps(10, apparent = TRUE) 

ipm_det_wrap <- function(split, ...) {
  tibble(term = "lambda",
         estimate = ipm_det(data = analysis(split), ...),
         std.error = NA_real_) #don't know formula for this
}

tic()
res <- ha_bt %>% 
  mutate(lambda = map(splits, ~ipm_det_wrap(.x, habitat = "1-ha")))

int_pctl(res, lambda)
toc() #35.9 seconds with 100 iterations and 10 samples.  Roughly 1 hour total computation for 1000 samples.


# scale up to dlnm IPM ----------------------------------------------------
tar_load(clim)

# This is going to take a lot longer
ipm_dlnm <- function(data, habitat, ...) {
  vit_list_dlnm_ff = list(
    vit_surv = surv_dlnm(data, habitat = "1-ha"),
    vit_size = size_dlnm(data, habitat = "1-ha"),
    vit_flwr = flwr_dlnm(data, habitat = "1-ha"),
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527, #germination and establishment
    vit_size_sdlg = size_sdlg_dlnm(data, habitat = "1-ha"),
    vit_surv_sdlg = surv_sdlg_dlnm(data, habitat = "1-ha")
  )

#TODO: probably need to update make_env_states() to take `data` as well so when it limits extrapolation it can use the bootstrapped data. 
make_proto_ipm_dlnm(vit_list_dlnm_ff) %>%
  make_dlnm_ipm(clim, seed = 1234, iterations = 1000,
                return_sub_kernels = FALSE, # don't save every iteration
                normalize_pop_size = FALSE, # to run as PVA
                report_progress = TRUE,
                usr_funs = list(get_scat_params = get_scat_params)) %>% 
  lambda(log = FALSE)
}

tic()
ipm_dlnm(data_full, habitat = "1-ha") #this breaks for some reason.  Something with environments not getting passed correctly?
toc()
# ??? seconds for a single lambda with 1000 iterations
