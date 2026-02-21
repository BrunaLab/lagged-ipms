
#'
#' This samples individuals with replacement, fits vital rate
#' models, builds an IPM, iterates it, then returns lambda. No intermediate
#' steps are saved, so this is intended for use in bootstrapping confidence
#' intervals around lambda.
#' 
#' 
## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
# lapply(list.files("./R", full.names = TRUE), source)

conflicts_prefer(dplyr::filter)
## tar_plan supports drake-style targets and also tar_target()
# tar_plan(
  
  # Load and wrangle data ---------------------------------------------------
  # All of these targets are run locally (deployment = "main")
  # Heliconia demographic datset
  # tar_file(file_demog, here("data", "HDP_1997_2009.csv"), deployment = "main")
  # tar_file(file_plots, here("data", "HDP_plots.csv"), deployment = "main")
  # 
  # # Other unpublished Heliconia datasets for fruit and seed vital rates
  # tar_file(file_1998, here("data", "ha_size_data_1998_cor.csv"), deployment = "main")
  # tar_file(file_2008, here("data", "Heliconia_acuminata_seedset_2008.csv"),
  #          deployment = "main")
  # tar_target(data_1998, read_wrangle_1998(file_1998), deployment = "main")
  # tar_target(data_2008, read.csv(file_2008), deployment = "main")
  # # 
  # # # Wrangle and combine data
  # tar_target(demog, read_wrangle_demog(file_demog, file_plots), deployment = "main")
  # tar_target(data_full, join_data(demog, clim), deployment = "main")
  
  
  # 
  # 
  # tar_target(data_cf, data_full %>% filter(habitat == "CF"), deployment = "main")
  # 
  # tar_target(data_ff, data_full %>% filter(habitat == "1-ha"), deployment = "main")
  # 
  
  data_full<-tar_read(data_full)
  data_1998<-tar_read(data_1998)
  data_2008<-tar_read(data_2008)
  
  
  n_cf<-data_full %>%
    filter(habitat=="CF") %>% 
    summarize(n=n_distinct(plant_id))
  n_ff<-data_full %>%
    filter(habitat=="1-ha") %>% 
    summarize(n=n_distinct(plant_id))
  
  
  plant_ids_all_plants<-data_full %>%
    select(plant_id) %>%
    distinct()
  
  # If true you have the ciorrect number of plants in your vector iof id's to be bootstrapped
  (n_cf+n_ff)==(plant_ids_all_plants %>% summarize(n=n_distinct(plant_id)))
  
  # select the plants for the bootstrap pop in ff and cf by sampling 
  # n_ff and c_ff, respectively, with replacement
  
  
  
  
  
  
  
  
  
  
  
  
  n_runs<-2000
  runs<-seq(1:n_runs)
  
  rand_lambda_cf_list<- list()
  rand_lambda_ff_list<- list()
  
  
  
  
  
    for (i in seq_along(runs)){
  
  
      
  rand_plantIDs_ff<-plant_ids_all_plants %>% 
    slice_sample(n = n_ff$n, replace = FALSE)
  
  rand_plantIDs_cf<-plant_ids_all_plants %>% 
    slice_sample(n = n_cf$n, replace = FALSE)

  
  # pull in the demographic data for the selected plants from `all_plants`
  rand_data_ff<-data_full %>% 
    filter(plant_id%in%rand_plantIDs_ff$plant_id)
  
  rand_data_cf<-data_full %>% 
    filter(plant_id%in%rand_plantIDs_cf$plant_id)
  
  
  # Vital rate models -------------------------------------------------------
  
  ## ├Shared vital rates ----------------------------------------------------
  # Vital rates from other datasets besides the 10 year demographic experiment
  # that are used in all IPMs
  
  source("./R/fruits_seeds.R")
  
  
  vit_other_ff = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527 #germination and establishment from Bruna 2002. Fig 3
  )
  
  vit_other_cf = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.057098765 #germination and establishment from Bruna 2002. Fig 3
  )
  
  ## ├Deterministic ---------------------------------------------------------
  
  #Deterministic IPMs have vital rates that include a smooth function of
  #log(size) in the previous timestep only.
  
  source("./R/vital_rates_simple.R")
  ## forests fragments
  vit_list_det_ff = c(
    list(
      vit_surv = surv_det(rand_data_ff),
      vit_size = size_det(rand_data_ff),
      vit_flwr = flwr_det(rand_data_ff),
      vit_size_sdlg = size_sdlg_det(rand_data_ff),
      vit_surv_sdlg = surv_sdlg_det(rand_data_ff)
    ),
    vit_other_ff)
  
  ## continuous forest
  vit_list_det_cf = c(
    list(
      vit_surv = surv_det(rand_data_cf),
      vit_size = size_det(rand_data_cf),
      vit_flwr = flwr_det(rand_data_cf),
      vit_size_sdlg = size_sdlg_det(rand_data_cf),
      vit_surv_sdlg = surv_sdlg_det(rand_data_cf)
    ),
    vit_other_cf)
  
  ## ├Simulation Parameters -------------------------------------------------
  # Starting population for simulations
  
  # Start with 1000 individuals distributed in size classes in a way that
  # matches observed data
  source("./R/make_pop_vec.R")
  pop_vec_ff = make_pop_vec(rand_data_ff, n_mesh = 100, n = 1000)
  pop_vec_cf = make_pop_vec(rand_data_cf, n_mesh = 100, n = 1000)
  
  # Sequence of years to use for stochastic simulations
  year_seq = sample(2000:2009, 1000, replace = TRUE)
  
  ## ├Deterministic ---------------------------------------------------------
  
  
  source("./R/make_proto_ipm_det.R")
  source("./R/get_scat_params.R")
  source("./packages.R")
  # library(ipmr)
  # library(metRology)
  # library(stats)
  
  ipm_det_ff <- tryCatch({
    
  
  make_proto_ipm_det(vit_list_det_ff, pop_vec_ff) %>% 
    make_ipm(iterations = 1000,  #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params))
  
  
  }, error = function(e) {
    message(sprintf("Run %d failed: %s", i, e$message))
    # Return NULL or some placeholder to indicate failure
  
    
  })
  
  if (is.numeric(ipm_det_ff$pop_state$lambda[1000])) {
    rand_lambda_ff<-ipm_det_ff$pop_state$lambda[1000]
  } else {
    rand_lambda_ff<-"run_failed"
  }
  
  
  ipm_det_cf <- tryCatch({
    
    
    make_proto_ipm_det(vit_list_det_cf, pop_vec_cf) %>% 
      make_ipm(iterations = 1000,  #only needs 100 to converge
               normalize_pop_size = FALSE, # to run as PVA
               usr_funs = list(get_scat_params = get_scat_params))
    
    
  }, error = function(e) {
    message(sprintf("Run %d failed: %s", i, e$message))
    # Return NULL or some placeholder to indicate failure
    
    
  })
  
  if (is.numeric(ipm_det_cf$pop_state$lambda[1000])) {
    rand_lambda_cf<-ipm_det_cf$pop_state$lambda[1000]
  } else {
    rand_lambda_cf<-"run_failed"
  }
  
  
  
  
  # library(ipmr)
  # library(stats)
  # source("./packages.R")
  # ipm_det_cf = make_proto_ipm_det(vit_list_det_cf, pop_vec_cf) %>% 
  #   make_ipm(iterations = 1000, #only needs 100 to converge
  #            normalize_pop_size = FALSE, # to run as PVA
  #            usr_funs = list(get_scat_params = get_scat_params))
  # 
  # 
  # 
  # rand_lambda_cf<-ipm_det_cf$pop_state$lambda[1000]
  
  
  
  
    rand_lambda_cf_list[[i]]<-rand_lambda_cf
    rand_lambda_ff_list[[i]]<-rand_lambda_ff
    
  
    
  
  
    }

  rand_lambda_cf<-unlist(rand_lambda_cf_list,use.names = TRUE) %>% tibble() %>% rename(rand_lambda_cf=".")
  rand_lambda_ff<-unlist(rand_lambda_ff_list,use.names = TRUE) %>% tibble() %>% rename(rand_lambda_ff=".")
  
  
  randomized_lambdas<-bind_cols(rand_lambda_cf,rand_lambda_ff) %>% 
    mutate(run=row_number()) 
  
  write_csv(randomized_lambdas,"./docs/figures/det_lambda_randomization_test.csv")
  
  randomized_lambdas<-randomized_lambdas %>%   
    mutate(rand_lambda_cf=case_when(
      rand_lambda_cf=="run_failed"~NA,
      .default = as.numeric(rand_lambda_cf)
    )) %>% 
    mutate(rand_lambda_ff=case_when(
      rand_lambda_ff=="run_failed"~NA,
      .default = as.numeric(rand_lambda_ff)
    )) %>% 
    drop_na() %>% 
    mutate(abs_diff_l=abs(rand_lambda_cf-rand_lambda_ff))
  
  
  
  
  randomized_lambdas<-randomized_lambdas %>%  
    slice_head(n=1000) %>% 
    arrange(desc(abs_diff_l))
  
  
  det_ff<-0.9778
  det_cf<-0.9897
  abs_diff_det<-abs(det_ff-det_cf)
  
  greater_than_obs<-randomized_lambdas %>%
    filter(abs_diff_l>abs_diff_det) %>% 
    summarize(greater_than_obs=n()) %>% 
    mutate(perc_below=(greater_than_obs+1)/(nrow(randomized_lambdas)+1)*100)
  
  
  write_csv(less_than_obs,"./docs/figures/det_randomization_test.csv")
  
  