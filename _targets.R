# Uncomment these lines to run locally on multiple cores
# options(
#   clustermq.scheduler = "multiprocess"
# )


# # Setup SSH connector
options(
  clustermq.scheduler = "ssh",
  clustermq.template = "ssh_clustermq.tmpl", #custom SSH template to use R 4.0
  clustermq.ssh.host = "hpg", #set up in ~/.ssh/config.
  clustermq.ssh.timeout = 30, #longer timeout helps with 2FA
  clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging
)

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # Load and wrangle data ---------------------------------------------------
  
  tar_file(file_demog, here("data", "Ha_survey_pre_submission.csv"), deployment = "main"),
  tar_file(file_clim, here("data", "xavier_daily_0.25x0.25.csv"), deployment = "main"),
  tar_file(file_1998, here("data", "ha_size_data_1998_cor.csv"), deployment = "main"),
  tar_file(file_2008, here("data", "Heliconia_acuminata_seedset_2008.csv"), deployment = "main"),
  
  demog = read_wrangle_demog(file_demog),
  clim = read_wrangle_clim(file_clim),
  data_full = join_data(demog, clim),
  
  data_1998 = read_wrangle_1998(file_1998),
  data_2008 = read.csv(file_2008),
  

  # Shared vital rates ------------------------------------------------------
  # Vital rates from other datasets besides the 10 year demographic experiment
  vit_other_ff = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527 #germination and establishment
  ),
  
  vit_other_cf = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.057098765 #germination and establishment
  ),

  
  # Deterministic IPM -------------------------------------------------------
  ## forests fragments
  vit_list_det_ff = c(
    list(
      vit_surv = surv_det(data_full, habitat = "1-ha"),
      vit_size = size_det(data_full, habitat = "1-ha"),
      vit_flwr = flwr_det(data_full, habitat = "1-ha"),
      vit_size_sdlg = size_sdlg_det(data_full, habitat = "1-ha"),
      vit_surv_sdlg = surv_sdlg_det(data_full, habitat = "1-ha")
    ),
    vit_other_ff),
  
  ipm_det_ff = make_proto_ipm_det(vit_list_det_ff) %>% 
    make_ipm(iterations = 1000,  #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  ## continuous forest
  vit_list_det_cf = c(
    list(
      vit_surv = surv_det(data_full, habitat = "CF"),
      vit_size = size_det(data_full, habitat = "CF"),
      vit_flwr = flwr_det(data_full, habitat = "CF"),
      vit_size_sdlg = size_sdlg_det(data_full, habitat = "CF"),
      vit_surv_sdlg = surv_sdlg_det(data_full, habitat = "CF")
    ),
    vit_other_cf),
  
  ipm_det_cf = make_proto_ipm_det(vit_list_det_cf) %>% 
    make_ipm(iterations = 1000, #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  
  # Stochastic kernel resampled IPM -----------------------------------------
  
  ## forets fragments
  vit_list_stoch_ff = c(
    list(
      vit_surv = surv_raneff(data_full, habitat = "1-ha"),
      vit_size = size_raneff(data_full, habitat = "1-ha"),
      vit_flwr = flwr_raneff(data_full, habitat = "1-ha"),
      vit_size_sdlg = size_sdlg_raneff(data_full, habitat = "1-ha"),
      vit_surv_sdlg = surv_sdlg_raneff(data_full, habitat = "1-ha")
    ), 
    vit_other_ff),
  
  ipm_stoch_ff = make_proto_ipm_stoch(vit_list_stoch_ff) %>%
    make_ipm(iterations = 1000,
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  ## continuous forest
  vit_list_stoch_cf = c(
    list(
      vit_surv = surv_raneff(data_full, habitat = "CF"),
      vit_size = size_raneff(data_full, habitat = "CF"),
      vit_flwr = flwr_raneff(data_full, habitat = "CF"),
      vit_size_sdlg = size_sdlg_raneff(data_full, habitat = "CF"),
      vit_surv_sdlg = surv_sdlg_raneff(data_full, habitat = "CF")
    ),
    vit_other_cf),
  
  ipm_stoch_cf = make_proto_ipm_stoch(vit_list_stoch_cf) %>%
    make_ipm(iterations = 1000,
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  
  # Stochastic parameter resampled IPM --------------------------------------
  
  ## forest fragments
  vit_list_dlnm_ff = c(
    list(
      vit_surv = surv_dlnm(data_full, habitat = "1-ha"),
      vit_size = size_dlnm(data_full, habitat = "1-ha"),
      vit_flwr = flwr_dlnm(data_full, habitat = "1-ha"),
      vit_size_sdlg = size_sdlg_dlnm(data_full, habitat = "1-ha"),
      vit_surv_sdlg = surv_sdlg_dlnm(data_full, habitat = "1-ha")
    ),
    vit_other_ff),
  
  proto_ipm_dlnm_ff = make_proto_ipm_dlnm(vit_list_dlnm_ff),
  ipm_dlnm_ff = proto_ipm_dlnm_ff %>%
    make_dlnm_ipm(clim, seed = 1234, iterations = 500,
                  return_sub_kernels = FALSE, # don't save every iteration
                  normalize_pop_size = FALSE, # to run as PVA
                  usr_funs = list(get_scat_params = get_scat_params)),
  
  ## continuous forest
  vit_list_dlnm_cf = c(
    list(
      vit_surv = surv_dlnm(data_full, habitat = "CF"),
      vit_size = size_dlnm(data_full, habitat = "CF"),
      vit_flwr = flwr_dlnm(data_full, habitat = "CF"),
      vit_size_sdlg = size_sdlg_dlnm(data_full, habitat = "CF"),
      vit_surv_sdlg = surv_sdlg_dlnm(data_full, habitat = "CF")
    ),
    vit_other_cf),
  
  proto_ipm_dlnm_cf = make_proto_ipm_dlnm(vit_list_dlnm_cf),
  ipm_dlnm_cf = proto_ipm_dlnm_cf %>%
    make_dlnm_ipm(clim, seed = 1234, iterations = 500,
                  return_sub_kernels = FALSE, # don't save every iteration
                  normalize_pop_size = FALSE, # to run as PVA
                  report_progress = TRUE,
                  usr_funs = list(get_scat_params = get_scat_params)),
  
  
  # Model Selection Table ---------------------------------------------------
  
  aic_tbl_df = make_AIC_tbl(
    vit_list_det_cf,
    vit_list_det_ff,
    vit_list_dlnm_cf,
    vit_list_dlnm_ff,
    vit_list_stoch_cf,
    vit_list_stoch_ff
  ),

# Lambdas -----------------------------------------------------------------

  # bootstrapping
#TODO: add tar_rep() for stochastic and dlnm models
  tar_rep(
    lambda_bt_det_ff,
    ipm_boot_det(data_full, vit_other = vit_other_ff, habitat = "1-ha"),
    batches = 5, #number of branches to create
    reps = 100 #reps per branch
  ),

  tar_rep(
    lambda_bt_det_cf,
    ipm_boot_det(data_full, vit_other = vit_other_cf, habitat = "CF"),
    batches = 5, #number of branches to create
    reps = 100 #reps per branch
  ),

#TODO: edit this function to use `calc_ci()` to get estimate, lower, upper
  lambda_tbl_df = make_lambda_tbl(
    ipm_det_cf,
    ipm_det_ff,
    ipm_stoch_cf,
    ipm_stoch_ff,
    ipm_dlnm_cf,
    ipm_dlnm_ff
  ),
  # Manuscript --------------------------------------------------------------
  tar_render(paper, here("docs", "paper.Rmd"), packages = "bookdown")
) %>% 
  tar_hook_before(hook = conflicted::conflict_prefer("filter", "dplyr")) %>% 
  tar_hook_before(hook = conflicted::conflict_prefer("lag", "dplyr"))
