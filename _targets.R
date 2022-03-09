## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  tar_file(file_demog, here("data", "Ha_survey_pre_submission.csv")),
  tar_file(file_clim, here("data", "xavier_daily_0.25x0.25.csv")),
  tar_file(file_1998, here("data", "ha_size_data_1998_cor.csv")),
  tar_file(file_2008, here("data", "Heliconia_acuminata_seedset_2008.csv")),
  
  demog = read_wrangle_demog(file_demog),
  clim = read_wrangle_clim(file_clim),
  data_full = join_data(demog, clim),
  
  data_1998 = read_wrangle_1998(file_1998),
  data_2008 = read.csv(file_2008),
  
  vit_list_det_ff = list(
    vit_surv = surv_det(data_full, habitat = "1-ha"),
    vit_size = size_det(data_full, habitat = "1-ha"),
    vit_flwr = flwr_det(data_full, habitat = "1-ha"),
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527, #germination and establishment
    vit_size_sdlg = size_sdlg_det(data_full, habitat = "1-ha"),
    vit_surv_sdlg = surv_sdlg_det(data_full, habitat = "1-ha")
  ),
  
  ipm_det = make_proto_ipm_det(vit_list_det_ff) %>% 
    make_ipm(iterations = 1000,
             usr_funs = list(get_scat_params = get_scat_params)),
  
  tar_render(paper, here("docs", "paper.Rmd"), packages = "bookdown")
) 
