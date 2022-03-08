## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  tar_file(demog_file, here("data", "Ha_survey_pre_submission.csv")),
  tar_file(clim_file, here("data", "xavier_daily_0.25x0.25.csv")),
  demog = read_wrangle_demog(demog_file),
  clim = read_wrangle_clim(clim_file),
  data_full = join_data(demog, clim),
  vit_list_det_ff = list(
    surv = surv_det(data_full, habitat = "1-ha")
  )
) 
