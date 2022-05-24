# Uncomment these lines to run locally on multiple cores
# options(
#   clustermq.scheduler = "multiprocess"
# )


# Or use these options to setup the SSH connector to use hipergator (or another
# HPC)
options(
  clustermq.scheduler = "ssh",
  clustermq.template = "ssh_clustermq.tmpl", #custom SSH template to use R 4.0
  clustermq.ssh.host = "hpg", #set up in ~/.ssh/config.
  clustermq.ssh.timeout = 80, #longer timeout helps with 2FA
  clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging
)

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # Load and wrangle data ---------------------------------------------------
  # All of these targets are run locally (deployment = "main")
  # Heliconia demographic datset
  tar_file(file_demog, here("data", "HDP_1997_2009.csv"), deployment = "main"),
  tar_file(file_plots, here("data", "HDP_plots.csv"), deployment = "main"),
  
  # Climate data from Xavier et al. 2016
  tar_file(file_clim, here("data", "xavier_daily_0.25x0.25.csv"), deployment = "main"),
  
  # Other unpublished Heliconia datasets for fruit and seed vital rates
  tar_file(file_1998, here("data", "ha_size_data_1998_cor.csv"), deployment = "main"),
  tar_file(file_2008, here("data", "Heliconia_acuminata_seedset_2008.csv"),
           deployment = "main"),
  tar_target(data_1998, read_wrangle_1998(file_1998), deployment = "main"),
  tar_target(data_2008, read.csv(file_2008), deployment = "main"),
  
  # Wrangle and combine data
  tar_target(demog, read_wrangle_demog(file_demog, file_plots), deployment = "main"),
  tar_target(clim, read_wrangle_clim(file_clim), deployment = "main"),
  tar_target(data_full, join_data(demog, clim), deployment = "main"),
  
  # Subset data since separate IPMs are fit for continuous forest (cf) and forest
  # fragments (ff)
  tar_target(data_cf, data_full %>% filter(habitat == "CF"), deployment = "main"),
  tar_target(data_ff, data_full %>% filter(habitat == "1-ha"), deployment = "main"),
  

  # Vital rate models -------------------------------------------------------

  ## ├Shared vital rates ----------------------------------------------------
  # Vital rates from other datasets besides the 10 year demographic experiment
  # that are used in all IPMs
  vit_other_ff = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.018921527 #germination and establishment from Bruna 2002. Fig 3
  ),
  
  vit_other_cf = list(
    vit_fruits = fruits_gam(data_1998),
    vit_seeds = seeds_gam(data_1998, data_2008),
    vit_germ_est = 0.057098765 #germination and establishment from Bruna 2002. Fig 3
  ),

  ## ├Deterministic ---------------------------------------------------------
  
  #Deterministic IPMs have vital rates that include a smooth function of
  #log(size) in the previous timestep only.
  
  ## forests fragments
  vit_list_det_ff = c(
    list(
      vit_surv = surv_det(data_ff),
      vit_size = size_det(data_ff),
      vit_flwr = flwr_det(data_ff),
      vit_size_sdlg = size_sdlg_det(data_ff),
      vit_surv_sdlg = surv_sdlg_det(data_ff)
    ),
    vit_other_ff),
  
  ## continuous forest
  vit_list_det_cf = c(
    list(
      vit_surv = surv_det(data_cf),
      vit_size = size_det(data_cf),
      vit_flwr = flwr_det(data_cf),
      vit_size_sdlg = size_sdlg_det(data_cf),
      vit_surv_sdlg = surv_sdlg_det(data_cf)
    ),
    vit_other_cf),
  
  ## ├Stochastic (random effect of year) ------------------------------------
  
  # For more on how environmental stochasticity was modeled, see
  # notes/environmental-stochasticity.Rmd
  
  ## forest fragments
  vit_list_stoch_ff = c(
    list(
      vit_surv = surv_raneff(data_ff),
      vit_size = size_raneff(data_ff),
      vit_flwr = flwr_raneff(data_ff),
      vit_size_sdlg = size_sdlg_raneff(data_ff),
      vit_surv_sdlg = surv_sdlg_raneff(data_ff)
    ), 
    vit_other_ff),
  
  ## continuous forest
  vit_list_stoch_cf = c(
    list(
      vit_surv = surv_raneff(data_cf),
      vit_size = size_raneff(data_cf),
      vit_flwr = flwr_raneff(data_cf),
      vit_size_sdlg = size_sdlg_raneff(data_cf),
      vit_surv_sdlg = surv_sdlg_raneff(data_cf)
    ),
    vit_other_cf),
  
  ## ├DLNMs -----------------------------------------------------------------
  
  # Vital rates for these IPMs are similar to those fit in Scott et al. 2022
  # (distributed lag non-linear models that model lagged effecst of SPEI
  # explicitly)
  
  # forest fragments
  vit_list_dlnm_ff = c(
    list(
      vit_surv = surv_dlnm(data_ff),
      vit_size = size_dlnm(data_ff),
      vit_flwr = flwr_dlnm(data_ff),
      vit_size_sdlg = size_sdlg_dlnm(data_ff),
      vit_surv_sdlg = surv_sdlg_dlnm(data_ff)
    ),
    vit_other_ff),
  
  # continuous forest
  vit_list_dlnm_cf = c(
    list(
      vit_surv = surv_dlnm(data_cf),
      vit_size = size_dlnm(data_cf),
      vit_flwr = flwr_dlnm(data_cf),
      vit_size_sdlg = size_sdlg_dlnm(data_cf),
      vit_surv_sdlg = surv_sdlg_dlnm(data_cf)
    ),
    vit_other_cf),
  
  
  ## ├Model Selection Table ---------------------------------------------------
  #Within each habitat type and vital rate, which type of model fits the data
  #best?
  aic_tbl_df = make_AIC_tbl(
    vit_list_det_cf,
    vit_list_det_ff,
    vit_list_dlnm_cf,
    vit_list_dlnm_ff,
    vit_list_stoch_cf,
    vit_list_stoch_ff
  ),
  
  # IPMs --------------------------------------------------------------------
  # IPMs are constructed and iterated using functions that wrap functions from
  # the `ipmr` package.
  
  ## ├Simulation Parameters -------------------------------------------------
  # Starting population for simulations
  
  # Start with 1000 individuals distributed in size classes in a way that
  # matches observed data
  pop_vec_ff = make_pop_vec(data_ff, n_mesh = 100, n = 1000),
  pop_vec_cf = make_pop_vec(data_cf, n_mesh = 100, n = 1000),
  
  # Sequence of years to use for stochastic simulations
  year_seq = sample(2000:2009, 1000, replace = TRUE),
  
  ## ├Deterministic ---------------------------------------------------------

  ipm_det_ff = make_proto_ipm_det(vit_list_det_ff, pop_vec_ff) %>% 
    make_ipm(iterations = 1000,  #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  ipm_det_cf = make_proto_ipm_det(vit_list_det_cf, pop_vec_cf) %>% 
    make_ipm(iterations = 1000, #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  
  ## ├Stochastic, matrix sampling --------------------------------------------
  # year_seq ensures that all IPMs use the same sequence of years so that
  # comparisons between habitats and between these and DLNM IPMs are fair.
  ipm_stoch_ff = make_proto_ipm_stoch(vit_list_stoch_ff, pop_vec_ff) %>%
    make_ipm(iterations = 1000,
             kernel_seq = year_seq,
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  ipm_stoch_cf = make_proto_ipm_stoch(vit_list_stoch_cf, pop_vec_cf) %>%
    make_ipm(iterations = 1000,
             kernel_seq = year_seq,
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)),
  
  ## ├Stochastic, DLNM ------------------------------------------------------
  # These are set up a little differently since to iterate the IPM I need to
  # first create the sequence of environments (lagged SPEI history) from year
  # sequence of years and the climate data.  `make_dlnm_ipm()` does this, then
  # add the environmental states to the proto IPM, then iterates the IPM.
  
  proto_ipm_dlnm_ff = make_proto_ipm_dlnm(vit_list_dlnm_ff, pop_vec_ff),
  
  ipm_dlnm_ff = proto_ipm_dlnm_ff %>%
    make_dlnm_ipm(
      clim,
      seed = 1234,
      year_seq = year_seq,
      normalize_pop_size = FALSE,
      return_sub_kernels = TRUE,
      usr_funs = list(get_scat_params = get_scat_params)
    ),
 
  proto_ipm_dlnm_cf = make_proto_ipm_dlnm(vit_list_dlnm_cf, pop_vec_cf),
  
  ipm_dlnm_cf = proto_ipm_dlnm_cf %>%
    make_dlnm_ipm(
      clim,
      seed = 1234,
      year_seq = year_seq,
      normalize_pop_size = FALSE,
      return_sub_kernels = TRUE,
      usr_funs = list(get_scat_params = get_scat_params)
    ),
 
  # Bootstrapped Lambdas -----------------------------------------------------------------
  # uses tar_rep() target factory to create dynamic branches to do 500
  # bootstraps of data, then use all-in-one functions to fit vital rate models,
  # build IPM, iterate, and return lambdas.
  
  ## ├Deterministic ---------------------------------------------------------
  tar_rep(
    lambda_bt_det_ff,
    ipm_boot_det(data_ff, vit_other = vit_other_ff),
    batches = 5, #number of branches to create
    reps = 100 #reps per branch
  ),

  tar_rep(
    lambda_bt_det_cf,
    ipm_boot_det(data_cf, vit_other = vit_other_cf),
    batches = 5, #number of branches to create
    reps = 100 #reps per branch
  ),
  
  ## ├Stochastic, matrix sampling --------------------------------------------
  tar_rep(
    lambda_bt_stoch_ff,
    ipm_boot_stoch(data_ff, vit_other = vit_other_ff, year_seq = year_seq),
    batches = 5,
    reps = 100
  ),

  tar_rep(
    lambda_bt_stoch_cf,
    ipm_boot_stoch(data_cf, vit_other = vit_other_cf, year_seq = year_seq),
    batches = 5,
    reps = 100
  ),

  ## ├Stochastic, DLNM ------------------------------------------------------
  # For these I use more batches, fewer reps because each rep takes about 90 min.
  # That way I can make incremental progress easier.
  # 
  tar_rep(
    lambda_bt_dlnm_ff,
    ipm_boot_dlnm(data_ff, vit_other = vit_other_ff, clim = clim, year_seq = year_seq),
    batches = 500,
    reps = 1
  ),

  tar_rep(
    lambda_bt_dlnm_cf,
    ipm_boot_dlnm(data_cf, vit_other = vit_other_cf, clim = clim, year_seq = year_seq),
    batches = 500,
    reps = 1
  ),
  
  ## ├Lambda table ------------------------------------------------------
  tar_target(
    lambda_table,
    make_lambda_table(
      ipm_list = list(
        det_ff = ipm_det_ff,
        det_cf = ipm_det_cf,
        stoch_ff = ipm_stoch_ff,
        stoch_cf = ipm_stoch_cf#,
        # dlnm_ff = ipm_dlnm_ff,
        # dlnm_cf = ipm_dlnm_cf
      ),
      bt_list = list(
        det_ff = lambda_bt_det_ff,
        det_cf = lambda_bt_det_cf,
        stoch_ff = lambda_bt_stoch_ff,
        stoch_cf = lambda_bt_stoch_cf#,
        # dlnm_ff = lambda_bt_dlnm_ff,
        # dlnm_cf = lambda_bt_dlnm_cf
      )
    ),
    deployment = "main" #data limited, not computation limited
  ),


  # Manuscript --------------------------------------------------------------
  tar_render(paper, here("docs", "paper.Rmd"), packages = "bookdown", output_format = "all")
  
  
) %>% 
  #always use dplyr::filter() and dplyr::lag()
  tar_hook_before(hook = conflicted::conflict_prefer("filter", "dplyr")) %>% 
  tar_hook_before(hook = conflicted::conflict_prefer("lag", "dplyr"))
