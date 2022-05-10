#' Stochastic DLNM IPM mega-function for bootstrapping lambda
#'
#' This samples individuals with replacement, fits vital rate
#' models as a function of SPEI history, builds an IPM, iterates it, then
#' returns lambda. No intermediate steps are saved, so this is intended for use
#' in bootstrapping confidence intervals around lambda.
#' 
#' @param data demographic data.  E.g. the data_ff or data_cf targets
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param clim the clim target.  Full climate timeseries (no lags calculated).
#' @param ... other arguments passed to `ipmr::make_ipm()`
#'
#' @return lambda
#' 
ipm_boot_dlnm_raw <- function(data, vit_other, clim, year_seq = NULL, ...) {
  #sample plant_ids with replacement within plots
  boot_ids <-
    data %>% 
    summarize(plant_id = unique(plant_id)) %>% 
    sample_n(n(), replace = TRUE) %>% 
    #for validation:
    mutate(unique_id = paste(plant_id, row_number(), sep = "-"))
  
  boot <- inner_join(data, boot_ids, by = "plant_id")
  
  #fit vital rates
  vit_list_dlnm <- c(list(
    vit_surv = surv_dlnm(boot),
    vit_size = size_dlnm(boot),
    vit_flwr = flwr_dlnm(boot),
    vit_size_sdlg = size_sdlg_dlnm(boot),
    vit_surv_sdlg = surv_sdlg_dlnm(boot)
  ), vit_other)
  
  #make starting population vector
  pop_vec <- make_pop_vec(boot, n_mesh = 100)
  
  #make IPM
  make_proto_ipm_dlnm(vit_list_dlnm, pop_vec) %>% 
    make_dlnm_ipm(clim, seed = 1234, iterations = 1000,
                  return_sub_kernels = FALSE, # don't save every iteration
                  normalize_pop_size = TRUE,
                  year_seq = year_seq,
                  usr_funs = list(get_scat_params = get_scat_params),
                  ...) %>% 
    #calculate lambda
    ipmr::lambda(log = FALSE)
  
}
#return NA if errors in fitting or iterating IPM
ipm_boot_dlnm <- purrr::possibly(ipm_boot_dlnm_raw, otherwise = NA_real_)


