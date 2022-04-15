#' Stochastic DLNM IPM mega-function for bootstrapping lambda
#'
#' This samples individuals within habitats with replacement, fits vital rate
#' models as a function of SPEI history, builds an IPM, iterates it, then
#' returns lambda. No intermediate steps are saved, so this is intended for use
#' in bootstrapping confidence intervals around lambda.
#' 
#' @param data the data_full target
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param habitat character, either "1-ha" or "CF"
#' @param clim the clim target.  Full climate timeseries (no lags calculated).
#'
#' @return lambda
#' 
ipm_boot_dlnm <- function(data, vit_other, habitat = c("1-ha", "CF"), clim) {
  #sample ha_id_numbers with replacement within plots
  boot_ids <-
    data %>% 
    group_by(habitat) %>% 
    summarize(ha_id_number = unique(ha_id_number)) %>% 
    group_by(habitat) %>% 
    sample_n(n(), replace = TRUE) %>% 
    #for validation:
    mutate(unique_id = paste(ha_id_number, row_number(), sep = "-"))
  
  boot <- inner_join(data, boot_ids)
  
  #fit vital rates
  vit_list_dlnm <- c(list(
    vit_surv = surv_dlnm(boot, habitat = habitat),
    vit_size = size_dlnm(boot, habitat = habitat),
    vit_flwr = flwr_dlnm(boot, habitat = habitat),
    vit_size_sdlg = size_sdlg_dlnm(boot, habitat = habitat),
    vit_surv_sdlg = surv_sdlg_dlnm(boot, habitat = habitat)
  ), vit_other)
  
  #make IPM
  make_proto_ipm_dlnm(vit_list_dlnm) %>% 
    make_dlnm_ipm(clim, seed = 1234, iterations = 1000,
                  return_sub_kernels = FALSE, # don't save every iteration
                  normalize_pop_size = TRUE,
                  usr_funs = list(get_scat_params = get_scat_params)) %>% 
    #calculate lambda
    ipmr::lambda(log = FALSE)
  
}



