#' Deterministic IPM mega-function for bootstrapping lambda
#'
#' This samples data with replacement, fits vital rate models, builds an IPM,
#' iterates it, then returns lambda. No intermediate steps are saved, so this is
#' intended for use in bootstrapping confidence intervals around lambda.
#' 
#' @param data the data_full target
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param habitat character, either "1-ha" or "CF"
#'
#' @return lambda
#' 
ipm_boot_det <- function(data, vit_other, habitat = c("1-ha", "CF")) {
  #sample with replacement
  boot <- dplyr::sample_n(data, nrow(data), replace = TRUE)
  
  #fit vital rates
  vit_list_det_ff <- c(list(
    vit_surv = surv_det(boot, habitat = habitat),
    vit_size = size_det(boot, habitat = habitat),
    vit_flwr = flwr_det(boot, habitat = habitat),
    vit_size_sdlg = size_sdlg_det(boot, habitat = habitat),
    vit_surv_sdlg = surv_sdlg_det(boot, habitat = habitat)
  ), vit_other)
  
  #make IPM
  make_proto_ipm_det(vit_list_det_ff) %>% 
    make_ipm(iterations = 100,  #only needs 100 to converge
             normalize_pop_size = FALSE, # to run as PVA
             usr_funs = list(get_scat_params = get_scat_params)
    ) %>% 
    #calculate lambda
    ipmr::lambda(log = FALSE)
  
}



