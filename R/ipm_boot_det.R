#' Deterministic IPM mega-function for bootstrapping lambda
#'
#' This samples individuals with replacement, fits vital rate
#' models, builds an IPM, iterates it, then returns lambda. No intermediate
#' steps are saved, so this is intended for use in bootstrapping confidence
#' intervals around lambda.
#' 
#' @param data demographic data.  E.g. the data_ff or data_cf targets
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param ... other arguments passed to `ipmr::make_ipm()`
#' 
#' @return lambda
#' 
ipm_boot_det <- function(data, vit_other, ...) {
  #sample plant_ids with replacement within plots
  boot_ids <-
    data %>% 
    summarize(plant_id = unique(plant_id)) %>% 
    sample_n(n(), replace = TRUE) %>% 
    #for validation:
    mutate(unique_id = paste(plant_id, row_number(), sep = "-"))
  
  boot <- inner_join(data, boot_ids, by = "plant_id")
  
  #fit vital rates
  vit_list_det <- c(list(
    vit_surv = surv_det(boot),
    vit_size = size_det(boot),
    vit_flwr = flwr_det(boot),
    vit_size_sdlg = size_sdlg_det(boot),
    vit_surv_sdlg = surv_sdlg_det(boot)
  ), vit_other)
  
  #make starting population vector
  pop_vec <- make_pop_vec(boot, n_mesh = 100)
  
  #make IPM
  make_proto_ipm_det(vit_list_det, pop_vec) %>% 
    make_ipm(iterations = 100,  #only needs 100 to converge
             normalize_pop_size = TRUE,
             usr_funs = list(get_scat_params = get_scat_params),
             ...
    ) %>% 
    #calculate lambda
    ipmr::lambda(log = FALSE)
  
}



