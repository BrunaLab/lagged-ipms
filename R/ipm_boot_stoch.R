#' Stochastic IPM mega-function for bootstrapping lambda
#'
#' This samples individuals with replacement, fits vital rate
#' models with a random effect of `year`, builds an IPM, iterates it, then
#' returns lambda. No intermediate steps are saved, so this is intended for use
#' in bootstrapping confidence intervals around lambda.
#' 
#' @param data demographic data.  E.g. the data_ff or data_cf targets
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param ... other arguments passed to `ipmr::make_ipm()`
#' 
#' @return lambda
#' 
ipm_boot_stoch <- function(data, vit_other, year_seq = NULL, ...) {
  #sample ha_id_numbers with replacement within plots
  boot_ids <-
    data %>% 
    summarize(ha_id_number = unique(ha_id_number)) %>% 
    sample_n(n(), replace = TRUE) %>% 
    #for validation:
    mutate(unique_id = paste(ha_id_number, row_number(), sep = "-"))
  
  boot <- inner_join(data, boot_ids)

  #fit vital rates
  vit_list_stoch <- c(list(
    vit_surv = surv_raneff(boot),
    vit_size = size_raneff(boot),
    vit_flwr = flwr_raneff(boot),
    vit_size_sdlg = size_sdlg_raneff(boot),
    vit_surv_sdlg = surv_sdlg_raneff(boot)
  ), vit_other)
  
  #make starting population vector
  pop_vec <- make_pop_vec(boot, n_mesh = 100)
  
  #make IPM
  make_proto_ipm_stoch(vit_list_stoch, pop_vec) %>% 
    make_ipm(iterations = 1000,  
             normalize_pop_size = TRUE,
             kernel_seq = year_seq,
             usr_funs = list(get_scat_params = get_scat_params),
             ...
    ) %>% 
    #calculate lambda
    ipmr::lambda(log = FALSE)
  
}



