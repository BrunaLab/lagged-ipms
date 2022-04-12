#' Deterministic IPM mega-function for bootstrapping lambda
#'
#' This fits vital rate models, builds an IPM, iterates it, then returns lambda.
#' No intermediate steps are saved, so this is intended for use in bootstrapping
#' confidence intervals around lambda.
#'
#' @param boots a bootstrap rset object produced by `rsample::bootstraps()`
#' @param vit_other a list containing vit_fruits, vit_seeds, and vit_germ_est
#' @param habitat character, either "1-ha" or "CF"
#'
#' @return vector of lambdas
#' 
#' 
ipm_boot_det <- function(boots, vit_other, habitat = c("1-ha", "CF")) {
  splits <- boots[["splits"]] %>% map(rsample::analysis)
  pb <- progress_bar$new(total = length(splits))
  
  map_dbl(splits, ~{
    pb$tick() #advance progress bar
    
    vit_list_det_ff <- c(list(
      vit_surv = surv_det(.x, habitat = habitat),
      vit_size = size_det(.x, habitat = habitat),
      vit_flwr = flwr_det(.x, habitat = habitat),
      vit_size_sdlg = size_sdlg_det(.x, habitat = habitat),
      vit_surv_sdlg = surv_sdlg_det(.x, habitat = habitat)
    ), vit_other)
    
    make_proto_ipm_det(vit_list_det_ff) %>% 
      make_ipm(iterations = 100,  #only needs 100 to converge
               normalize_pop_size = FALSE, # to run as PVA
               usr_funs = list(get_scat_params = get_scat_params)
               ) %>% 
      ipmr::lambda(log = FALSE)
  })
}



