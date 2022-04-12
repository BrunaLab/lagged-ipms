#' Deterministic IPM mega-function for bootstrapping lambda
#'
#' This fits vital rate models, builds an IPM, iterates it, then returns lambda.
#' No intermediate steps are saved, so this is intended for use in bootstrapping
#' confidence intervals around lambda.
#'
#' @param boots a bootstrap rset object produced by `rsample::bootstraps()`
#' @param habitat character, either "1-ha" or "CF"
#'
#' @return vector of lambdas
#' 
#' 
ipm_boot_det <- function(boots, habitat = c("1-ha", "CF")) {
  splits <- boots[["splits"]] %>% map(rsample::analysis)
  pb <- progress_bar$new(total = length(splits))
  map_dbl(splits, ~{
    pb$tick()
    vit_list_det_ff <- list(
      vit_surv = surv_det(.x, habitat = habitat),
      vit_size = size_det(.x, habitat = habitat),
      vit_flwr = flwr_det(.x, habitat = habitat),
      vit_fruits = fruits_gam(data_1998), #TODO: these don't need to be re-fit for every bootstrap maybe?
      vit_seeds = seeds_gam(data_1998, data_2008),
      vit_germ_est = 0.018921527, #germination and establishment
      vit_size_sdlg = size_sdlg_det(.x, habitat = habitat),
      vit_surv_sdlg = surv_sdlg_det(.x, habitat = habitat)
    )
    
    make_proto_ipm_det(vit_list_det_ff) %>% 
      make_ipm(iterations = 100,  #only needs 100 to converge
               normalize_pop_size = FALSE, # to run as PVA
               usr_funs = list(get_scat_params = get_scat_params)
               ) %>% 
      ipmr::lambda(log = FALSE)
  })
}



