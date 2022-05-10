#' Make model competition table data frame
#' 
#' Takes lists of vital rates (e.g. vit_list_det_ff, vit_list_dlnm_cf, etc.), extracts the vital rates models contained within, and returns a table of AIC values.
#'
#' @param ... targets following the pattern `vit_list_<det/stoch/dlnm>_<ff/cf>`
#'
#' @return a tibble
#' 
make_AIC_tbl <- function(...) {
  all_mods <- rlang::list2(...)
  mod_names <- rlang::ensyms(...) %>% as.character() %>% str_remove_all("vit_list_")
  names(all_mods) <- mod_names
  all_mods
  
  to_pull <- c("vit_surv", "vit_size", "vit_flwr", "vit_size_sdlg", "vit_surv_sdlg")
  
  all_mods %>% 
    map_df(~.x[to_pull] %>%
             map_df(glance, .id = "model"), .id = "ipm") %>% 
    separate(ipm, into = c("env_effect_incl", "habitat")) %>% 
    mutate(model = str_remove(model, "vit_")) %>% 
    group_by(habitat, model) %>% 
    mutate(dAIC = AIC- min(AIC)) %>% 
    select(model, habitat, env_effect_incl, df, AIC, dAIC) %>% 
    mutate(env_effect_incl = fct_recode(
      env_effect_incl,
      "DLNM" = "dlnm",
      "random effect of year" = "stoch",
      "none" = "det",
    )) %>% 
    arrange(habitat, model, dAIC)
  
}

# make_AIC_table(vit_list_det_cf,
#                vit_list_det_ff,
#                vit_list_dlnm_cf,
#                vit_list_dlnm_ff,
#                vit_list_stoch_cf,
#                vit_list_stoch_ff)
