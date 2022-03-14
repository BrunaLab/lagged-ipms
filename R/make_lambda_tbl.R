make_lambda_tbl <- function(...) {
  all_ipms <- rlang::list2(...)
  ipm_names <- rlang::ensyms(...)
  names(all_ipms) <- ipm_names
  
  all_ipms %>% 
    map_df(~ipmr::lambda(.x, log = FALSE), .id = "ipm") %>% 
    pivot_longer(everything(), names_to = "ipm", names_prefix = "ipm_", values_to = "lambda") %>% 
    separate(ipm, into = c("IPM", "habitat")) %>% 
    mutate(habitat = toupper(habitat)) %>% 
    pivot_wider(names_from = habitat, values_from = lambda) 
  
}
