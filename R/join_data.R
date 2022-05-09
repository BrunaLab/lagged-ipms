join_data <- function(demog, clim) {
  demog <- demog %>% 
    mutate(lon = case_when(
      # plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
      # plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
      plot %in% c("CF-1", "FF-3", "CF-2", "CF-3", "CF-6") ~ "-59.875",
      plot %in% c("FF-5", "FF-2", "FF-1", "CF-4", "FF-4", "CF-5") ~ "-60.125",
      TRUE ~ NA_character_
    ), .before = plot)
  
  #filter climate data so only months with census (february)
  clim_sub <- 
    clim %>% 
    filter(month == 2) %>% 
    #remove burnin for SPEI
    rowwise() %>% 
    filter(!any(is.na(spei_history)))
  
  #join and clean up
  df_full <-
    left_join(demog, clim_sub, by = c("lon", "year")) %>% 
    select(-lon, -lat)
  #return:
  df_full
}
