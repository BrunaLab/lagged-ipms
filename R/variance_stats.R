variance_stats <- function() {
  
  path <- here("docs/figures/df_for_stats.csv")
  df_for_stats<-read_csv(path) %>%
    dplyr::filter(IPM!="Deterministic")
  
  seedling_cf <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'seedling') %>%
    dplyr::filter(Habitat=="CF")
  
  seedling_ff <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'seedling') %>%
    dplyr::filter(Habitat=="FF")
  
  pre1_cf <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'pre-reproductive1') %>%
    dplyr::filter(Habitat=="CF")
  
  pre1_ff <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'pre-reproductive1') %>%
    dplyr::filter(Habitat=="FF")
  
  pre2_cf <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'pre-reproductive2') %>%
    dplyr::filter(Habitat=="CF")
  
  pre2_ff <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'pre-reproductive2') %>%
    dplyr::filter(Habitat=="FF")
  
  rep1_cf <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'reproductive1') %>%
    dplyr::filter(Habitat=="CF")
  
  rep1_ff <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'reproductive1') %>%
    dplyr::filter(Habitat=="FF")
  
  rep2_cf <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'reproductive2') %>%
    dplyr::filter(Habitat=="CF")
  
  rep2_ff <- df_for_stats %>%
    dplyr::filter(log_size_bin== 'reproductive2') %>%
    dplyr::filter(Habitat=="FF")
  
  # seedling_cf
  # seedling_ff
  # pre1_cf
  # pre1_ff
  # pre2_cf
  # pre2_ff
  # rep1_cf
  # rep1_ff
  # rep2_cf
  # rep2_ff
  
  #
  
  #
  
  
  
  
  # Create a list of datasets
  
  datasets <- list(seedling_cf,
                   seedling_ff,
                   pre1_cf,
                   pre1_ff,
                   pre2_cf,
                   pre2_ff,
                   rep1_cf,
                   rep1_ff,
                   rep2_cf,
                   rep2_ff)
  
  comparison <- c("seedling, CF",
                  "seedling, FF",
                  "Pre-reproductive 1, CF",
                  "Pre-reproductive 1, FF",
                  "Pre-reproductive 2, CF",
                  "Pre-reproductive 2, FF",
                  "Reproductive 1, CF",
                  "Reproductive 1, FF",
                  "Reproductive 2, CF",
                  "Reproductive 2, FF")
  
  # Apply the test to each dataset
  
  results <- map(datasets, ~ vartest::ansari.test(prop_plants ~ IPM, data = .x))
  
  # qqnorm(my_data2$prop_plants)
  
  # qqline(my_data2$prop_plants)
  
  # View the results
  
  comparison <- comparison
  p_values <- map_dbl(results, "p.value")
  statistic <- map_dbl(results, "statistic")
  parameter <- map_dbl(results, "parameter")
  
  stats_table<-tibble(
    "comparison"=comparison,
    "Statistic"=statistic,
    "df"=parameter,
    "p_values"=p_values,
  ) %>%
    mutate(p_values=round(p_values,6)) %>%
    mutate(p_values=as.character(p_values)) %>%
    mutate(Statistic=round(Statistic,2)) %>%
    separate_wider_delim(
      comparison,
      delim = ", ",
      names = c("Vital Rate","Habitat")) %>%
    mutate(p_values=as.numeric(p_values)) %>% 
    mutate(p_values=round(p_values,3)) %>% 
    mutate(p_values=if_else(p_values<0.0009,0.0001,p_values)) %>%
    mutate(p_values=as.character(p_values)) %>%
    mutate(p_values=if_else(p_values=="1e-04","< 0.0001",p_values)) %>%
    # mutate(p_values = if_else(p_values==0.0001,paste("\< ", p_values, sep=""),p_values)) %>%
    mutate(p_values=if_else(p_values<0.05,paste(p_values,"*",sep=""),p_values)) %>%
    rename(`p value` = p_values)

write_csv(stats_table,here("docs","figures","varstats_table.csv"))

df_for_stats<-df_for_stats %>% 
mutate(log_size_bin=factor(log_size_bin,levels = c(
  "seedling", "pre-reproductive1","pre-reproductive2", "reproductive1","reproductive2"), ordered = TRUE))


means_vars<-df_for_stats %>% 
  # mutate(prop_plants=prop_plants*100) %>% 
  group_by(IPM,Habitat,log_size_bin) %>% 
  summarize(mean=mean(prop_plants),
            median=median(prop_plants), 
            variance=var(prop_plants),
            n=n_distinct(iteration)) %>% 
  arrange(log_size_bin,IPM,Habitat)

means_vars_CF<-means_vars %>%
  dplyr::filter(Habitat=="CF") %>% 
  rename(medianCF=median,
         meanCF=mean,
         varCF=variance) %>% 
  ungroup() %>% 
  select(-Habitat)
means_vars_FF<-means_vars %>%
  dplyr::filter(Habitat=="FF") %>% 
  rename(medianFF=median,
         meanFF=mean,
         varFF=variance) %>% 
  ungroup() %>% 
  select(-Habitat)

means_vars<-left_join(means_vars_CF,means_vars_FF) %>% 
  relocate(log_size_bin,.before=1) %>% 
  relocate(medianCF,.after="IPM") %>% 
  relocate(medianFF,.after="medianCF") %>% 
  relocate(meanFF,.after="meanCF") %>% 
  relocate(n,.after="varFF") %>% 
  mutate(across(medianCF:meanFF,\(x) round(x,4))) %>% 
  mutate(across(varCF:varFF,\(x) round(x,6))) 
write_csv(means_vars,here("docs","figures","meanvar_table.csv"))

}
