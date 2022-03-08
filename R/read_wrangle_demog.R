read_wrangle_demog <- function(path) {
  ha_raw <- read_csv(here::here("data", "Ha_survey_pre_submission.csv"), 
                     col_types = cols(notes = col_character()))
  #clean colnames
  ha_raw %>% 
    clean_names() %>% 
    #remove unused cols
    select(-tag_number, -row, -column, -x_09, -y_09, -duplicate_tag, -plot_id, -survey_status, -treefall_status, -adult_recruit) %>% 
    #create logical flowering column
    mutate(flwr = !is.na(infl) & infl > 0, .after = infl) %>% 
    #create logical seedling column
    mutate(sdlg = !is.na(sdlg_status), .before = code) %>% 
    select(-sdlg_status) %>% 
    #remove plants with 0 shoots or height (categorically different than small plant)
    filter(ht > 0 | is.na(ht), shts > 0 | is.na(shts)) %>% 
    #calculate log(size)
    mutate(log_size = log(shts * ht), .after = ht) %>% 
    #try to separate true NAs for flwr from what should be 0s
    mutate(flwr = if_else(is.na(log_size), NA, flwr)) %>% 
    #create column for survival
    group_by(ha_id_number) %>% 
    mutate(surv = as_living(ht, n = 3), .after = sdlg) %>% 
    mutate(surv = if_else(!is.na(code) & str_detect(code, "dead"), FALSE, surv)) %>% 
    filter(surv | lag(surv)) %>%  #remove entries after last year alive.
    filter(pmin(cumsum(!is.na(shts))) != 0) %>%  #remove leading NAs.
    ungroup() %>% 
    
    #create some columns with info about the previous timestep
    group_by(ha_id_number) %>% 
    mutate(log_size_prev = lag(log_size), .after = log_size) %>% 
    mutate(sdlg_prev = case_when(
      lag(sdlg) ~ TRUE, #if seedling prev timestep, use TRUE
      sdlg ~ NA, #if current seedling, use NA
      !lag(sdlg) ~ FALSE #otehrwise use FALSE
    ), .after = sdlg) %>% 
    ungroup() %>% 
    #set column types
    mutate(across(c("habitat", "ranch", "plot", "ha_id_number"), factor)) %>% 
    mutate(year_fct = factor(year), .after = year) %>% 
    #not using 10-ha data
    filter(habitat %in% c("CF", "1-ha"))
  # head(ha)
}