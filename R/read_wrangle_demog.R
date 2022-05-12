#' Read and wrangle Heliconia acuminata demographic dataset
#' 
#' Reads in the Dryad-archived demographic dataset and does some wrangling: 1)
#' creates a new binary column for flowering (from nubmer of infloresences), 2)
#' creates `log_size` variable (`log(ht*shts)`), 3) creates a column for
#' survival assuming that plants that haven't been found for 3 years were dead
#' the last time they were seen, 4) does some filtering and selecting to get
#' just the data we need.
#'
#' @param file_demog the demographic dataset
#' @param file_plots the dataset with plot information
#'
#' @return a tibble
#' 
read_wrangle_demog <- function(file_demog, file_plots) {
  ha_raw <- read_csv(file_demog)
  ha_plots <- read_csv(file_plots)
  ha_raw <- full_join(ha_plots, ha_raw, by = "plot")
  #clean colnames
  ha_raw %>% 
    #remove unused cols
    select(
      -ranch,
      -bdffp_no,
      -yr_isolated,
      -subplot,
      -check_pre_dryad,
      -found_without_tag,
      -treefall_status,
      -condition
    ) %>%
    #create logical flowering column
    mutate(flwr = !is.na(infl) & infl > 0, .after = infl) %>% 
    #rename logical seedling column
    rename(sdlg = recorded_sdlg) %>% 
    #remove plants with 0 shoots or height (categorically different than small plant)
    filter(ht > 0 | is.na(ht), shts > 0 | is.na(shts)) %>% 
    #calculate log(size)
    mutate(log_size = log(shts * ht), .after = ht) %>% 
    #create column for survival
    group_by(plant_id) %>% 
    mutate(surv = as_living(log_size, n = 3), .after = sdlg) %>% 
    mutate(surv = if_else(census_status == "dead", FALSE, surv)) %>% 
    filter(surv | dplyr::lag(surv)) %>%  #remove entries after last year alive.
    filter(pmin(cumsum(!is.na(log_size))) != 0) %>%  #remove leading NAs.
    ungroup() %>% 
    
    #create some columns with info about the previous timestep
    group_by(plant_id) %>% 
    mutate(log_size_prev = dplyr::lag(log_size), .after = log_size) %>% 
    mutate(sdlg_prev = case_when(
      dplyr::lag(sdlg) ~ TRUE, #if seedling prev timestep, use TRUE
      sdlg ~ NA, #if current seedling, use NA
      !dplyr::lag(sdlg) ~ FALSE #otherwise use FALSE
    ), .after = sdlg) %>% 
    ungroup() %>% 
    mutate(year_fct = factor(year), .after = year) %>% 
    #not using 10-ha data
    filter(habitat %in% c("forest", "one")) %>% 
    #rename habitat types
    mutate(habitat = str_replace_all(habitat, c("forest" = "CF", "one" = "1-ha"))) %>% 
    #set column types
    mutate(across(c("plot", "habitat", "plant_id"), factor))
}