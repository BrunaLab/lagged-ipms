library(tidyverse)
library(janitor)
library(here)
library(SPEI)
library(tsibble)
library(lubridate)
library(tsModel)
source(here("R", "00-functions.R"))


# wrangle demography data -------------------------------------------------

ha_raw <- read_csv(here::here("data", "Ha_survey_pre_submission.csv"), 
                   col_types = cols(notes = col_character()))
#clean colnames
ha <- 
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


# wrangle climate data ----------------------------------------------------

xa_raw <- read_csv(here("data", "xavier_daily_0.25x0.25.csv"))

# Calculate SPEI
xa <-
xa_raw %>% 
  unite(latlon, lat, lon) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(latlon, year, month) %>% 
  #convert to monthly
  summarize(precip = sum(precip), eto = sum(eto)) %>% 
  mutate(date = make_date(year = year, month = month, day = 15),
         .after = year) %>% 
  group_by(latlon) %>% 
  mutate(spei = as.numeric(
    spei(
      ts(precip - eto, freq = 12, start = c(year(min(date)), month(min(date)))),
      scale = 3
    )$fitted
  )) %>% 
  
#create lags
  
  group_by(latlon) %>% 
  mutate(spei_history = tsModel::Lag(spei, 0:36),
         L = matrix(0:36, nrow = n(), ncol = 37, byrow = TRUE)) %>% 
  separate(latlon, into = c("lat", "lon"), sep = "_") %>% 
  #clean up 
  select(-month) %>% 
  ungroup()
# head(xa)


# Join demographic and climate data ---------------------------------------

#manually assign longitude values to plots

ha <- ha %>% 
  mutate(lon = case_when(
  plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
  plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
  TRUE ~ NA_character_
), .before = plot)

#filter climate data so only months with census (february)
xa_sub <- 
  xa %>% 
  filter(month(date) == 2) %>% 
  #remove burnin for SPEI
  rowwise() %>% 
  filter(!any(is.na(spei_history)))

#join and clean up
df_full <-
  left_join(ha, xa_sub, by = c("lon", "year")) %>% 
  select(-lon, -lat)

skimr::skim(df_full)

write_rds(df_full, here("data", "data_cleaned.rds"))
