# library(mgcv)
# library(gratia)
# library(tidyverse)
# 
# m1 <- gam(ripe_fruits ~ s(log_size_prev, bs = "cr"),
#           family = poisson(link = "log"),
#           method = "REML",
#           data = data_1998)
# m2 <- gam(ripe_fruits ~ s(log_size_prev, bs = "cr"),
#           family = nb(link = "log"),
#           method = "REML",
#           data = data_1998)
# m3 <- gam(ripe_fruits ~ s(log_size_prev, bs = "cr"),
#           family = ziP,
#           method = "REML",
#           data = data_1998)
# AIC(m1, m2, m3) #negbin wins
# appraise(m1)
# appraise(m2)
# appraise(m3)
# 
# k.check(m1)
# k.check(m2)
# k.check(m3)
# 
# predict(m1, newdata = tibble(log_size_prev = 4), type = "response")
# predict(m2, newdata = tibble(log_size_prev = 4), type = "response")
# predict(m3, newdata = tibble(log_size_prev = 4), type = "response")
# 
# draw(m1, residuals = TRUE)
# draw(m2, residuals = TRUE)
# draw(m3, residuals = TRUE)
# summary(m1)
# summary(m2)
# summary(m3)


fruits_gam <- function(data_1998) {
  gam(ripe_fruits ~ s(log_size_prev, bs = "cr"),
      family = nb,
      method = "REML",
      data = data_1998)
}

seeds_gam <- function(data_1998, data_2008) {
  #combine data on seed number, retaining number of fruits
  seeds_df <- 
    bind_rows(
      data_2008 %>% 
        select(seeds = No_of_seeds) %>% 
        add_column(fruits = 1),
      
      data_1998 %>% 
        select(seeds = sds_collected,
               fruits = ripe_fruits)
    ) %>% 
    filter(!is.na(seeds)) %>% 
    filter(!(fruits==0 & seeds>0)) #these are obvious data entry errors
  #model as intercept-only with offset of log(fruits) to get mean # seeds/fruit with predict()
  #note, seeds/fruits probably closer to uniform distribution than poisson, but straight mean is not that different.
  gam(seeds ~ 1,
      offset = log(fruits),
      family = poisson,
      method = "REML",
      data = seeds_df)
}


