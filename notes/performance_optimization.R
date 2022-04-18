#performance optimization
library(targets)
library(tidyverse)
library(mgcv)
library(tictoc)
conflict_prefer("filter", "dplyr")
df <- tar_read(data_full) %>% filter(habitat == "CF", sdlg_prev == FALSE)

# gam(), REML
tic()
m1 <- 
  gam(
    surv ~ s(log_size_prev, bs = "cr", k = 10) +
      te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "REML"
  )
toc()
#152.428 sec elapsed

tic()
predict(m1)
toc()
# 10.741 sec elapsed

# bam(), fREML
tic()
m2 <- 
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 10) +
      te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "fREML"
  )
toc()
#99.156 sec elapsed

tic()
predict(m2)
toc()
mean(c(6.806, 6.685, 6.8))
# 6.763 sec elapsed

tic()
predict(m2, newdata.guaranteed = TRUE)
toc()
mean(c(6.456, 6.671, 6.851))
#6.659 sec elapsed

# gam(), REML, optimizer = "bfgs"
tic()
m3 <- 
  gam(
    surv ~ s(log_size_prev, bs = "cr", k = 10) +
      te(spei_history, L, bs = "cr", k = c(15,15)),
    family = binomial,
    data = df,
    method = "REML",
    optimizer = c("outer", "bfgs")
  )
toc()
#238.691 sec elapsed

tic()
predict(m3)
toc()
# 10.21 sec elapsed

# Do the benefits hold with a simpler model?

# gam(), REML
tic()
m4 <- 
  gam(
    surv ~ s(log_size_prev, bs = "cr", k = 10),
    family = binomial,
    data = df,
    method = "REML"
  )
toc()
mean(c(0.754, 0.542, 0.743, 0.599, 0.614))
# 0.65 sec elapsed

tic()
m5 <- 
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 10),
    family = binomial,
    data = df,
    method = "fREML"
  )
toc()
mean(c(0.708, 0.583, 0.375, 0.755, 0.41))
# 0.57 sec elapsed

# marginally faster.


#what about random effects?
tic()
m6 <- 
  gam(
    surv ~ s(log_size_prev, bs = "cr", k = 20) +
      s(year_fct, bs = "re"),
    family = binomial,
    data = df,
    method = "REML"
  )
toc()
mean(c(8.444, 8.003, 7.574))
#8.007 sec elapsed

tic()
m7 <- 
  bam(
    surv ~ s(log_size_prev, bs = "cr", k = 20) +
      s(year_fct, bs = "re"),
    family = binomial,
    data = df,
    method = "fREML"
  )
toc()
mean(c(1.489, 1.246, 1.131))
# 1.289 sec elapsed

#damn!



