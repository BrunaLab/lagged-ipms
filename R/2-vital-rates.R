#' #' Here I'll fit the vital rates models for deterministic, kernel resampled, and parameter resampled IPM
#' #' 
#' #' 
#' library(tidyverse)
#' library(mgcv)
#' library(gratia)
#' library(broom)
#' 
#' 
#' # read and separate data --------------------------------------------------
#' 
#' ha <- read_rds("data/data_cleaned.rds")
#' ha_ff <- ha %>% filter(sdlg_prev == FALSE, habitat == "1-ha") 
#' ha_cf <- ha %>% filter(sdlg_prev == FALSE, habitat == "CF")
#' ha_sdlg_ff <- ha %>% filter(sdlg_prev == TRUE, habitat == "1-ha")
#' ha_sdlg_cf <- ha %>% filter(sdlg_prev == TRUE, habitat == "CF")
#' 
#' 
#' # Simple -----------------------------------------------------------
#' 
#' ## Survival --------------------------------------------------------------
#' 
#' 
#' surv_det_ff <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15),
#'   family = binomial,
#'   data = ha_ff,
#'   method = "REML"
#' )
#' # appraise(surv_det_ff, method = "simulate")
#' # gam.check(surv_det_ff, k.rep = 999)
#' 
#' surv_det_cf <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15),
#'   family = binomial,
#'   data = ha_cf,
#'   method = "REML"
#' )
#' # appraise(surv_det_cf, method = "simulate")
#' # gam.check(surv_det_cf, k.rep = 999)
#' 
#' 
#' ## Size ------------------------------------------------------------------
#' 
#' size_det_ff <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 20),
#'   family = scat,
#'   data = ha_ff %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_det_ff)
#' # gam.check(size_det_ff, k.rep = 999) #no diff between k = 20 and k = 25
#' 
#' size_det_cf <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 20),
#'   family = scat,
#'   data = ha_cf %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_det_cf)
#' # gam.check(size_det_cf, k.rep = 999)
#' 
#' 
#' ## Flowering -------------------------------------------------------------
#' 
#' flwr_det_ff <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15),
#'   family = binomial,
#'   data = ha_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_det_ff, method = "simulate")
#' # gam.check(flwr_det_ff)
#' 
#' flwr_det_cf <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15),
#'   family = binomial,
#'   data = ha_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_det_cf, method = "simulate")
#' # gam.check(flwr_det_cf)
#' 
#' 
#' ## Seedling growth -------------------------------------------------------
#' 
#' size_sdlg_det_ff <- gam(
#'   log_size ~ 1,
#'   family = scat,
#'   data = ha_sdlg_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(size_sdlg_det_ff)
#' 
#' size_sdlg_det_cf <- gam(
#'   log_size ~ 1,
#'   family = scat,
#'   data = ha_sdlg_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(size_sdlg_det_ff)
#' 
#' 
#' ## Seedling survival -----------------------------------------------------
#' 
#' surv_sdlg_det_ff <- gam(
#'   surv ~ 1,
#'   family = binomial,
#'   data = ha_sdlg_ff,
#'   method = "REML"
#' )
#' plogis(coef(surv_sdlg_det_ff))
#' 
#' surv_sdlg_det_cf <- gam(
#'   surv ~ 1,
#'   family = binomial,
#'   data = ha_sdlg_cf,
#'   method = "REML"
#' )
#' plogis(coef(surv_sdlg_det_cf))
#' 
#' 
#' # Heirarchical -----------------------------------------------------------
#' 
#' ## Survival --------------------------------------------------------------
#' 
#' 
#' surv_raneff_ff <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15) + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_ff,
#'   method = "REML"
#' )
#' # appraise(surv_raneff_ff, method = "simulate")
#' # gam.check(surv_raneff_ff, k.rep = 999)
#' 
#' surv_raneff_cf <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15) + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_cf,
#'   method = "REML"
#' )
#' # appraise(surv_raneff_cf, method = "simulate")
#' # gam.check(surv_raneff_cf, k.rep = 999)
#' 
#' 
#' ## Size ------------------------------------------------------------------
#' 
#' size_raneff_ff <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 20) + s(year_fct, bs = "re"),
#'   family = scat,
#'   data = ha_ff %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_raneff_ff)
#' # gam.check(size_raneff_ff, k.rep = 999) #no diff between k = 20 and k = 25
#' 
#' size_raneff_cf <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 20) + s(year_fct, bs = "re"),
#'   family = scat,
#'   data = ha_cf %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_raneff_cf)
#' # gam.check(size_raneff_cf, k.rep = 999)
#' 
#' 
#' ## Flowering -------------------------------------------------------------
#' 
#' flwr_raneff_ff <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15) + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_raneff_ff, method = "simulate")
#' # gam.check(flwr_raneff_ff)
#' 
#' flwr_raneff_cf <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15) + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_raneff_cf, method = "simulate")
#' # gam.check(flwr_raneff_cf)
#' 
#' 
#' ## Seedling growth -------------------------------------------------------
#' 
#' size_sdlg_raneff_ff <- gam(
#'   log_size ~ 1 + s(year_fct, bs = "re"),
#'   family = scat,
#'   data = ha_sdlg_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(size_sdlg_raneff_ff)
#' 
#' size_sdlg_raneff_cf <- gam(
#'   log_size ~ 1 + s(year_fct, bs = "re"),
#'   family = scat,
#'   data = ha_sdlg_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(size_sdlg_raneff_ff)
#' 
#' 
#' ## Seedling survival -----------------------------------------------------
#' 
#' surv_sdlg_raneff_ff <- gam(
#'   surv ~ 1 + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_sdlg_ff,
#'   method = "REML"
#' )
#' plogis(coef(surv_sdlg_raneff_ff))
#' 
#' surv_sdlg_raneff_cf <- gam(
#'   surv ~ 1 + s(year_fct, bs = "re"),
#'   family = binomial,
#'   data = ha_sdlg_cf,
#'   method = "REML"
#' )
#' plogis(coef(surv_sdlg_raneff_cf))
#' 
#' 
#' # DLNMs -------------------------------------------------------------------
#' #TODO: check k for all these
#' ## Survival --------------------------------------------------------------
#' 
#' surv_dlnm_ff <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15) + 
#'     te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = binomial,
#'   data = ha_ff,
#'   method = "REML"
#' )
#' # appraise(surv_dlnm_ff, method = "simulate")
#' # gam.check(surv_dlnm_ff, k.rep = 999)
#' # draw(surv_dlnm_ff)
#' 
#' surv_dlnm_cf <- gam(
#'   surv ~ s(log_size_prev, bs = "cr", k = 15) + 
#'     te(spei_history, L, bs = "cr", k = c(12,12)),
#'   family = binomial,
#'   data = ha_cf,
#'   method = "REML"
#' )
#' # appraise(surv_dlnm_cf, method = "simulate")
#' # gam.check(surv_dlnm_cf, k.rep = 999)
#' 
#' 
#' ## Size ------------------------------------------------------------------
#' 
#' size_dlnm_ff <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 15) + 
#'     te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = scat,
#'   data = ha_ff %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_dlnm_ff)
#' # gam.check(size_dlnm_ff, k.rep = 999) 
#' 
#' size_dlnm_cf <- gam(
#'   log_size ~ s(log_size_prev, bs = "cr", k = 20) +
#'     te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = scat,
#'   data = ha_cf %>% filter(surv), #only surviving plants get to grow
#'   method = "REML"
#' )
#' # appraise(size_dlnm_cf)
#' # gam.check(size_dlnm_cf, k.rep = 999)
#' 
#' 
#' ## Flowering -------------------------------------------------------------
#' 
#' flwr_dlnm_ff <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15) + 
#'     te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = binomial,
#'   data = ha_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_dlnm_ff, method = "simulate")
#' # gam.check(flwr_dlnm_ff)
#' 
#' flwr_dlnm_cf <- gam(
#'   flwr ~ s(log_size_prev, bs = "cr", k = 15) +
#'     te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = binomial,
#'   data = ha_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # appraise(flwr_dlnm_cf, method = "simulate")
#' # gam.check(flwr_dlnm_cf)
#' 
#' 
#' ## Seedling growth -------------------------------------------------------
#' #TODO: decide whether to include or not.
#' size_sdlg_dlnm_ff <- gam(
#'   log_size ~ 1 + te(spei_history, L, bs = "cr", k = c(15,15)),
#'   family = scat,
#'   data = ha_sdlg_ff %>% filter(surv),
#'   method = "REML"
#' )
#' # edf(size_sdlg_dlnm_ff)
#' # draw(size_sdlg_dlnm_ff)
#' # appraise(size_sdlg_dlnm_ff)
#' 
#' size_sdlg_dlnm_cf <- gam(
#'   log_size ~ 1 + te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = scat,
#'   data = ha_sdlg_cf %>% filter(surv),
#'   method = "REML"
#' )
#' # edf(size_sdlg_dlnm_cf) #not different from k = 10
#' # draw(size_sdlg_dlnm_cf)
#' # appraise(size_sdlg_dlnm_ff)
#' 
#' 
#' ## Seedling survival -----------------------------------------------------
#' 
#' surv_sdlg_dlnm_ff <- gam(
#'   surv ~ 1 + te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = binomial,
#'   data = ha_sdlg_ff,
#'   method = "REML"
#' )
#' # edf(surv_sdlg_dlnm_ff) #no change with k = 15
#' 
#' surv_sdlg_dlnm_cf <- gam(
#'   surv ~ 1 + te(spei_history, L, bs = "cr", k = c(10,10)),
#'   family = binomial,
#'   data = ha_sdlg_cf,
#'   method = "REML"
#' )
#' # edf(surv_sdlg_dlnm_cf)
#' 
#' 
#' # AIC tables ---------------------------------------------------------------
#' 
#' list(
#'   size_det_cf = size_det_cf,
#'   size_det_ff = size_det_ff,
#'   surv_det_cf = surv_det_cf,
#'   surv_det_ff = surv_det_ff,
#'   flwr_det_cf = flwr_det_cf,
#'   flwr_det_ff = flwr_det_ff,
#'   size_raneff_cf = size_raneff_cf,
#'   size_raneff_ff = size_raneff_ff,
#'   surv_raneff_cf = surv_raneff_cf,
#'   surv_raneff_ff = surv_raneff_ff,
#'   flwr_raneff_cf = flwr_raneff_cf,
#'   flwr_raneff_ff = flwr_raneff_ff,
#'   size_dlnm_ff = size_dlnm_ff,
#'   size_dlnm_cf = size_dlnm_cf,
#'   surv_dlnm_cf = surv_dlnm_cf,
#'   surv_dlnm_ff = surv_dlnm_ff,
#'   flwr_dlnm_cf = flwr_dlnm_cf,
#'   flwr_dlnm_ff = flwr_dlnm_ff,
#'   size.sdlg_det_cf = size_sdlg_det_cf,
#'   size.sdlg_det_ff = size_sdlg_det_ff,
#'   surv.sdlg_det_cf = surv_sdlg_det_cf,
#'   surv.sldg_det_ff = surv_sdlg_det_ff,
#'   size.sdlg_raneff_cf = size_sdlg_raneff_cf,
#'   size.sdlg_raneff_ff = size_sdlg_raneff_ff,
#'   surv.sdlg_raneff_cf = surv_sdlg_raneff_cf,
#'   surv.sdlg_raneff_ff = surv_sdlg_raneff_ff,
#'   size.sdlg_dlnm_cf = size_sdlg_dlnm_cf,
#'   size.sdlg_dlnm_ff = size_sdlg_dlnm_ff,
#'   surv.sdlg_dlnm_cf = surv_sdlg_dlnm_cf,
#'   surv.sdlg_dlnm_ff = surv_sdlg_dlnm_ff
#' ) %>% 
#'   map_df(glance, .id = "model") %>% 
#'   separate(model, into = c("vital_rate", "type", "habitat"), sep = "_") %>% 
#'   arrange(vital_rate, habitat, type) %>% 
#'   group_by(vital_rate, habitat) %>% 
#'   mutate(dAIC = AIC - min(AIC)) %>% 
#'   select(vital_rate, type, habitat, df, dAIC)
