make_lambda_table <- function(ipm_list, bt_list) {

#not very reusable, but that's fine.
#ipm_list should look like this:
  # ipm_list <- list(
  #   det_ff = ipm_det_ff,
  #   det_cf = ipm_det_cf,
  #   stoch_ff = ipm_stoch_ff,
  #   stoch_cf = ipm_stoch_cf
  # )
#bt_list should look like this:
  # bt_list <- list(
  #   det_ff = lambda_bt_det_ff,
  #   det_cf = lambda_bt_det_cf,
  #   stoch_ff = lambda_bt_stoch_ff,
  #   stoch_cf = lambda_bt_stoch_cf
  #   # dlnm_ff = lambda_bt_dlnm_ff,
  #   # dlnm_cf = lambda_bt_dlnm_cf
  # )

  
 lambdas <- 
   ipm_list %>% 
   map_df(~lambda(.x, log = FALSE) %>% set_names("lambda"), .id = "model")
  
  calc_ci <- function(x, alpha = 0.05) {
    mu <- mean(x)
    ci <- quantile(x, c(0 + (alpha/2), 1 - (alpha/2)))
    tibble(mean = mu, lower = ci[1], upper = ci[2])
  }
  
lambdas_bt <-
  bt_list %>%
  map_df(calc_ci, .id = "model") #TODO move calc_ci here

full_join(lambdas, lambdas_bt, by = "model") %>%
  separate(model, into = c("ipm", "habitat"))

}