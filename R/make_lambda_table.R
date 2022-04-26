make_lambda_table <- function(ipm_list, bt_list, alpha = 0.05) {

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
   map(~lambda(.x, log = FALSE))
  
 #calc bias-corrected percentile CIs
  map2_df(lambdas, bt_list, ~bcpi(t0 = .x, t = .y, alpha = alpha), .id = "model") %>%
  separate(model, into = c("ipm", "habitat"))

}
