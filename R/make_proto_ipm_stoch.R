library(ipmr)
make_proto_ipm_stoch <- function(data_list) {
  
  init_ipm(
    sim_gen = "general",
    di_dd = "di",
    det_stoch = "stoch",
    kern_param = "kern"
  ) %>% 
    
    # Define growth and survival kernel
    define_kernel(
      "P_yr", #_yr makes a different P kernel for each value of `yr` from par_set_indices
      formula = s_z * G_z1z * d_log_size,
      family  = "CC",
      # Probability of survival
      s_z = predict(vit_surv,
                    newdata = tibble(log_size_prev = log_size_1, year_fct = yr),
                    type = 'response'),
      # Growth
      G_z1z = dt.scaled(
        log_size_2,
        df   = size_nu,
        mean = size_mu,
        sd   = size_sd
      ),
      size_mu = predict(vit_size,
                        newdata = tibble(log_size_prev = log_size_1, year_fct = yr),
                        type = 'response'),
      size_nu = get_scat_params(vit_size)["nu"],
      size_sd = get_scat_params(vit_size)["sd"],
      
      data_list = data_list,
      states          = list(c('log_size')),
      uses_par_sets   = TRUE,
      par_set_indices = list(yr = 2000:2009),
      evict_cor       = TRUE,
      evict_fun       = truncated_distributions("t.scaled", "G_z1z")
    ) %>% 
    
    # Define fecundity and recruitment kernel
    define_kernel(
      "go_sdlg_yr",
      formula = p_f * fruits * seeds * g_e * d_log_size,
      family  = "CD",
      # probability of flowering
      p_f = predict(vit_flwr,
                    newdata = tibble(log_size_prev = log_size_1, year_fct = yr),
                    type = 'response'),
      fruits = predict(vit_fruits,
                       newdata = tibble(log_size_prev = log_size_1),
                       type = 'response'), 
      seeds = exp(coef(vit_seeds)[1]), #mean seeds per fruit
      g_e = vit_germ_est,
      
      data_list     = data_list,
      states        = list(c('log_size', 'sdlg')),
      uses_par_sets = TRUE,
      par_set_indices = list(yr = 2000:2009)
    ) %>% 
    
    define_kernel(
      name = "stay_sdlg",
      formula = 0,
      family = "DD",
      states = list(c("sdlg")),
      evict_cor = FALSE
    ) %>% 
    
    # Enter into post-seedling
    define_kernel(
      name = "leave_sdlg_yr",
      formula = s_sdlg * G_z2_sdlg,
      family = "DC",
      s_sdlg = predict(vit_surv_sdlg,
                       newdata = tibble(year_fct = yr),
                       type = 'response'), #intercept from intercept-only glm
      G_z2_sdlg = dt.scaled(
        x    = log_size_2,
        df   = size_sdlg_nu,
        mean = size_sdlg_mu,
        sd   = size_sdlg_sd
      ),
      size_sdlg_mu = coef(vit_size_sdlg)[1], #intercept from intercept-only glm
      size_sdlg_nu = get_scat_params(vit_size_sdlg)["nu"],
      size_sdlg_sd = get_scat_params(vit_size_sdlg)["sd"],
      
      data_list = data_list,
      states = list(c("sdlg", "log_size")),
      uses_par_sets = TRUE,
      par_set_indices = list(yr = 2000:2009),
      evict_cor = TRUE,
      evict_fun = truncated_distributions("t.scaled", "G_z2_sdlg")
    ) %>% 
    
    # define implementation with midpoint rule
    define_impl(make_impl_args_list(
      kernel_names = c("P_yr",        "go_sdlg_yr", "stay_sdlg", "leave_sdlg_yr"),
      int_rule = "midpoint",
      state_start  = c("log_size", "log_size", "sdlg",     "sdlg"),
      state_end    = c("log_size", "sdlg",     "sdlg",     "log_size")
    )) %>% 
    #define lower bound, upper bound, and number of meshpoints for log_size
    define_domains(log_size = c(0, 8.018296, 100)) %>%  
    #arbitrary starting population state
    define_pop_state(n_log_size = runif(100), n_sdlg = .1)
  
}