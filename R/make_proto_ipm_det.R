# library(ipmr)
make_proto_ipm_det <- function(data_list, pop_vec) {
  
  init_ipm(
    sim_gen = "general",
    di_dd = "di",
    det_stoch = "det"
  ) %>% 
    
    # Define growth and survival kernel
    define_kernel(
      "P",
      formula = s_z * G_z1z * d_log_size,
      family  = "CC",
      # Probability of survival
      s_z = predict(vit_surv,
                    newdata = tibble(log_size_prev = log_size_1),
                    type = 'response'),
      # Growth
      G_z1z = dt.scaled(
        log_size_2,
        df   = size_nu,
        mean = size_mu,
        sd   = size_sd
      ),
      size_mu = predict(vit_size,
                        newdata = tibble(log_size_prev = log_size_1),
                        type = 'response'),
      size_nu = get_scat_params(vit_size)["nu"],
      size_sd = get_scat_params(vit_size)["sd"],
      
      data_list = data_list,
      states        = list(c('log_size')),
      uses_par_sets = FALSE,
      evict_cor     = TRUE,
      evict_fun     = truncated_distributions("t.scaled", "G_z1z")
    ) %>% 
    
    # Define fecundity and recruitment kernel
    define_kernel(
      "go_sdlg",
      formula = p_f * fruits * seeds * g_e * d_log_size,
      family  = "CD",
      # probability of flowering
      p_f = predict(vit_flwr,
                    newdata = tibble(log_size_prev = log_size_1),
                    type = 'response'),
      fruits = predict(vit_fruits,
                       newdata = tibble(log_size_prev = log_size_1),
                       type = 'response'), 
      seeds = exp(coef(vit_seeds)), #mean seeds per fruit
      g_e = vit_germ_est,
      
      data_list     = data_list,
      states        = list(c('log_size', 'sdlg')),
      uses_par_sets = FALSE
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
      name = "leave_sdlg",
      formula = s_sdlg * G_z2_sdlg,
      family = "DC",
      s_sdlg = plogis(coef(vit_surv_sdlg)[1]), #intercept from intercept-only glm
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
      uses_par_sets = FALSE,
      evict_cor = TRUE,
      evict_fun = truncated_distributions("t.scaled", "G_z2_sdlg")
    ) %>% 
    
    # define implementation with midpoint rule
    define_impl(make_impl_args_list(
      kernel_names = c("P",        "go_sdlg",  "stay_sdlg", "leave_sdlg"),
      int_rule     = c("midpoint", "midpoint", "midpoint",  "midpoint"),
      state_start  = c("log_size", "log_size", "sdlg",      "sdlg"),
      state_end    = c("log_size", "sdlg",     "sdlg",      "log_size")
    )) %>% 
    #define lower bound, upper bound, and number of meshpoints for log_size
    define_domains(log_size = c(0, 8.018296, 100)) %>%  
    #arbitrary starting population state
    define_pop_state(pop_vectors = pop_vec)
  
}