library(targets)
library(ipmr)
library(purrr)
library(metRology)
library(here)
library(tibble)
source(here("R", "make_proto_ipm_stoch.R"))
source(here("R", "get_scat_params.R"))

tar_load(vit_list_stoch_ff)
proto_stoch <- make_proto_ipm_stoch(vit_list_stoch_ff)

make_ipm(proto_stoch,
         iterations = 10,
         kernel_seq = sample(2000:2009, 10, replace = TRUE),
         usr_funs = list(get_scat_params = get_scat_params))



# use ipmr vignette -------------------------------------------------------
library(ipmr)

# Define some fixed parameters

fixed_list <- list(
  s_int     = 1.03,   # fixef(my_surv_mod)[1] - uses fixef because we now have a model with random effects
  s_slope   = 2.2,    # fixef(my_surv_mod)[2]
  g_int     = 3.7,    # fixef(my_grow_mod)[1]
  g_slope   = 0.92,   # fixef(my_grow_mod)[2]
  sd_g      = 0.9,    # sd(resid(my_grow_mod))
  r_r_int   = 0.09,   # coef(my_repro_mod)[1] - uses coef because there are no random effects in this model
  r_r_slope = 0.05,   # coef(my_repro_mod)[2]
  r_s_int   = 0.1,    # fixef(my_flower_mod)[1]
  r_s_slope = 0.005,  # fixef(my_flower_mod)[2]
  mu_fd     = 9,      # mean(my_recr_data$size_2, na.rm = TRUE)
  sd_fd     = 2       # sd(my_recr_data$size_2, na.rm = TRUE)
)
# Now, simulate some random intercepts for growth (g_), survival (s_), 
# and offspring production (r_s_). This part is for the purpose of the example.

# First, we create vector of values corresponding to 

g_r_int   <- rnorm(5, 0, 0.3) # unlist(ranef(my_grow_mod)) for an lme4 output
s_r_int   <- rnorm(5, 0, 0.7) # unlist(ranef(my_surv_mod)) for an lme4 output
r_s_r_int <- rnorm(5, 0, 0.2) # unlist(ranef(my_flower_mod)) for an lme4 output

nms <- paste("r_", 1:5, sep = "")

names(g_r_int)   <- paste('g_', nms, sep = "")
names(s_r_int)   <- paste('s_', nms, sep = "")
names(r_s_r_int) <- paste('r_s_', nms, sep = "")

# Each set of parameters is converted to a named list. The names should match
# the variables referenced in each define_kernel() call.

g_params   <- as.list(g_r_int)
s_params   <- as.list(s_r_int)
r_s_params <- as.list(r_s_r_int)

# add them all together using c()

all_params_list <- c(fixed_list, g_params, s_params, r_s_params)

inv_logit <- function(sv, int, slope) {
  return(
    1/(1 + exp(-(int + slope * sv)))
  )
}

# same as above, but handles the extra term from the random effect we simulated.

inv_logit_r <- function(sv, int, slope, r_eff) {
  return(
    1/(1 + exp(-(int + slope * sv + r_eff)))
  )
}

pois_r <- function(sv, int, slope, r_eff) {
  return(
    exp(
      int + slope * sv + r_eff
    )
  )
}

my_funs <- list(inv_logit   = inv_logit,
                inv_logit_r = inv_logit_r,
                pois_r      = pois_r)


my_ipm <- init_ipm(sim_gen    = "simple",
                   di_dd      = "di", 
                   det_stoch  = "stoch",
                   kern_param = "kern") %>%
  define_kernel(
    name             = 'P_yr',         # P becomes P_yr
    formula          = s_yr * g_yr,    # g and s become g_yr and s_yr, respectively
    family           = "CC",
    
    # Note the usage of the inv_logit_r, which we defined in the block above.
    # it is passed to make_ipm() 
    
    s_yr             = inv_logit_r(ht_1, s_int, s_slope, s_r_yr), 
    g_yr             = dnorm(ht_2, mean = mu_g_yr, sd = sd_g),
    mu_g_yr          = g_int + g_slope * ht_1 + g_r_yr,
    
    # all_params_list contains the named parameters g_r_1, g_r_2, s_r_1, s_r_2, etc.
    # This is the only level where the user is required to fully expand the name
    # X par_set_indices combinations. 
    
    data_list        = all_params_list,
    states           = list(c('ht')),
    uses_par_sets    = TRUE,
    par_set_indices  = list(yr = 1:5),
    evict_cor        = TRUE,
    
    # reference to g_yr in evict_fun is also updated
    
    evict_fun        = truncated_distributions("norm", "g_yr")
    
  ) %>%
  define_kernel(
    name             = "F_yr",             # Update the names as we did for the P kernel
    formula          = r_r * r_s_yr * r_d,
    family           = "CC",
    r_r              = inv_logit(ht_1, r_r_int, r_r_slope),
    r_s_yr           = pois_r(ht_1, r_s_int, r_s_slope, r_s_r_yr),
    r_d              = dnorm(ht_2, mean = mu_fd, sd = sd_fd),
    data_list        = all_params_list,
    states           = list(c('ht')),
    uses_par_sets    = TRUE,
    par_set_indices = list(yr = 1:5),
    evict_cor        = TRUE,
    evict_fun        = truncated_distributions("norm", "r_d")
  ) %>%
  define_impl(
    make_impl_args_list(
      kernel_names = c("P_yr", "F_yr"),
      int_rule     = rep("midpoint", 2),
      state_start    = rep("ht", 2),
      state_end      = rep("ht", 2)
    )
  ) %>%
  define_domains(ht = c(0.2, 40, 100)) %>%
  define_pop_state(n_ht = runif(100)) %>% 
  make_ipm(usr_funs   = my_funs,
           kernel_seq = sample(1:5, 100, replace = TRUE),
           iterate    = TRUE,
           iterations = 100)


# The default for stochastic lambda is to compute mean(log(ipm$pop_state$lambda)).
# It also removes the first 10% of iterations to handle "burn in". The amount
# removed can be adjusted with the burn_in parameter.

stoch_lambda <- lambda(my_ipm)

# lambda(comp_method = 'pop_size', type = 'all') will compute the population 
# growth rate for every time step as the sum(n_ht_t_1) / sum(n_ht_t).

iteration_lambdas <- lambda(my_ipm, type_lambda = 'all')


debug(make_ipm)
