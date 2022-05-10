#' Plot population stage structure over time
#'
#' For a stochastic Heliconia IPM, plots size distribution changes over
#' iterations as a binned area plot.
#'
#' @param ipm an iterated ipmr ipm
#' @param bins number of bins to group continuous log_size into (not including
#'   seedlings)
#' @param normalize logical; TRUE plots proportions while FALSE (default) plots
#'   actual numbers
#'
plot_pop_states <- function(ipm, bins = 6, normalize = FALSE) {
  meshpts <- int_mesh(ipm, full_mesh = FALSE)$log_size_1
  
  pop_states <- pop_state(ipm)
  rownames(pop_states$n_log_size) <-  meshpts
  
  post_sdlg_df <-
    as_tibble(pop_states$n_log_size,
              .name_repair = "unique",
              rownames = "log_size") %>%
    mutate(log_size = as.numeric(log_size)) %>% 
    pivot_longer(
      -log_size,
      names_to = "iteration", #TODO: need to account for burnin
      names_transform = list(iteration = ~factor(as.numeric(.))),
      values_to = "n_plants",
      names_prefix = "...")
  
  #bin sizes for easier plotting
  log_size_range <- as.numeric(ipmr::domains(ipm)$log_size[1:2])
  bin_width <- diff(log_size_range) / (bins - 1)
  post_sdlg_binned <-
    post_sdlg_df %>%
    mutate(log_size_bin = cut_width(log_size, bin_width, boundary = log_size_range[1])) %>%
    group_by(log_size_bin, iteration) %>%
    summarize(n_plants = sum(n_plants)) %>%
    mutate(iteration = as.numeric(iteration)) 
  
  #make seedling number data frame and
  
  sdlg_df <- 
    tibble(
      log_size_bin = factor("seedling"),
      n_plants = as.numeric(pop_states$n_sdlg)
    ) %>% 
    add_column(iteration = 1:nrow(.))
  
  pop_state_tidy <-
    bind_rows(post_sdlg_binned, sdlg_df) %>% 
    mutate(log_size_bin = fct_relevel(log_size_bin, "seedling", after = 0)) %>%
    mutate(log_size_bin = fct_rev(log_size_bin)) %>% 
    #calculate proportions
    group_by(iteration) %>% 
    mutate(prop_plants = n_plants/sum(n_plants))
  
  if (normalize) {
    yvar <- "prop_plants"
  } else {
    yvar <- "n_plants"
  }
  
  p <- 
    ggplot(pop_state_tidy,
           aes_string(x = "iteration", y = yvar, fill = "log_size_bin")) +
    geom_area(color = "black", size =.2) +
    scale_fill_viridis_d() +
    coord_cartesian(expand = FALSE)
  #return
  p
}

