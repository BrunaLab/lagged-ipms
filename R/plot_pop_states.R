# library(targets)
# library(ipmr)
# tar_load(c(
#   ipm_det_cf,
#   ipm_det_ff,
#   ipm_stoch_cf,
#   ipm_stoch_ff,
#   ipm_dlnm_cf,
#   ipm_dlnm_ff
# ))



#' Plot population stage structure over time
#'
#' @param ipm_list a named list of `ipmr` IPM objects.  See example
#' @param bins number of bins for log_size
#' @param save_path a filepath (including filename and extension) to save the plot.  Passed to `ggsave()`
#' @param ... other arguments passed to `ggsave()`
#'
#' @return a ggplot object
#'
#' @examples
#' list(
#'   det_cf = ipm_det_cf,
#'   det_ff = ipm_det_ff,
#'   stoch_cf = ipm_stoch_cf,
#'   stoch_ff = ipm_stoch_ff,
#'   dlnm_cf = ipm_dlnm_cf,
#'   dlnm_ff = ipm_dlnm_ff
#' ) %>%
#'   plot_pop_states(bins = 6, save_path = "test.png", height = 5)
#'   
plot_pop_states <- function(ipm_list, bins = 7, save_path = NULL, ...) {
  df <-
    ipm_list %>%
    #extract and bin population states from all IPMs
    map_df(~bin_pop_states(.x, bins), .id = "model") %>% 
    #wrangle into a pretty dataframe
    separate(model, into = c("ipm", "habitat")) %>% 
    mutate(
      ipm = str_replace_all(
        ipm,
        c(
          "det" = "Deterministic",
          "stoch" = "Kernel-resampled",
          "dlnm" = "Parameter-resampled"
        )
      ),
      habitat = toupper(habitat)
    ) %>% 
    rename(IPM = ipm, Habitat = habitat)
  
  #plot with facet_grid
  
  p <- 
    ggplot(df, aes(x = iteration, y = prop_plants, fill = log_size_bin)) +
    geom_area(aes(color = log_size_bin), size =.2) +
    scale_color_viridis_d("log(size)", aesthetics = c("fill", "color")) +
    coord_cartesian(expand = FALSE) +
    facet_grid(Habitat ~ IPM, labeller = label_both) +
    labs(y = "Proportion", x = "Iteration") + 
    theme_bw() +
    theme(panel.spacing.y = unit(1, "lines")) #increases spacing between top and bottom row so axis labels don't overlap
  
  if(!is.null(save_path)) {
    ggsave(save_path, p, ...)
    return(save_path)
  } else {
    return(invisible(p))
  }
}



bin_pop_states <- function(ipm, bins) {
  
  meshpts <- int_mesh(ipm, full_mesh = FALSE)$log_size_1
  
  pop_states <- pop_state(ipm)
  rownames(pop_states$n_log_size) <-  meshpts
  
  post_sdlg_df <-
    as.data.frame(pop_states$n_log_size) %>%
    as_tibble(rownames = "log_size") %>%
    mutate(log_size = as.numeric(log_size)) %>% 
    pivot_longer(
      -log_size,
      names_to = "iteration",
      names_transform = list(iteration = ~factor(as.numeric(.))),
      values_to = "n_plants",
      names_prefix = "V")
  
  #bin sizes for easier plotting
  log_size_range <- as.numeric(ipmr::domains(ipm)$log_size[1:2])
  bin_width <- diff(log_size_range) / (bins - 1)
  post_sdlg_binned <-
    post_sdlg_df %>%
    mutate(log_size_bin = cut_width(log_size, bin_width, boundary = log_size_range[1])) %>%
    group_by(log_size_bin, iteration) %>%
    summarize(n_plants = sum(n_plants), .groups = "drop") %>%
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
}
