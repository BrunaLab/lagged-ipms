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
#' Uses custom size bins relating to changes in vital rates.  "seedling" is the
#' discrete class for seedlings, "pre-reproductive 1" are plants with low
#' survival (~ < 0.8) and a near 0 probability of flowering, "pre-reproductive
#' 2" are plants with high survival (~ >0.8) and a near 0 probability of
#' flowering, "reproductive 1" are plants with high survival probability and low
#' flowering probability (~ < 0.3), "reproductive 2" are plants with high
#' survival probability and high flowering probability (~ > 0.3).
#'
#' @param ipm_list a named list of `ipmr` IPM objects.  See example
#' @param xlim a length 2 vector passed to `coord_cartesian()`.  By default only
#'   the first 250 iterations are plotted for clarity.
#' @param save_path a filepath (including filename and extension) to save the
#'   plot.  Passed to `ggsave()`
#' @param ... other arguments passed to `ggsave()`
#'   
#' @return if `save_path` is not specified, then a ggplot object is returned.
#'   Otherwise `save_path` is returned.
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
#'   plot_pop_states(save_path = "test.png", height = 5)
#'   
plot_pop_states <- function(ipm_list, xlim = c(0, 250), save_path = NULL, ...) {
  df <-
    ipm_list %>%
    #extract and bin population states from all IPMs
    map_df(~bin_pop_states(.x), .id = "model") %>% 
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
  
  #plot proportions over time with facet_grid
  p <- 
    ggplot(df, aes(x = iteration, y = prop_plants, fill = log_size_bin)) +
    geom_area(aes(color = log_size_bin), size =.2) +
    scale_color_viridis_d("log(size) bin", aesthetics = c("fill", "color")) +
    coord_cartesian(expand = FALSE, xlim = xlim) +
    facet_grid(Habitat ~ IPM, labeller = label_both) +
    labs(y = "Proportion", x = "Iteration") + 
    theme_bw() +
    theme(panel.spacing = unit(1, "lines")) #increases spacing between top and bottom row so axis labels don't overlap
  
  #Plot relationship between proportion in CF and in FF at each iteration (after removing burnin)
  p2 <- 
    df %>% 
    dplyr::filter(iteration > 30 & iteration <=250) %>% 
    pivot_wider(id_cols = c(IPM, log_size_bin, iteration),
                names_from = Habitat, 
                values_from = prop_plants) %>%
    ggplot(aes(x = CF, y = FF, color = log_size_bin, fill = log_size_bin)) +
    geom_point(alpha = 0.4, key_glyph = draw_key_rect) + 
    guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
    geom_abline(slope = 1) +
    scale_color_viridis_d("log(size) bin", aesthetics = c("fill", "color")) +
    facet_wrap(~IPM, labeller = label_both) +
    labs(x = "Proportion in CF", y = "Proportion in FF") +
    theme_bw() +
    theme(panel.spacing = unit(1, "lines")) +
    coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))
  
  #use `patchwork` to put them together
  #TODO: get legends to "collect" properly
  full_plot <- 
    p/p2 +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect", heights = c(2,1))
    
  
  if(!is.null(save_path)) {
    ggsave(save_path, full_plot, ...)
    return(save_path)
  } else {
    return(invisible(full_plot))
  }
}



bin_pop_states <- function(ipm) {
  
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

  post_sdlg_binned <-
    post_sdlg_df %>%
    mutate(log_size_bin = case_when(
      log_size <= 2.5 ~ "pre-reproductive 1 [0, 2.5]",
      log_size > 2.5 & log_size <= 4.5 ~ "pre-reproductive 2 (2.5, 4.5]",
      log_size > 4.5 & log_size <= 6 ~ "reproductive 1 (4.5, 6]",
      log_size > 6 ~ paste0("reproductive 2 (6, ", round(log_size_range[2], 2),")")
    )) %>%     group_by(log_size_bin, iteration) %>%
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
