


# load libraries ----------------------------------------------------------


library(targets)
library(tidyverse)
library(patchwork)
library(ggExtra)


# load data ---------------------------------------------------------------
ipm_det_cf<-tar_read(ipm_det_cf)
ipm_det_ff<-tar_read(ipm_det_ff)
ipm_dlnm_cf<-tar_read(ipm_dlnm_cf)
ipm_dlnm_ff<-tar_read(ipm_dlnm_ff)
ipm_stoch_cf<-tar_read(ipm_stoch_cf)
ipm_stoch_ff<-tar_read(ipm_stoch_ff)

ipm_list<-
  list(
    det_cf = ipm_det_cf,
    det_ff = ipm_det_ff,
    stoch_cf = ipm_stoch_cf,
    stoch_ff = ipm_stoch_ff,
    dlnm_cf = ipm_dlnm_cf,
    dlnm_ff = ipm_dlnm_ff
  )

# create df of proportions in CF and FF  ----------------------------------



## FUNCTION - BIN POP STATES

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

# generate the Df you need

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
          "stoch" = "Stochastic",
          "dlnm" = "Stochastic with lagged effects"
        )
      ),
      habitat = toupper(habitat)
    ) %>% 
    rename(IPM = ipm, Habitat = habitat)
  return(df)
}

df<-plot_pop_states(ipm_list, xlim = c(0, 250))

# marginal plots of each stage --------------------------------------------

df %>% dplyr::filter(log_size_bin=="seedling") %>% dplyr::filter(Habitat=="FF") %>% arrange(desc(prop_plants))

marg_plot_data <- df %>% 
  dplyr::filter(iteration > 30 & iteration <=250) %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  pivot_wider(values_from=c(prop_plants,n_plants),names_from=Habitat) %>% 
  arrange(IPM,log_size_bin,iteration) %>% 
  # dplyr::filter(IPM!="Deterministic") %>% 
  # dplyr::filter(IPM!="Stochastic") %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\[0, 2\\.5\\]","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(2\\.5, 4\\.5\\]","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(6, 8\\.02\\)","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(4\\.5, 6\\]","")) %>% 
  mutate(log_size_bin=gsub(" ","",log_size_bin))


marg_plot_data$log_size_bin <- factor(marg_plot_data$log_size_bin, 
                                      levels = c("seedling", 
                                                 "pre-reproductive1", 
                                                 "pre-reproductive2", 
                                                 "reproductive1", 
                                                 "reproductive2"))


df_for_stats <- df %>% 
dplyr::filter(iteration > 30 & iteration <=250) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\[0, 2\\.5\\]","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(2\\.5, 4\\.5\\]","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(6, 8\\.02\\)","")) %>% 
  mutate(
    log_size_bin = str_replace_all(
      log_size_bin," \\(4\\.5, 6\\]","")) %>% 
  mutate(log_size_bin=gsub(" ","",log_size_bin))


df_for_stats$log_size_bin <- factor(df_for_stats$log_size_bin, 
                           levels = c("seedling", 
                                      "pre-reproductive1", 
                                      "pre-reproductive2", 
                                      "reproductive1", 
                                      "reproductive2"))


# stats on distribution of stage proportions ------------------------------



df_for_stats %>% 
  group_by(IPM,log_size_bin,Habitat) %>% 
  summarize(
    mean_prop_plants=mean(prop_plants),
    var_prop_plants=var(prop_plants),
    sd_prop_plants=sd(prop_plants),
    median_prop_plants=var(prop_plants),
    min_prop_plants=min(prop_plants),
    max_prop_plants=max(prop_plants)
  ) 



# plot of marginal values -------------------------------------------------


# library(RColorBrewer)
# library(viridis)

stage_marginal_plot <- function(stage) {
  
  
  
  if (stage=="seedling") {
    colors<-c("Stochastic"="#fde725",
              "Stochastic with lagged effects"= "#FEF5A8")
  } else if (stage=="pre-reproductive1") {
    colors<-c("Stochastic"="#5ec962",
              "Stochastic with lagged effects"= "#DFF4E0")
  } else if (stage=="pre-reproductive2") {
    colors<-c("Stochastic"="#21918c",
              "Stochastic with lagged effects"= "#A6D3D1")
  } else if (stage=="reproductive1") {
    colors<-c("Stochastic"="#3b528b",
              "Stochastic with lagged effects"= "#B1BAD1")
  } else if (stage=="reproductive2") {
    colors<-c("Stochastic"="#440154",
              "Stochastic with lagged effects"= "#DACCDD")
  } else {
    print('the stage is incorrectly defined')
  }
  
  #fde725 yellow sdlg #FEF5A8
  #5ec962 green pre-repro1 #DFF4E0 or darker #BFE9C0
  #21918c teal pre-repro2 #D3E9E8 or darker #A6D3D1
  #3b528b blue. repro 1. #D8DCE8 or darker #B1BAD1
  #440154 purple repro 2 (use #DACCDD for parameter-shuffle or darker #B499BB)
  
  
  
  
  p3 <- ggplot(marg_plot_data %>% 
                 dplyr::filter(log_size_bin==stage), 
               aes(x=prop_plants_CF, 
                   y=prop_plants_FF, 
                   color=IPM)) + #, size=CF/FF
    # geom_point(alpha = .7) +
    geom_point()+
    #removes legend
    # guides(fill = guide_legend(override.aes = list(alpha = 4)), color = "none") + 
    # Control legend circle sizes

    guides(color = guide_legend(override.aes = list(size = 5)))+
    geom_abline(slope = 1) +
    scale_color_manual(values=colors)+
    
    # scale_color_viridis_d("IPM", aesthetics = c("fill", "color"),option = "magma") +
    # facet_wrap(~log_size_bin, labeller = label_both) +
    labs(x = "Proportion in CF", y = "Proportion in FF") +
    ggtitle(stage)+
    # coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))+
    theme_bw() +
    theme(legend.position = "bottom")+
    theme(plot.title=element_text(family='', 
                                  face='bold', 
                                  colour='black', 
                                  size=16,
                                  hjust=0.5, 
                                  vjust=0.5))+
    theme(legend.text = element_text(size=14))+
    theme(axis.title = element_text(size = 14,
                                    color = "black"
                                    ))+
    theme(axis.text = element_text(size = 12,
                                    color = "black"
    ))+
    # theme(legend.position = "inside")+
    theme(legend.title=element_blank())
  # +
  #   theme(panel.spacing = unit(1, "lines")) 
  # 
  # +
  
  
  p3 <-
    # ggMarginal(p3, type="histogram",groupColour = TRUE, groupFill = TRUE)
    ggMarginal(p3, groupColour = TRUE, groupFill = TRUE)
  
  return(p3)
}



p1<-stage_marginal_plot("seedling")
p2<-stage_marginal_plot("pre-reproductive1")
p3<-stage_marginal_plot("pre-reproductive2")
p4<-stage_marginal_plot("reproductive1")
p5<-stage_marginal_plot("reproductive2")


# Use annotation_custom() to embed the grob or gtable
blank_plot <- ggplot() + 
  theme_void()

p1<- blank_plot +
  annotation_custom(p1)
p2<- blank_plot +
  annotation_custom(p2)
p3<- blank_plot +
  annotation_custom(p3)
p4<- blank_plot +
  annotation_custom(p4)
p5<- blank_plot +
  annotation_custom(p5)



#   #use `patchwork` to put them together
#   #TODO: get legends to "collect" properly
marginal_all <-
  p1 + plot_spacer()+ p2 + p3 + p4 + p5 +
  plot_layout(ncol = 2)+
  # (p1 | p2 | p3)/ (p4 | p5)+
  # (p1 / p2 / p3) / (p4 / p5)+
  plot_annotation(tag_levels = "A") +
  
  theme(plot.tag = element_text(face = "bold"))+
  plot_layout(guides = "collect")


ggsave("./docs/figures/marginal_stage_fig.png", height = 15, width = 10, units = "in")



marginal_all_long <-
  p1 + p2 + p3 + p4 + p5 +
  plot_layout(ncol = 1)+
  # (p1 | p2 | p3)/ (p4 | p5)+
  # (p1 / p2 / p3) / (p4 / p5)+
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")



ggsave("./docs/figures/marginal_stage_fig_long.png", height = 25, width = 5, units = "in")


# statistical tests - variances -------------------------------------------

write_csv(df_for_stats,"./docs/figures/df_for_stats.csv")


# F-test
my_data<-df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive1') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")
res.ftest <- var.test(prop_plants ~ IPM, data = my_data)
res.ftest


my_data2<-df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive2') %>% 
  dplyr::filter(IPM=="Stochastic") %>% 
  dplyr::filter(Habitat=="FF")

# Test for normality
library(rstatix)
df_for_stats %>%
  dplyr::filter(IPM!="Deterministic") %>% 
  group_by(log_size_bin,IPM) %>% 
  shapiro_test(prop_plants) 
# all deviate from normal distribution


library(vartest)

seedling_cf <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'seedling') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="CF")

seedling_ff <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'seedling') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")

pre1_cf <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive1') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="CF")


pre1_ff <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive1') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")
  


pre2_cf <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive2') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="CF")

pre2_ff <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'pre-reproductive2') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")



rep1_cf <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'reproductive1') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="CF")




rep1_ff <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'reproductive1') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")



rep2_cf <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'reproductive2') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="CF")

rep2_ff <- df_for_stats %>% 
  dplyr::filter(log_size_bin== 'reproductive2') %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  dplyr::filter(Habitat=="FF")





seedling_cf
seedling_ff
pre1_cf
pre1_ff
pre2_cf
pre2_ff
rep1_cf
rep1_ff
rep2_cf
rep2_ff

# 
# 
# library(purrr)
# 
# # Define a custom function
# custom_function <- function(x) {
#   return(mean(x))
# }
# 
# # Apply the custom function to grouped subsets using purrr
# result <- df_for_stats %>%
#   split(.$group) %>%
#   map_df(~ data.frame(group = unique(.x$group), mean_value = custom_function(.x$value)))
# 
# # Print the result
# print(result)


vartest::ansari.test(prop_plants ~ IPM, data = pre2_cf) 

qqnorm(my_data2$prop_plants)
qqline(my_data2$prop_plants)
