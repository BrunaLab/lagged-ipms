# extract values of lambda fom targets to make figures

# load libraries ----------------------------------------------------------


library(targets)
library(tidyverse)
library(RColorBrewer)  


# extract data ------------------------------------------------------------

# Deterministic CF
ipm_det_cf<-tar_read(ipm_det_cf)
l_det_cf<-ipm_det_cf$pop_state$lambda
l_det_cf<-as.vector(l_det_cf)
l_det_cf<-as_tibble(l_det_cf) %>% 
  slice_tail(n=900) %>% 
  rename(det_cf=value)

# Deterministic FF
ipm_det_ff<-tar_read(ipm_det_ff)
l_det_ff<-ipm_det_ff$pop_state$lambda
l_det_ff<-as.vector(l_det_ff)
l_det_ff<-as_tibble(l_det_ff) %>% 
  slice_tail(n=900) %>% 
  rename(det_ff=value)

# lagged CF
ipm_dlnm_cf<-tar_read(ipm_dlnm_cf)
l_lag_cf<-ipm_dlnm_cf$pop_state$lambda
l_lag_cf<-as.vector(l_lag_cf)
l_lag_cf<-as_tibble(l_lag_cf) %>% 
  slice_tail(n=900) %>% 
  rename(lag_cf=value)



# lagged FF
ipm_dlnm_ff<-tar_read(ipm_dlnm_ff)
l_lag_ff<-ipm_dlnm_ff$pop_state$lambda
l_lag_ff<-as.vector(l_lag_ff)
l_lag_ff<-as_tibble(l_lag_ff) %>% 
  slice_tail(n=900) %>% 
  rename(lag_ff=value)

# stochastic CF
ipm_stoch_cf<-tar_read(ipm_stoch_cf)
l_stoch_cf<-ipm_stoch_cf$pop_state$lambda
l_stoch_cf<-as.vector(l_stoch_cf)
l_stoch_cf<-as_tibble(l_stoch_cf) %>% 
  slice_tail(n=900) %>% 
  rename(stoch_cf=value)

# stochastic FF
ipm_stoch_ff<-tar_read(ipm_stoch_ff)
l_stoch_ff<-ipm_stoch_ff$pop_state$lambda
l_stoch_ff<-as.vector(l_stoch_ff)
l_stoch_ff<-as_tibble(l_stoch_ff) %>% 
  slice_tail(n=900) %>% 
  rename(stoch_ff=value)


# bind into single tibble -------------------------------------------------

lambdas<-bind_cols(l_det_cf,
                   l_det_ff, 
                   l_stoch_cf,
                   l_stoch_ff,
                   l_lag_cf,
                   l_lag_ff) %>% 
  mutate(run=row_number(), .before=1) %>% 
  pivot_longer(cols = det_cf:lag_ff,
               names_to = "ipm",
               values_to = "lambda") %>% 
  arrange(ipm,run) %>% 
  mutate(lambda=round(lambda,3)) %>% 
  mutate(ipm_type=case_when(
    ipm == "det_cf" ~ "det",
    ipm == "det_ff" ~ "det",
    ipm == "stoch_cf" ~ "stoch",
    ipm == "stoch_ff" ~ "stoch",
    ipm == "lag_cf" ~ "lag",
    ipm == "lag_ff" ~ "lag",
    .default = as.character(ipm))
  ) %>% 
  mutate(habitat=case_when(
    ipm == "det_cf" ~ "cf",
    ipm == "det_ff" ~ "ff",
    ipm == "stoch_cf" ~ "cf",
    ipm == "stoch_ff" ~ "ff",
    ipm == "lag_cf" ~ "cf",
    ipm == "lag_ff" ~ "ff",
    .default = as.character(ipm))
  )
  
lambdas <- lambdas %>% 
  mutate(ipm_type=fct_relevel(ipm_type, 
              "det", "stoch", "lag")) %>% 
  mutate(habitat=fct_relevel(habitat, 
                              "cf", "ff"))


# plots -------------------------------------------------------------------
plot_data<-lambdas 

# plot_data<-lambdas
means<-plot_data %>% 
  group_by(ipm_type,habitat) %>% 
  summarize(mean_l=mean(lambda),
            sd_l=sd(lambda, na.rm = TRUE)) %>% 
  mutate(ipm_type = str_replace_all(
    ipm_type,
    c(
      "lag" = "Stochastic, parameter-resampled",
      "stoch" = "Stochastic, kernel-resampled",
      "det" = "Deterministic"
    )
  )) %>%
  # mutate(mean_l=if_else(habitat=="ff",(mean_l*(-1)),mean_l)) %>% # for plotting
  mutate(habitat = str_replace_all(
    habitat,
    c(
      "cf" = "Continuous Forest",
      "ff" = "Forest Fragments"
    )
  )) 
  

# 
# display.brewer.pal(n = 8, name = 'PRGn')
# brewer.pal(n = 8, name = "PRGn")

fig <- ggplot(plot_data, 
              aes(x=lambda,
                  fill=habitat)) +  
  geom_histogram(binwidth=.0015,
                 alpha=0.9,
                 color="gray35") +
  scale_fill_manual(values=c("#C2A5CF", "#A6DBA0")) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 250))+
  # scale_fill_brewer(palette = "PRGn")
  theme_classic()+
  facet_grid(rows = vars(ipm_type))+ #,
             # rows= vars(habitat))+
  # facet_wrap(~ipm_type)+
  # geom_vline(data=means, aes(xintercept=mean_l))
  geom_segment(aes(x = mean_l, 
                   y = 0, 
                   xend = mean_l, 
                   yend = 140,
                   color=habitat
                   ),
               linewidth = 0.8,
               data = means
               )+
  scale_color_manual(values=c("#C2A5CF", "#A6DBA0"))
fig

# PLOT 2 - MIRROR histogram


cf<-plot_data %>% dplyr::filter(habitat=="cf") %>% rename(lambda_cf=lambda)
ff<-plot_data %>% dplyr::filter(habitat=="ff") %>% rename(lambda_ff=lambda)
plot_data2<-left_join(cf,ff,by=c("run","ipm_type")) %>% 
  mutate(ipm_type = str_replace_all(
    ipm_type,
    c(
      "lag" = "Stochastic, parameter-resampled",
      "stoch" = "Stochastic, kernel-resampled",
      "det" = "Deterministic"
    )
  )) 
plot_data2
# 
# means2 <- means %>% 
#   select(-sd_l) %>% 
#   pivot_wider(names_from = habitat, values_from = mean_l) %>% 
#   mutate(ipm_type = str_replace_all(
#     ipm_type,
#     c(
#       "lag" = "Stochastic, parameter-resampled",
#       "stoch" = "Stochastic, kernel-resampled",
#       "det" = "Deterministic"
#     )
#   )) %>% 
# mutate(ff=ff*-1) # this is so that the line for the means of FF goes down
# Fill in missing values

plot_data2<-plot_data2 %>% dplyr::filter(ipm_type!="Deterministic")

lambdas_fig <- ggplot(plot_data2, aes(x=lambda)) +
  geom_histogram(aes(x = lambda_cf, 
                      y = ..density..),
                     # binwidth=10,
                      # alpha=0.9), 
                  fill="darkgreen",
                 color="gray20") +
  scale_y_continuous(n.breaks = 10, limits = c(-55, 55))+
  scale_x_continuous(n.breaks = 10, limits = c(0.82, 1.06))+
  annotate("text", x = 0.87, y = 20, label = "Continuous Forest", color="darkgreen")+
  geom_histogram( aes(x = lambda_ff, 
                      y = -..density..),
                      # binwidth=10,
                      # alpha=0.9
                  fill= "gray40",
                  color="gray20") +
  annotate("text", x = 0.87, y = -20, label = "Forest Fragments", color="gray40")+
  theme_classic() +
  xlab(expression(lambda))+
  ylab("No. of Iterations")+
  facet_grid(rows = vars(ipm_type))+
  geom_segment(aes(x = mean_l,
                   y = 0,
                   xend = mean_l,
                   yend = 50
  ),
  linewidth = 0.8,
  color="darkgreen",
  data = means %>% dplyr::filter(ipm_type!="Deterministic" & habitat=="Continuous Forest")
  )+
  geom_segment(aes(x = mean_l,
                   y = 0,
                   xend = mean_l,
                   yend = -50
  ),
  linewidth = 0.8,
  color="gray40",
  data = means %>% dplyr::filter(ipm_type!="Deterministic" & habitat=="Forest Fragments")
  )+
  annotate("segment", 
           x = 0.99, 
           xend = 0.99,
           y = 0, 
           yend = 50, 
           color="black",
           linetype=2)+
  annotate("segment", 
           x = 0.978, 
           xend = 0.978,
           y = 0, 
           yend = -50, 
           color="black",
           linetype=2)



ggsave("./docs/figures/lambdas_fig.png", height = 7.5, width = 8.5, units = "in")



# deterministic % greater -------------------------------------------------

dlnm_lambdas<-plot_data2 %>% dplyr::filter(ipm_type=="Stochastic, parameter-resampled")

det_lambda_cf<-means %>% dplyr::filter(ipm_type=="Deterministic" & habitat=="Continuous Forest") %>% select(mean_l)
det_lambda_cf<-as.numeric(det_lambda_cf[1,2])

det_lambda_ff<-means %>% dplyr::filter(ipm_type=="Deterministic" & habitat=="Forest Fragments") %>% select(mean_l)
det_lambda_ff<-as.numeric(det_lambda_ff[1,2])

n_lambdas<-nrow(dlnm_lambdas)
CFdlnm_perc_lessthan_det<-(dlnm_lambdas %>% arrange(lambda_cf) %>%  dplyr::filter(lambda_cf<det_lambda_cf) %>% tally())/n_lambdas*100
FFdlnm_perc_lessthan_det<-(dlnm_lambdas %>% arrange(lambda_ff) %>%  dplyr::filter(lambda_ff<det_lambda_ff) %>% tally())/n_lambdas*100
# statistical tests -------------------------------------------------------



t.test(lambda ~ habitat, 
       data = (plot_data %>% 
                 dplyr::filter(ipm_type=="stoch"))
       )

t.test(lambda ~ habitat, 
       data = (plot_data %>% 
                 dplyr::filter(ipm_type=="lag"))
)


res_aov <- aov(lambda ~ ipm_type,
               data = plot_data
)
# We can now check normality visually:
  
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

# test normality of the residuals with the Shapiro-Wilk test 
shapiro.test(res_aov$residuals)

# Boxplot
boxplot(lambda ~ habitat, 
        data = plot_data
)

# Dotplot
library("lattice")

dotplot(lambda ~ habitat, 
        data = plot_data
)

# Levene's test
leveneTest(lambda ~ habitat, 
           data = plot_data
)


# test the homogeneity of the variances and the normality of the 
# residuals visually (and both at the same time) 
  
par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(res_aov, which = 3)

# 2. Normality
plot(res_aov, which = 2)

# ANOVA

# With the oneway.test() function:
  # 1st method:
  oneway.test(lambda ~ habitat, 
              data = plot_data,
              var.equal = FALSE # assuming equal variances
  )
summary(res_aov)



# variances of popstructure ratios ----------------------------------------



ipm_list<-
  tar_read(
  list(
    det_cf = ipm_det_cf,
    det_ff = ipm_det_ff,
    stoch_cf = ipm_stoch_cf,
    stoch_ff = ipm_stoch_ff,
    dlnm_cf = ipm_dlnm_cf,
    dlnm_ff = ipm_dlnm_ff
  ))

ipm_det_cf<-tar_read(ipm_det_cf)
ipm_det_ff<-tar_read(ipm_det_ff)
ipm_dlnm_cf<-tar_read(ipm_dlnm_cf)
ipm_dlnm_ff<-tar_read(ipm_dlnm_ff)
ipm_stoch_cf<-tar_read(ipm_stoch_cf)
ipm_stoch_ff<-tar_read(ipm_stoch_ff)

ipm_stoch_cf$pop_state
ipm_list<-list(ipm_det_cf,
           ipm_det_ff,
           ipm_dlnm_cf,
           ipm_dlnm_ff,
           ipm_stoch_cf,
           ipm_stoch_ff
           )

# Deterministic CF
ipm_det_cf<-tar_read(ipm_det_cf)
l_det_cf<-ipm_det_cf$pop_state$lambda
l_det_cf<-as.vector(l_det_cf)
l_det_cf<-as_tibble(l_det_cf) %>% 
  slice_tail(n=900) %>% 
  rename(det_cf=value)

# Deterministic FF
ipm_det_ff<-tar_read(ipm_det_ff)
l_det_ff<-ipm_det_ff$pop_state$lambda
l_det_ff<-as.vector(l_det_ff)
l_det_ff<-as_tibble(l_det_ff) %>% 
  slice_tail(n=900) %>% 
  rename(det_ff=value)

# lagged CF
ipm_dlnm_cf<-tar_read(ipm_dlnm_cf)
l_lag_cf<-ipm_dlnm_cf$pop_state$lambda
l_lag_cf<-as.vector(l_lag_cf)
l_lag_cf<-as_tibble(l_lag_cf) %>% 
  slice_tail(n=900) %>% 
  rename(lag_cf=value)



# lagged FF
ipm_dlnm_ff<-tar_read(ipm_dlnm_ff)
size_lag_ff<-ipm_dlnm_ff$pop_state$n_log_size
l_lag_ff<-as.vector(l_lag_ff)
l_lag_ff<-as_tibble(l_lag_ff) %>% 
  slice_tail(n=900) %>% 
  rename(lag_ff=value)

# stochastic CF
ipm_stoch_cf<-tar_read(ipm_stoch_cf)
l_stoch_cf<-ipm_stoch_cf$pop_state$lambda
l_stoch_cf<-as.vector(l_stoch_cf)
l_stoch_cf<-as_tibble(l_stoch_cf) %>% 
  slice_tail(n=900) %>% 
  rename(stoch_cf=value)

# stochastic FF
ipm_stoch_ff<-tar_read(ipm_stoch_ff)
l_stoch_ff<-ipm_stoch_ff$pop_state$lambda
l_stoch_ff<-as.vector(l_stoch_ff)
l_stoch_ff<-as_tibble(l_stoch_ff) %>% 
  slice_tail(n=900) %>% 
  rename(stoch_ff=value)



#  from plot_pop_states ---------------------------------------------------



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
          "stoch" = "Kernel-resampled",
          "dlnm" = "Parameter-resampled"
        )
      ),
      habitat = toupper(habitat)
    ) %>% 
    rename(IPM = ipm, Habitat = habitat)
  return(df)
}
  
  
  #plot proportions over time with facet_grid
  # p <- 
  #   ggplot(df, aes(x = iteration, y = prop_plants, fill = log_size_bin)) +
  #   geom_area(aes(color = log_size_bin), size =.2) +
  #   scale_color_viridis_d("log(size) bin", aesthetics = c("fill", "color")) +
  #   coord_cartesian(expand = FALSE, xlim = xlim) +
  #   facet_grid(Habitat ~ IPM, labeller = label_both) +
  #   labs(y = "Proportion", x = "Iteration") + 
  #   theme_bw() +
  #   theme(panel.spacing = unit(1, "lines")) #increases spacing between top and bottom row so axis labels don't overlap
  # 
  #Plot relationship between proportion in CF and in FF at each iteration (after removing burnin)
#   p2 <- 
#     df %>% 
#     dplyr::filter(iteration > 30 & iteration <=250) %>% 
#     pivot_wider(id_cols = c(IPM, log_size_bin, iteration),
#                 names_from = Habitat, 
#                 values_from = prop_plants) %>%
#     ggplot(aes(x = CF, y = FF, color = log_size_bin, fill = log_size_bin)) +
#     geom_point(alpha = 0.4, key_glyph = draw_key_rect) + 
#     guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
#     geom_abline(slope = 1) +
#     scale_color_viridis_d("log(size) bin", aesthetics = c("fill", "color")) +
#     facet_wrap(~IPM, labeller = label_both) +
#     labs(x = "Proportion in CF", y = "Proportion in FF") +
#     theme_bw() +
#     theme(panel.spacing = unit(1, "lines")) +
#     coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))
#   
#   #use `patchwork` to put them together
#   #TODO: get legends to "collect" properly
#   full_plot <- 
#     p/p2 +
#     plot_annotation(tag_levels = "A") +
#     plot_layout(guides = "collect", heights = c(2,1))
#   
#   
#   if(!is.null(save_path)) {
#     ggsave(save_path, full_plot, ...)
#     return(save_path)
#   } else {
#     return(invisible(full_plot))
#   }
# }


library(tidyverse)
library(targets)
library(ipmr)


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
df<-plot_pop_states(ipm_list, xlim = c(0, 250))

df2<-df %>% 
  dplyr::filter(iteration > 30 & iteration <=250) %>% 
  pivot_wider(id_cols = c(IPM, log_size_bin, iteration),
              names_from = Habitat, 
              values_from = prop_plants) %>% 
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


df2$log_size_bin <- factor(df2$log_size_bin, 
                           levels = c("seedling", 
                                      "pre-reproductive1", 
                                      "pre-reproductive2", 
                                      "reproductive1", 
                                      "reproductive2"))


unique(df2$log_size_bin)

sdlg<-df2 %>% dplyr::filter(log_size_bin=="seedling")
pr_1<-df2 %>% dplyr::filter(log_size_bin=="pre-reproductive1")
pr_2<-df2 %>% dplyr::filter(log_size_bin=="pre-reproductive2")
rep_1<-df2 %>% dplyr::filter(log_size_bin=="reproductive1")
rep_2<-df2 %>% dplyr::filter(log_size_bin=="reproductive2")

cut_interval(rep_2$CF, 15)
cut_interval(rep_2$FF, 15)

# Define the number of bins
num_bins <- 10
# Assign values to bins of equal width
rep_2$CF_bin <- cut(rep_2$CF, breaks = num_bins, labels = FALSE)
rep_2$FF_bin <- cut(rep_2$FF, breaks = num_bins, labels = FALSE)
df2_bin <- df2 %>% 
  group_by(log_size_bin) %>% 
  mutate(CF_bin=cut(CF, breaks = num_bins, labels = FALSE)) %>% 
  mutate(FF_bin=cut(FF, breaks = num_bins, labels = FALSE)) %>% 
  relocate(CF_bin,.after="CF")

# variances in FF for given value in CF and vice versa  -------------------


variances_forCF_bins<-df2_bin %>% 
  group_by(IPM,log_size_bin,CF_bin) %>% 
  summarize(n_in_CFbin=n(),
            mean_FF=mean(FF),
            sd_FF=mean(FF),
            var_FF=var(FF),
  ) 
variances_forCF_bins


FFvar_plot<-ggplot(variances_forCF_bins, aes(x=CF_bin, y=mean_FF)) + 
  geom_errorbar(aes(ymin=mean_FF-sd_FF, ymax=mean_FF+sd_FF), width=.1, colour="#A6CEE3") +
  # geom_point()+
  geom_point(alpha = 0.4, key_glyph = draw_key_rect, colour="#A6CEE3") +
  facet_grid(cols=vars(log_size_bin),rows=vars(IPM))+
  guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
  # scale_color_brewer(palette = "Paired")+
  # scale_color_viridis_d("IPM", aesthetics = c("fill", "color"),option = "magma") +
  labs(y = "mean +/- SD of \nProp in FF", x = "Proportion in CF") +
  # ggtitle(stage)+
  scale_y_continuous(limits = c(0, 1))+
  theme(panel.spacing = unit(1, "lines")) +
  theme_bw() 


variances_forFF_bins<-df2_bin %>% 
  group_by(IPM,log_size_bin,FF_bin) %>% 
  summarize(n_in_FF_bin=n(),
            mean_CF=mean(CF),
            sd_CF=mean(CF),
            var_CF=var(CF),
  ) 
variances_forFF_bins


variances_forCF_bins2<-variances_forCF_bins %>% 
  rename(bin=CF_bin,
         n_in_bin=n_in_CFbin,
         mean_opposite=mean_FF,
         sd_opposite=sd_FF,
         var_opposite=var_FF) %>% 
  mutate(habitat="CF")

  variances_forFF_bins2<-variances_forFF_bins %>% 
  rename(bin=FF_bin,
         n_in_bin=n_in_FF_bin,
         mean_opposite=mean_CF,
         sd_opposite=sd_CF,
         var_opposite=var_CF) %>% 
  mutate(habitat="FF")
  
  variances_all<-bind_rows(variances_forFF_bins2,variances_forCF_bins2)
# "#A6CEE3" "#1F78B4" 

  all_var_plot<-
    ggplot(variances_all, aes(x=bin, y=mean_opposite,colour=habitat, shape=habitat)) + 
    geom_errorbar(aes(ymin=mean_opposite-sd_opposite, ymax=mean_opposite+sd_opposite),  position = "dodge",width=0.4) +
    geom_point(alpha = 0.4, key_glyph = draw_key_rect,position=position_dodge(width = 0.4))+
    facet_grid(cols=vars(log_size_bin),rows=vars(IPM))+
    # guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
    scale_color_brewer(palette = "Paired")+
    # scale_color_viridis_d("IPM", aesthetics = c("fill", "color"),option = "magma") +
    labs(y = "mean +/- SD of \nProp in Opposite Habitat", x = "Proportion in Habitat") +
    # ggtitle(stage)+
    # coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))+
    scale_y_continuous(limits = c(0, 1))+
    theme(panel.spacing = unit(1, "lines")) +
    theme(legend.position = "right")+
    theme_bw() 
  all_var_plot
  ggsave("./docs/figures/relative_variances_fig.png", height = 7.5, width = 8.5, units = "in")
  
  
  
###########
CFvar_plot<-
ggplot(variances_forFF_bins, aes(x=FF_bin, y=mean_CF)) + 
  geom_errorbar(aes(ymin=mean_CF-sd_CF, ymax=mean_CF+sd_CF), width=.1, colour="#1F78B4") +
  # geom_point()+
  geom_point(alpha = 0.4, key_glyph = draw_key_rect, colour="#1F78B4") +
  facet_grid(cols=vars(log_size_bin),rows=vars(IPM))+
  guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
  # scale_color_brewer(palette = "Paired")+
  # scale_color_viridis_d("IPM", aesthetics = c("fill", "color"),option = "magma") +
  labs(y = "mean +/- SD of \nProp in CF", x = "Proportion in FF") +
  # ggtitle(stage)+
  # coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))+
  scale_y_continuous(limits = c(0, 1))+
  theme(panel.spacing = unit(1, "lines")) +
  theme_bw() 

FFvar_plot/CFvar_plot

# marginal plots of each stage --------------------------------------------


marg_plot_data <- df2_bin %>% 
  dplyr::filter(IPM!="Deterministic") %>% 
  # dplyr::filter(IPM!="Kernel-resampled") %>% 
  dplyr::filter(log_size_bin=="reproductive1") 
  
# Kernel-resampled
# levels(as.factor(marg_plot_data$log_size_bin))

library(ggExtra) # marginal histogram
# classic plot :
# log_size_bin
p3<-ggplot(marg_plot_data, aes(x=CF, y=FF, color=IPM)) + #, size=CF/FF
  geom_point() +
  theme(legend.position="none")

# with marginal histogram
ggMarginal(p3, groupColour = TRUE, groupFill = TRUE)
ggMarginal(p3, type="histogram",groupColour = TRUE, groupFill = TRUE)


library(RColorBrewer)


  stage_marginal_plot <- function(stage) {
    library(patchwork)
    plot_data<-df2_bin %>%
      ungroup() %>% 
      dplyr::filter(IPM!="Deterministic") %>%  
      dplyr::filter(iteration > 30 & iteration <=250) %>%
      dplyr::filter(log_size_bin==stage) 
    
    plot_data<-droplevels(plot_data)
    
    p3 <-plot_data %>% 
      ggplot(aes(x=CF, y=FF, color=IPM)) +
      geom_point() +
      geom_point(alpha = 0.4, key_glyph = draw_key_rect) +
      guides(fill = guide_legend(override.aes = list(alpha = 1)), color = "none") +
      geom_abline(slope = 1) +
      scale_color_brewer(palette = "Paired")+
      
      # scale_color_viridis_d("IPM", aesthetics = c("fill", "color"),option = "magma") +
      # facet_wrap(~log_size_bin, labeller = label_both) +
      labs(x = "Proportion in CF", y = "Proportion in FF") +
      ggtitle(stage)+
      # coord_fixed(ratio = 1, xlim = c(0, 0.65), ylim = c(0, 0.65))+
      theme_bw() 
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
  class(p4)
  
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
      plot_layout(guides = "collect")
    marginal_all
    
    ggsave("./docs/figures/marginal_stage_fig.png", height = 7.5, width = 8.5, units = "in")