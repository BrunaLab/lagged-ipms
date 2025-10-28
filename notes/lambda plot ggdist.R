library(targets)
library(tidyverse)
library(ggdist)

tar_load(starts_with("lambda_"))

df <-
  tibble(
   det_ff = lambda_bt_det_ff,
   det_cf = lambda_bt_det_cf,
   stoch_ff = lambda_bt_stoch_ff,
   stoch_cf = lambda_bt_stoch_cf,
   dlnm_ff = lambda_bt_dlnm_ff,
   dlnm_cf = lambda_bt_dlnm_cf
 ) %>% 
    pivot_longer(everything(), 
                 names_to = c("ipm", "habitat"),
                 names_sep = "_", 
                 values_to = "lambda") %>% 
  mutate(
    ipm = fct_relevel(ipm, "det", "stoch", "dlnm") %>%
      fct_recode(Deterministic = "det", Stochastic = "stoch", `Lagged effects` = "dlnm")
  ) %>% 
  mutate(
    habitat = toupper(habitat)
  )


p <-
  ggplot(df, aes(x = lambda, fill = habitat)) +
  stat_gradientinterval(
    aes(slab_alpha = stat(-pmax(abs(1-2*cdf),.95))),
    .width = c(0.83, 0.95), #multiple quantiles can be specified for the interval (black lines)
    fill_type = "auto",
    point_interval = "mean_qi"
    ) +
  geom_vline(xintercept = 1) +
  facet_wrap(~ipm, ncol = 1) +
  scale_x_continuous(breaks = scales::extended_breaks(n = 10)) +
  scale_fill_manual(values = c(FF = "#E66101", CF = "#5E3C99")) +
  theme_bw() +
  theme(
    text = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption.position = "plot"
  ) +
  labs(
    x = "Population Growth Rate (lambda)",
    fill = "Habitat",
    caption = "Mean (point) ± 83% (thick) and 95% (thin) CIs\n from 500 bootstrapped simulations"
  )
p
ggsave("test.png", width = 950, height = 975, units = "px")

