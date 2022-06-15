library(targets)
library(marginaleffects)
library(patchwork)
tar_load(starts_with("vit_list"))

plot_cap(vit_list_det_cf$vit_surv, condition = "log_size_prev") +
plot_cap(vit_list_det_ff$vit_surv, condition = "log_size_prev")

plot_cap(vit_list_stoch_cf$vit_surv, condition = "log_size_prev", exclude = "s(year_fct)") +
plot_cap(vit_list_stoch_ff$vit_surv, condition = "log_size_prev", exclude = "s(year_fct)")

#still doesn't work https://github.com/vincentarelbundock/marginaleffects/issues/363
#weird work-around:
df <- data.frame()
plot_cap(vit_list_dlnm_cf$vit_surv, condition = "log_size_prev", exclude = "te(spei_history,L)") +
plot_cap(vit_list_dlnm_ff$vit_surv, condition = "log_size_prev", exclude = "te(spei_history,L)")
#but also, I should probably construct newdata manually.  Average SPEI at all lag times sounds in the spirit of an adjusted prediction.
predictions(vit_list_dlnm_cf$vit_surv, newdata = head(vit_list_dlnm_cf$vit_surv$model)) %>% as_tibble()
