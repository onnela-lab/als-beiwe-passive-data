
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(here)
library(ggsci)
library(cowplot)
options(digits.secs = 0)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# note we currently only plot the model fitted values corresponding to models 
# where x_label was "ALSFRS-RSE total score" 

plt_df_subj_all <- readRDS(file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_alsfrsrse_plt_df_subj_all.rds"))
plt_df_popul_all <- readRDS(file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_alsfrsrse_plt_df_popul_all.rds"))
        
param_df <- 
  plt_df_subj_all %>% 
  select(y_name, y_label) %>% distinct() %>%
  mutate(dat_type = NA) %>%
  mutate(dat_type = replace(dat_type, grepl("Home", y_label), "gps")) %>%
  mutate(dat_type = replace(dat_type, grepl("Distance", y_label), "gps")) %>%
  mutate(dat_type = replace(dat_type, grepl("Index", y_label), "ai")) %>%
  mutate(dat_type = replace(dat_type, grepl("Steps", y_label), "walking")) %>%
  mutate(dat_type = replace(dat_type, grepl("cadence", y_label), "walking")) %>%
  mutate(model_type = "LMM") %>%
  mutate(model_type = replace(model_type, grepl("P\\(", y_label), "GLMM (binom)")) %>%
  mutate(dat_type = factor(dat_type, levels = c("gps", "ai", "walking"))) %>%
  mutate(model_type = factor(model_type, levels = c("LMM", "GLMM (binom)"))) %>%
  arrange(model_type, dat_type) %>%
  select(-c(dat_type))


# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

plt_list <- list()
for (i in 1 : nrow(param_df)){ # i <- 1
  y_name_i <- param_df$y_name[i]
  y_label_i <- param_df$y_label[i]
  plt_df_subj_i <- plt_df_subj_all %>% filter(y_name == y_name_i)
  plt_df_popul_i <- plt_df_popul_all %>% filter(y_name == y_name_i)
  beiwe_id_levels <- sort(unique(plt_df_subj_i$beiwe_id))
  plt_df_subj_i$beiwe_id_fct <- factor(plt_df_subj_i$beiwe_id, levels = beiwe_id_levels)
  
  txt_color <- "black"
  txt_size <- 10
  plt <- 
    ggplot(plt_df_subj_i, aes(x = x, y = y, color = beiwe_id_fct, group = beiwe_id_fct)) + 
    geom_line(aes(x = x, y = mu), size = 0.7, linetype = 1) + 
    geom_point(alpha = 0.3, size = 0.8) + 
    geom_line(data = plt_df_popul_i, aes(x = x, y = mu), inherit.aes = FALSE,
              size = 0.8, color = "black", alpha = 0.8) + 
    labs(y = y_label_i, x = "ALSFRS-RSE total score", title = "") + 
    theme(legend.position = "none", plot.margin = unit(rep(0.3, 4), "cm")) + 
    scale_x_continuous(breaks = seq(15, 45, 5), expand = rep(0.01, 2)) +
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(color = txt_color, size = txt_size),
      axis.text.y = element_text(color = txt_color, size = txt_size),  
      axis.title.x = element_text(color = txt_color, size = txt_size),
      axis.title.y = element_text(color = txt_color, size = txt_size),
      plot.title = ggtext::element_markdown(color = txt_color, size = txt_size),
      text = element_text(color = txt_color, size = txt_size) 
    ) 
  plt_list[[length(plt_list) + 1]] <- plt
}
# add empty data plot
plt_list[[length(plt_list) + 1]] <- ggplot() + geom_blank()

# generate final plot
plt <- plot_grid(plotlist = plt_list, ncol = 3, align = "vh", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "lmm_estimate_measure_vs_alsfrsrse_plt_df_subj_all.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10, dpi = 300)



