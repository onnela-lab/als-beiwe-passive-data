#' @description 
#' This script uses linear mixed models (LMM) to estimate change of survey outcomes 
#' over time: 
#' - ALSFRS-RSE total score
#' - ALSFRS-RSE subscores: 
#'    * Q1-3; (bulbar function), 
#'    * Q4-6 (fine motor function), 
#'    * Q7-9 (gross motor function), 
#'    * Q10-12 (respiratory function). 

rm(list = ls())
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(lme4)
library(lmerTest)
library(cowplot)
library(broom.mixed)
library(MuMIn)
options(digits.secs = 0)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read and prepare data
# ------------------------------------------------------------------------------

# study analysis sample 
dat_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id_with_factor.rds")
dat_beiwe_id <- readRDS(dat_beiwe_id_path) 
dim(dat_beiwe_id)
head(dat_beiwe_id)

# survey data  
dat_alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
dat_alsfrsrse <- 
  fread(dat_alsfrsrse_path) %>% 
  as.data.frame() %>%
  filter(beiwe_id %in% dat_beiwe_id$beiwe_id) %>%
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(as.Date(et_time_date) - min(as.Date(et_time_date))),
    month_relative = day_relative / 30.5
  ) %>%
  ungroup() %>%
  mutate(beiwe_id_fct = factor(beiwe_id, levels = dat_beiwe_id$beiwe_id, labels = dat_beiwe_id$beiwe_id_fct)) %>%
  # add outcome which is a sum of 4 scores
  mutate(
    frs_q123 = frs1 + frs2 + frs3,
    frs_q456 = frs4 + frs5 + frs6,
    frs_q789 = frs7 + frs8 + frs9,
    frs_q101112 = frs10 + frs11 + frs12
  ) 
  
# check
dim(dat_alsfrsrse)
head(dat_alsfrsrse)
length(unique(dat_alsfrsrse$beiwe_id))


# ------------------------------------------------------------------------------
# fit LMM model -- ALSFRS-RSE total score, subscores
# ------------------------------------------------------------------------------

# predefine object to store results
mod_out = data.frame()

# shared params
dat = dat_alsfrsrse
x_name = "month_relative"
x_label = "Time [months]"
mod_formula = as.formula("y ~ x  + (1 + x | beiwe_id)")


# ALSFRS-RSE total score
y_name_vec = "frs_total_score"
y_label_vec = "ALSFRS-RSE total score"
mod_out_tmp <- (get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula))$out_df
mod_out = rbind(mod_out, mod_out_tmp)

# ALSFRS-RSE Q1-3
y_name_vec = "frs_q123"
y_label_vec = "ALSFRS-RSE Q1-3 (bulbar)"
mod_out_tmp = (get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula))$out_df
mod_out = rbind(mod_out, mod_out_tmp)

# ALSFRS-RSE Q4-6
y_name_vec = "frs_q456"
y_label_vec = "ALSFRS-RSE Q4-6 (fine motor)"
mod_out_tmp = (get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula))$out_df
mod_out = rbind(mod_out, mod_out_tmp)

# ALSFRS-RSE Q7-9
y_name_vec = "frs_q789"
y_label_vec = "ALSFRS-RSE Q7-9 (gross motor)"
mod_out_tmp = (get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula))$out_df
mod_out = rbind(mod_out, mod_out_tmp)

# ALSFRS-RSE Q10-12
y_name_vec = "frs_q101112"
y_label_vec = "ALSFRS-RSE Q10-12 (respiratory)"
mod_out_tmp = (get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula))$out_df
mod_out = rbind(mod_out, mod_out_tmp)


mod_out

# save to file 
fwrite(mod_out, file.path(here(), "results_tables", "table_lmm_estimate_survey_over_time.csv"))


# ------------------------------------------------------------------------------
# plot LMM model fit 
# ------------------------------------------------------------------------------

y_name_vec <- c("frs_total_score")
plt_y_breaks_list = list(
  seq(12, 50, by = 4)
)

plt_y_limits_list = list(
  c(NA, 50)
)

i = 1
# for (i in 1 : 2){ # i = 1
y_name = y_name_vec[i]
print(y_name)
# model data frame
mod_df <- 
  dat %>%
  rename_with(~ c("y"), all_of(y_name))  %>%
  rename_with(~ c("x"), all_of(x_name))
mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa"))
# plot data frames
plt_df <- mod_df 
plt_df$mu <- getME(mod_out, "mu")
plt_df_group <- expand.grid(x = c(0, 12))
plt_df_group$mu <- predict(mod_out, newdata = plt_df_group, re.form = NA)
# plot params 
plt_x_breaks <- seq(0, 12, by = 1)
plt_y_breaks <- plt_y_breaks_list[[i]]
plt_y_limits <- plt_y_limits_list[[i]] 
# plot 
plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = x, y = mu, color = beiwe_id_fct, group = beiwe_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = x, y = y, color = beiwe_id_fct, group = beiwe_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = x, y = mu), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(breaks = plt_y_breaks, limits = plt_y_limits) +
  labs(x = "Time [months]", y = "ALSFRS-RSE total score") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))
# save plot
plt_fname <- sprintf("lmm_survey_over_time_%s.jpeg", y_name)
plt_fpath <- file.path(here(), "results_figures", plt_fname)
ggsave(plt_fpath, plot = plt, width = 10 * 7/10, height = 7 * 7/10, units = "in", dpi = 150)
# }


# ------------------------------------------------------------------------------
# [optional]
# ------------------------------------------------------------------------------

# additional checks at the baseline

dat %>% 
  group_by(beiwe_id) %>%
  filter(et_time_date == min(et_time_date)) %>%
  ungroup() %>%
  pull(frs8)






