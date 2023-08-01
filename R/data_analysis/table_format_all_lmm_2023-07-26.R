
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(here)
options(digits.secs = 0)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read all tables and combine
# ------------------------------------------------------------------------------

dat_all = data.frame()


# ------------------------------------------------------------------------------
# [valid minutes ~ time] model

dat_fpath = file.path(here(), "results_tables", "table_lmm_estimate_acc_valid_minutes_vs_time.csv")
dat = fread(dat_fpath) %>% as.data.frame()
dat$model_label = "valid_minutes_time_model"
dat_all = rbind(dat_all, dat)

# ------------------------------------------------------------------------------
# [ALSFRS-R score ~ time] model

dat_fpath = file.path(here(), "results_tables", "table_lmm_estimate_survey_over_time.csv")
dat = fread(dat_fpath) %>% as.data.frame()
dat$model_label = "alsfrs_r_score_time_model"
dat_all = rbind(dat_all, dat)


# ------------------------------------------------------------------------------
# [measure ~ ALSFRS-R score] model

dat_fpath <- file.path(here(), "results_tables", "table_lmm_estimate_measure_vs_alsfrsrse.csv")
dat = fread(dat_fpath) %>% as.data.frame()
dat$model_label = "measure_alsfrs_r_score_model"
dat_all = rbind(dat_all, dat)


# ------------------------------------------------------------------------------
# [measure ~ time] model

dat_fpath <- file.path(here(), "results_tables", "table_lmm_estimate_measure_vs_time.csv")
dat = fread(dat_fpath) %>% as.data.frame()
# remove the baseline measures
dat = dat %>% filter(!grepl("bsln", y_label))
dat$model_label = "measure_time_model"
dat_all = rbind(dat_all, dat)


# ------------------------------------------------------------------------------
# do the BH correction
# ------------------------------------------------------------------------------

dim(dat_all)

dat_all$slope_pval_bhadj = p.adjust(dat_all$slope_pval, method = "BH")
sum(dat_all$slope_pval < 0.05)



# -----------------------------------------------------------------------------
# format 
# ------------------------------------------------------------------------------


dat_all %>% filter(slope_pval_bhadj < 0.2) %>% pull(slope_pval) %>% sort()

# format columns content
dat_all_fmt <- 
  dat_all %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    slope_pval_f = ifelse(slope_pval_bhadj < 0.2, sprintf("*%s", slope_pval), sprintf("%s", slope_pval))
  ) %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval_f, ")")
  ) %>% 
  select(
    y_label, 
    x_label, 
    intcp_f,
    slope_f,
    R2m,
    R2c,
    model_label
  ) %>% 
  as.data.frame() 

names_vec = c(
  "Outcome", 
  "Covariate", 
  "Interc. est. [95% CI]",
  "Slope est. [95% CI] (p-val.)",
  "R2m",
  "R2c"
)


# ------------------------------------------------------------------------------
# #1 measure vs time 
# ------------------------------------------------------------------------------

# Table 3

df_tmp = 
  dat_all_fmt %>%
  filter(model_label == "measure_time_model") %>%
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
names(df_tmp) <- names_vec

# view(df_tmp)


# ------------------------------------------------------------------------------
# #2 measure vs alsfrs-r 
# ------------------------------------------------------------------------------

# Table 4

y_label_levels = c(
  "Home time [hours]",
  "log(Distance travelled [km])",
  "log(Activity Index)",
  "log(Activity Index from top 1 minute)",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] from top 1 min",
  "log(Step count)",
  "log(Step count from top 1 minute)"
)

y_label_labels = c(
  "Home time [hours]",
  "log(Distance travelled) [km]",
  "log(Activity Index)",
  "log(Activity Index from top 1 minute)",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] from top 1 min",
  "log(Steps count)",
  "log(Steps count from top 1 minute)"
)

x_label_levels = c(
  "ALSFRS-RSE total score",
  "ALSFRS-RSE Q1-3 (bulbar)",
  "ALSFRS-RSE Q4-6 (fine motor)",
  "ALSFRS-RSE Q7-9 (gross motor)",
  "ALSFRS-RSE Q10-12 (respiratory)"
)

# unique(df_tmp$y_label)
# unique(df_tmp$x_label)

df_tmp = 
  dat_all_fmt %>%
  filter(model_label == "measure_alsfrs_r_score_model") %>%
  mutate(y_label = factor(y_label, levels = y_label_levels)) %>%
  mutate(x_label = factor(x_label, levels = x_label_levels)) %>%
  arrange(x_label, y_label)

df_tmp <- 
  df_tmp %>% 
  select(
    y_label, 
    x_label, 
    intcp_f,
    slope_f,
    R2m,
    R2c
  ) %>% 
  as.data.frame() 
names(df_tmp) <- names_vec

# view(df_tmp)


# ------------------------------------------------------------------------------
# #3 alsfrs-r vs time 
# ------------------------------------------------------------------------------

# Supplementary Table 1

df_tmp = 
  dat_all_fmt %>%
  filter(model_label == "alsfrs_r_score_time_model") 

# view(df_tmp)





