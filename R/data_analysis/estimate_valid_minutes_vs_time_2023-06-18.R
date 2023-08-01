
#' @description 
#' Estimate models that quantify "phone collects any data # minutes" and 
#' "phone on person # minutes" over time.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(here)
library(ggsci)
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
# read data
# ------------------------------------------------------------------------------

# study analysis sample: Beiwe ID 
sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id_with_factor.rds")
sample_beiwe_id <- readRDS(sample_beiwe_id_path) 
dim(sample_beiwe_id)
head(sample_beiwe_id)

# study analysis sample: Beiwe acc
sample_acc_path <- file.path(here(), "results_objects", "analysis_sample_acc_et_time_date.rds")
sample_acc <- readRDS(sample_acc_path) %>% select(beiwe_id, et_time_date)
dim(sample_acc)
length(unique(sample_acc$beiwe_id))

# study analysis sample: Beiwe GPS
sample_gps_path <- file.path(here(), "results_objects", "analysis_sample_gps_local_time_date.rds")
sample_gps <- readRDS(sample_gps_path) %>% select(beiwe_id, local_time_date)
dim(sample_gps)
length(unique(sample_gps$beiwe_id))



# ------------------------------------------------------------------------------
# survey data
dat_alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
dat_alsfrsrse <- 
  fread(dat_alsfrsrse_path) %>% 
  as.data.frame() %>%
  filter(beiwe_id %in% sample_beiwe_id$beiwe_id)

# daily measures: Beiwe acc AI 
dat_acc_ai_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
dat_acc_ai <- 
  readRDS(dat_acc_ai_path) %>%
  inner_join(sample_acc, by = c("beiwe_id", "et_time_date"))
dim(dat_acc_ai)
length(unique(dat_acc_ai$beiwe_id))

# daily measures: Beiwe acc walking 
# dat_acc_oak_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_oak_t24hr_allwalking_cond10sec.rds")
dat_acc_oak_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_oak_t24hr_et_sustainedharmonic_COND_10.rds")
dat_acc_oak <- 
  readRDS(dat_acc_oak_path) %>%
  inner_join(sample_acc, by = c("beiwe_id", "et_time_date"))
dim(dat_acc_oak)
length(unique(dat_acc_oak$beiwe_id))

# daily measures: Beiwe GPS
dat_gps_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_processed_daily.csv")
dat_gps <- 
  fread(dat_gps_path) %>%
  as.data.frame() %>%
  inner_join(sample_gps, by = c("beiwe_id", "local_time_date"))
dim(dat_gps)
length(unique(dat_gps$beiwe_id))


# ------------------------------------------------------------------------------
# make a combined data set
# ------------------------------------------------------------------------------

# prepare data sets for joining
dat_acc_ai_JOIN <- dat_acc_ai %>% rename(time_date = et_time_date)
dat_acc_oak_JOIN <- dat_acc_oak %>% rename(time_date = et_time_date) %>% select(-c(valid_minutes, phone_worn_minutes))
dat_gps_JOIN <- dat_gps %>% rename(time_date = local_time_date) %>% mutate(time_date = as.Date(time_date))
sample_beiwe_id_JOIN <- sample_beiwe_id %>% select(beiwe_id, date_min_meta, date_max_meta, beiwe_id_fct)
dat_comb <- 
  dat_acc_ai_JOIN %>%
  full_join(dat_acc_oak_JOIN) %>%
  full_join(dat_gps_JOIN) %>%
  full_join(sample_beiwe_id_JOIN) %>% 
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(time_date, min(time_date), units = c("days"))),
    month_relative = day_relative/30.5,
    day_relative_true = as.numeric(difftime(time_date, date_min_meta, units = c("days"))),
    month_relative_true = day_relative_true/30.5,
  ) %>%
  ungroup() 
  
# check
nrow(dat_comb)
nrow(dat_comb) == dat_comb %>% select(time_date, beiwe_id) %>% distinct() %>% nrow()
summary(dat_comb$day_relative)
summary(dat_comb$day_relative_true)
names(dat_comb)

# ------------------------------------------------------------------------------
# other mutate
# ------------------------------------------------------------------------------

dat_comb <- dat_comb %>% mutate(home_time = ifelse(home_time < 8, NA, home_time))


# ------------------------------------------------------------------------------
# FIT MOODEL: valid minutes over time 
# ------------------------------------------------------------------------------

# use only the days for which there is phone accelerometer data collected 
dat = dat_comb %>% filter(!is.na(ai))

y_name_vec <- c("valid_minutes")
y_label_vec <- c("Accelerometer valid minutes")
x_name <- "month_relative"
x_label <- "Time [months]"
mod_formula <- as.formula("y ~ x  + (1 + x | beiwe_id)")
y_scale = FALSE
x_scale = FALSE

out_list <- get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out_1 <- out_list$out_df
mod_out_1

# ------------------------------------------------------------------------------
# FIT MODEL: phone worn minutes
# ------------------------------------------------------------------------------

# use only the days for which there is phone accelerometer data collected 
mod_df = dat_comb %>% filter(!is.na(ai))

y_name_vec <- c("phone_worn_minutes")
y_label_vec <- c("Phone at person minutes")
x_name <- "month_relative"
x_label <- "Time [months]"
mod_formula <- as.formula("y ~ x  + (1 + x | beiwe_id)")
y_scale = FALSE
x_scale = FALSE

out_list <- get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out_2 <- out_list$out_df
mod_out_2

# ------------------------------------------------------------------------------
# Combine and save 
# ------------------------------------------------------------------------------

# save table to file
mod_out <- rbind(mod_out_1, mod_out_2)
fwrite(mod_out, file.path(here(), "results_tables", "table_lmm_estimate_acc_valid_minutes_vs_time.csv"))



