#' @description 
#' Generate table 2: Beiwe survey compliance, accelerometer and GPS data quality. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# study analysis sample 
dat_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
dat_beiwe_id <- readRDS(dat_beiwe_id_path) 
dim(dat_beiwe_id)
head(dat_beiwe_id)

# acc data: valid days only 
dat_acc_et_time_date_path <- file.path(here(), "results_objects", "analysis_sample_acc_et_time_date.rds")
dat_acc_et_time_date <- readRDS(dat_acc_et_time_date_path)
# check
dim(dat_acc_et_time_date)
head(dat_acc_et_time_date)
length(unique(dat_acc_et_time_date$beiwe_id))

# GPS data: valid days only 
dat_gps_local_time_date_path <- file.path(here(), "results_objects", "analysis_sample_gps_local_time_date.rds")
dat_gps_local_time_date <- readRDS(dat_gps_local_time_date_path)
# check
dim(dat_gps_local_time_date)
head(dat_gps_local_time_date)
length(unique(dat_gps_local_time_date$beiwe_id))

# survey data  
dat_alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
dat_alsfrsrse <- 
  fread(dat_alsfrsrse_path) %>% 
  as.data.frame() %>%
  filter(beiwe_id %in% dat_beiwe_id$beiwe_id)
# check
dim(dat_alsfrsrse)
head(dat_alsfrsrse)
length(unique(dat_alsfrsrse$beiwe_id))

# (precomputed)
# Beiwe passive data (acc, GPS) quality stats 
# dat_beiwe_passive_data_quality_path <- file.path(here(), "results_objects", "beiwe_passive_data_quality_stats_comb.rds")
# dat_beiwe_passive_data_quality <- readRDS(dat_beiwe_passive_data_quality_path)


# ------------------------------------------------------------------------------
# aggregate data 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# alsfrsrse number of submissions

alsfrsrse_submissions_cnt_median_min_max <- 
  dat_alsfrsrse %>%
  group_by(beiwe_id) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "ALSFRS-RSE submissions number Mean (SD), Median [min, max]", .before = everything())
alsfrsrse_submissions_cnt_median_min_max


# ------------------------------------------------------------------------------
# alsfrsrse days between submissions

alsfrsrse_submissions_daydiff_median_min_max <- 
  dat_alsfrsrse %>%
  group_by(beiwe_id) %>%
  mutate(days_diff = as.numeric(as.Date(et_time_date) - as.Date(lag(et_time_date))) ) %>%
  filter(!is.na(days_diff)) %>%
  summarise(value = mean(days_diff)) %>%
  ungroup() %>% 
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "ALSFRS-RSE avg. days difference between submissions Mean (SD), Median [min, max]", .before = everything())
alsfrsrse_submissions_daydiff_median_min_max


# ------------------------------------------------------------------------------
# Days in observation perid

wearables_compliance_daysinobsperiod <- 
  dat_beiwe_id %>%
  summarise(
    value_median = median(obs_duration),
    value_min = min(obs_duration),
    value_max = max(obs_duration),
    value_mean = mean(obs_duration),
    value_sd = sd(obs_duration)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Days in observation perid  Mean (SD), Median [min, max]", .before = everything())
wearables_compliance_daysinobsperiod


# ------------------------------------------------------------------------------
# # acc. data days included in the analysis sample

wearables_compliance_acc <- 
  dat_acc_et_time_date %>%
  group_by(beiwe_id) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  summarise(
    value_median = median(cnt),
    value_min = min(cnt),
    value_max = max(cnt),
    value_mean = mean(cnt),
    value_sd = sd(cnt)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "# acc. data days included in the analysis sample  Mean (SD), Median [min, max]", .before = everything())
wearables_compliance_acc


# ------------------------------------------------------------------------------
# # GPS data days included in the analysis sample

wearables_compliance_gps <- 
  dat_gps_local_time_date %>%
  group_by(beiwe_id) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  summarise(
    value_median = median(cnt),
    value_min = min(cnt),
    value_max = max(cnt),
    value_mean = mean(cnt),
    value_sd = sd(cnt)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "# GPS data days included in the analysis sample  Mean (SD), Median [min, max]", .before = everything())
wearables_compliance_gps


# --------------------------------------------------------------------------------------
# Avg. # acc. valid minutes per day in the observation period

# create extended version of the meta data
time_date_grid_list <- list()
for (i in 1 : nrow(dat_beiwe_id)){ # i <- 1
  print(i)
  beiwe_id_i <- dat_beiwe_id$beiwe_id[i]
  time_date_i <- seq(as.Date(dat_beiwe_id$date_min_meta[i]), as.Date(dat_beiwe_id$date_max_meta[i]), by = "1 day")
  out_df_i <- data.frame(time_date = time_date_i)
  out_df_i$beiwe_id = beiwe_id_i
  time_date_grid_list[[i]] <- out_df_i
}
time_date_grid_df <- rbindlist(time_date_grid_list) %>% as.data.frame()


acc_to_join_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
acc_to_join <- 
  readRDS(acc_to_join_path)  %>%
  inner_join(dat_beiwe_id) %>%
  filter(et_time_date >= date_min_meta) %>%
  filter(et_time_date <= date_max_meta) %>%
  select(beiwe_id, time_date = et_time_date, valid_minutes)

acc_quality_alldays <- 
  time_date_grid_df %>%
  left_join(acc_to_join) %>%
  mutate(valid_minutes = ifelse(is.na(valid_minutes), 0, valid_minutes)) %>%
  group_by(beiwe_id) %>%
  summarise(value = mean(valid_minutes)) %>%
  ungroup() %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Avg. # acc. valid minutes per day in the observation period   Mean (SD), Median [min, max]", .before = everything())
acc_quality_alldays


# --------------------------------------------------------------------------------------
# Avg. # acc. valid minutes per day in the analysis sampl

acc_quality_validdays <-
  dat_acc_et_time_date %>%
  group_by(beiwe_id) %>%
  summarise(value = mean(valid_minutes)) %>%
  ungroup() %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Avg. # acc. valid minutes per day in the analysis sample   Mean (SD), Median [min, max]", .before = everything())
acc_quality_validdays



# --------------------------------------------------------------------------------------
# Avg. # GPS collection minutes per day in the observation period

gps_to_join_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_processed_daily.csv")
gps_to_join <- 
  fread(gps_to_join_path)  %>%
  as.data.frame() %>%
  # because there is also "obs_duration" (days of monitoring) in dat_beiwe_id data frame
  rename(gps_obs_duration = obs_duration) %>%
  inner_join(dat_beiwe_id) %>%
  filter(local_time_date >= date_min_meta) %>%
  filter(local_time_date <= date_max_meta) %>%
  select(beiwe_id, time_date = local_time_date, gps_obs_duration) %>%
  mutate(time_date = as.Date(time_date))

gps_quality_alldays <- 
  time_date_grid_df %>%
  left_join(gps_to_join) %>%
  mutate(gps_obs_duration = ifelse(is.na(gps_obs_duration), 0, gps_obs_duration)) %>%
  group_by(beiwe_id) %>%
  summarise(value0 = mean(gps_obs_duration)) %>%
  ungroup() %>%
  # hours to minutes
  mutate(value = (value0 * 60)) %>%
    summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Avg. # GPS collection minutes per day in the observation period   Mean (SD), Median [min, max]", .before = everything())
gps_quality_alldays


# --------------------------------------------------------------------------------------
# Avg. # GPS collection minutes per day in the analysis sample 

gps_quality_validdays <-
  dat_gps_local_time_date %>%
  group_by(beiwe_id) %>%
  summarize(value0 = mean(obs_duration)) %>%
  ungroup() %>%
  # hours to minutes
  mutate(value = (value0 * 60)) %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.1f", value_mean), " (", sprintf("%.1f", value_sd), "), ", sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Avg. # GPS collection minutes per day in the analysis sample   Mean (SD), Median [min, max]", .before = everything())
gps_quality_validdays


# --------------------------------------------------------------------------------------
# combine together
# --------------------------------------------------------------------------------------

tbl_out <- 
  wearables_compliance_daysinobsperiod %>% 
  rbind(alsfrsrse_submissions_cnt_median_min_max) %>%
  rbind(alsfrsrse_submissions_daydiff_median_min_max) %>%
  rbind(wearables_compliance_acc) %>%
  rbind(acc_quality_alldays) %>%
  rbind(acc_quality_validdays) %>%
  rbind(wearables_compliance_gps) %>%
  rbind(gps_quality_alldays) %>%
  rbind(gps_quality_validdays) 
tbl_out

# View(tbl_out)

# ------------------------------------------------------------------------------
# save to file

tbl_out_path <- file.path(here(), 'results_tables', "table_2.csv")
fwrite(tbl_out, tbl_out_path)

