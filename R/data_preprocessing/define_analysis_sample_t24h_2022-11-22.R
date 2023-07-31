
rm(list = ls())

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(runstats)
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))

W <- 28
W_FRAC <- 7/28
OBSERVATION_PERIOD_DATE_CAP

# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# data with study start/end
dat_meta_path <- file.path(here(), "data_participants_other_processed", "beiwe_2_0_start_and_end_dates_clean.csv")
# define study sample observation period: 
# start date 1 day after "baseline_beiwe_date" 
# end date as min{1 day before "beiwe_data_query_end", OBSERVATION_PERIOD_DATE_CAP) 
# OBSERVATION_PERIOD_DATE_CAP is when we started pulling the data from the server for that participants
dat_meta <- 
  fread(dat_meta_path) %>% 
  as.data.frame() %>%
  mutate(
    date_min_meta = (baseline_beiwe_date + 1),
    date_max_meta = as.character(beiwe_data_query_end - 1),
    date_max_meta = ifelse(as.Date(date_max_meta) > OBSERVATION_PERIOD_DATE_CAP, as.character(OBSERVATION_PERIOD_DATE_CAP), date_max_meta),
    date_max_meta = as.Date(date_max_meta)
  ) %>% 
  select(beiwe_id, date_min_meta, date_max_meta) 

# check
range(dat_meta$date_min_meta)
range(dat_meta$date_max_meta)


# ------------------------------------------------------------------------------
# read survey data
# ------------------------------------------------------------------------------

# data with beiwe surveys
alsfrsrse_df_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
alsfrsrse_df <- fread(alsfrsrse_df_path) %>% as.data.frame()

length(unique(alsfrsrse_df$beiwe_id))
!any(is.na(alsfrsrse_df))
dim(alsfrsrse_df)

# define sample that passes condition: at least 2 complete surveys
sample_from_alsfrsrse <- 
  (alsfrsrse_df) %>%
  group_by(beiwe_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  pull(beiwe_id) %>%
  unique()
length(sample_from_alsfrsrse)


# ------------------------------------------------------------------------------
# define eligible subset based on acc data 
# ------------------------------------------------------------------------------

# acc data 
dat_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
acc_df0 <- readRDS(dat_path) 
acc_df1 <- 
  acc_df0 %>%
  inner_join(dat_meta) %>%
  filter(et_time_date >= date_min_meta) %>%
  filter(et_time_date <= date_max_meta) 

# get participant specific bands
plt_df_bands <- 
  acc_df1 %>% 
  filter(valid_minutes > 0) %>%
  group_by(beiwe_id) %>%
  summarise(
    valid_minutes_mean = mean(valid_minutes)
  ) %>%
  ungroup() %>%
  mutate(
    valid_minutes_lo = valid_minutes_mean - ACC_VALID_MINUTES_BAND_EPSILON,
    valid_minutes_up = valid_minutes_mean + ACC_VALID_MINUTES_BAND_EPSILON
  )

# keep only values within participant-specific [mean +/- ACC_VALID_MINUTES_BAND_EPSILON] bands for balid minutes
acc_df2 <- 
  acc_df1 %>%
  inner_join(plt_df_bands, by = "beiwe_id") %>%
  mutate(day_is_within_band = ifelse(valid_minutes > valid_minutes_lo & valid_minutes < valid_minutes_up, 1, 0)) 


# ------------------------------------------------------------------------------
# filter: keep only acc dates that meet conditions for valid days: 
# is kept, is within any 28 days long window such that at least 14 days are valid

# check: should all be 1 except a few NAs
acc_df2 %>%
  group_by(beiwe_id) %>%
  mutate(
    days_diff = as.numeric(et_time_date - lag(et_time_date))
  ) %>%
  pull(days_diff) %>%
  summary()

# define running mean calculated out of 0/1-valued flag whether a day is within 
# the participant-specific valid day bounds
acc_df3 <- 
  acc_df2 %>%
  group_by(beiwe_id) %>%
  # to calculate running mean
  filter(n() >= W) %>%
  arrange(beiwe_id, et_time_date) %>%
  mutate(
    day_is_within_band_rm = runstats::RunningMean(day_is_within_band, W = W, circular = FALSE)
  )
summary(acc_df3$day_is_within_band_rm)


# define a 0/1-valued flag whether a day is within any W days long window such 
# that at least W_FRAC days are within the participant-specific valid day bounds
acc_df4a <- data.frame()
beiwe_id_vec <- unique(acc_df3$beiwe_id)
for (i in 1 : length(beiwe_id_vec)){
  print(paste0("i = ", i))
  beiwe_id_i <- beiwe_id_vec[i]
  acc_df_i <- acc_df3 %>% filter(beiwe_id == beiwe_id_i)
  day_is_eligiblevalid_i <- rep(0, nrow(acc_df_i))
  j_cnt <- (nrow(acc_df_i) - W + 1)
  for (j in 1 : j_cnt){
    W_win_ok <- (acc_df_i$day_is_within_band_rm[j] >= W_FRAC)
    if (W_win_ok){
      idx_ok <- j + seq(0, by = 1, length.out = W)
      day_is_eligiblevalid_i[idx_ok] <- 1
    }
  }
  acc_df_i$day_is_eligiblevalid <- day_is_eligiblevalid_i
  acc_df4a <- rbind(acc_df4a, acc_df_i)
}

# final filtering
acc_df4 <- 
  acc_df4a %>%
  filter(valid_minutes > 0,day_is_within_band == 1, day_is_eligiblevalid == 1) %>%
  group_by(beiwe_id) %>%
  filter(n() >= 28) %>%
  as.data.frame()

# ------------------------------------------------------------------------------
# define sample that passes all above conditions for acc

sample_from_acc <- 
  acc_df4 %>%
  pull(beiwe_id) %>%
  unique()
length(sample_from_acc)


# ------------------------------------------------------------------------------
# define eligible subset based on GPS data 
# ------------------------------------------------------------------------------

gps_df_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_processed_daily.csv")
gps_df0 <- fread(gps_df_path) %>% as.data.frame()

gps_df1 <- 
  gps_df0 %>%
  inner_join(dat_meta) %>%
  filter(local_time_date >= date_min_meta) %>%
  filter(local_time_date <= date_max_meta) %>%
  select(beiwe_id, local_time_date, obs_duration, home_time) %>%
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup() 

# check
mean(!is.na(gps_df1$home_time))


gps_df2 <- 
  gps_df1 %>%
  mutate(day_has_any_observation = ifelse(is.na(home_time), 0, 1)) %>%
  group_by(beiwe_id) %>%
  filter(n() >= W) %>%
  arrange(beiwe_id, local_time_date) %>%
  mutate(
    day_has_any_observation_rm = runstats::RunningMean(day_has_any_observation, W = W, circular = FALSE)
  ) %>%
  ungroup()
summary(gps_df2$day_has_any_observation_rm)

# define a 0/1-valued flag whether a day is within any 28 days long window such 
# that at least 14 days have any obGPS observation time 
gps_df3a <- data.frame()
beiwe_id_vec <- unique(gps_df2$beiwe_id)
for (i in 1 : length(beiwe_id_vec)){
  print(paste0("i = ", i))
  beiwe_id_i <- beiwe_id_vec[i]
  gps_df_i <- gps_df2 %>% filter(beiwe_id == beiwe_id_i)
  day_is_eligiblevalid_i <- rep(0, nrow(gps_df_i))
  j_cnt <- (nrow(gps_df_i) - W + 1)
  for (j in 1 : j_cnt){
    W_win_ok <- (gps_df_i$day_has_any_observation_rm[j] >= W_FRAC)
    if (W_win_ok){
      idx_ok <- j + seq(0, by = 1, length.out = W)
      day_is_eligiblevalid_i[idx_ok] <- 1
    }
  }
  gps_df_i$day_is_eligiblevalid <- day_is_eligiblevalid_i
  gps_df3a <- rbind(gps_df3a, gps_df_i)
}


# final filtering
gps_df3 <- 
  gps_df3a %>%
  filter(day_has_any_observation == 1, day_is_eligiblevalid == 1) %>%
  group_by(beiwe_id) %>%
  filter(n() >= 28) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# define sample that passes all above conditions for gps

sample_from_gps <- 
  gps_df3 %>%
  pull(beiwe_id) %>%
  unique()
length(sample_from_gps)


# ------------------------------------------------------------------------------
# combine samples: beiwe_id only
# ------------------------------------------------------------------------------

df1 <- data.frame(beiwe_id = sample_from_alsfrsrse, has_alsfrsrse = 1)
df2 <- data.frame(beiwe_id = sample_from_acc, has_acc = 1)
df3 <- data.frame(beiwe_id = sample_from_gps, has_gps = 1)

df_comb <- df1 %>% full_join(df2) %>% full_join(df3)
df_comb
df_comb_final_sub <- 
  df_comb %>%
  filter(complete.cases(.))
dim(df_comb_final_sub)


# ------------------------------------------------------------------------------
# analysis sample beiwe id with observation period range dates and 
# observation period duration

sample_beiwe_id_F <- 
  dat_meta %>%
  filter(beiwe_id %in% df_comb_final_sub$beiwe_id) %>%
  mutate(obs_duration = (as.numeric(difftime(date_max_meta, date_min_meta)) + 1 ))

saveRDS(sample_beiwe_id_F, file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds"))


# ------------------------------------------------------------------------------
# acc data 

sample_acc_et_time_date <- 
  acc_df4 %>%
  filter(beiwe_id %in% df_comb_final_sub$beiwe_id) %>%
  select(beiwe_id, et_time_date, valid_minutes, phone_worn_minutes) 

saveRDS(sample_acc_et_time_date, file.path(here(), "results_objects", "analysis_sample_acc_et_time_date.rds"))


# ------------------------------------------------------------------------------
# GPS data 

sample_gps_et_time_date <- 
  gps_df3 %>%
  filter(beiwe_id %in% df_comb_final_sub$beiwe_id) %>%
  select(beiwe_id, local_time_date, obs_duration) 
length(unique(sample_gps_et_time_date$beiwe_id))

saveRDS(sample_gps_et_time_date, file.path(here(), "results_objects", "analysis_sample_gps_local_time_date.rds"))

