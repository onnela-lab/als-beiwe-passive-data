
rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(runstats)
library(here)
options(dplyr.summarise.inform = FALSE)

# source config file with definitions of some preprocessing parameters
source(file.path(here(), "R", "config.R"))

DAT_FNAME_vec <- c("beiwe_oak_t1min_et_sustainedharmonic.rds")
MINUMIM_WALKING_SECONDS_IN_MINUTE_vec <- c(0, 10, 20)
param_df = expand_grid(DAT_FNAME = DAT_FNAME_vec, MINUMIM_WALKING_SECONDS_IN_MINUTE = MINUMIM_WALKING_SECONDS_IN_MINUTE_vec)


for (row_i in 1 : nrow(param_df)){ # row_i <- 1

  # ------------------------------------------------------------------------------
  # read preprocessed data
  # ------------------------------------------------------------------------------
  
  DAT_FNAME <- param_df$DAT_FNAME[row_i]
  MINUMIM_WALKING_SECONDS_IN_MINUTE <- param_df$MINUMIM_WALKING_SECONDS_IN_MINUTE[row_i]
  
  # ------------------------------------------------------------------------------
  # read preprocessed data
  # ------------------------------------------------------------------------------
  
  dat_path <- file.path(here(), "data_beiwe_processed", "accelerometer", DAT_FNAME)
  dat <- readRDS(dat_path) 
  message(paste0("STARTING: ", DAT_FNAME, ", MINUMIM_WALKING_SECONDS_IN_MINUTE = ", MINUMIM_WALKING_SECONDS_IN_MINUTE))
  
  # filter to keep current conditioning
  dat2 <- 
    dat %>% 
    mutate(
      # if walking_time_sec less than predefined threshold, 
      # steps_cnt = ifelse(walking_time_sec < MINUMIM_WALKING_SECONDS_IN_MINUTE, 0, steps_cnt),
      # 2023-06-18: added multiplication of step count by 2x to account for 10 second-on, 10 second-off 
      # data collection cycle
      steps_cnt = ifelse(walking_time_sec < MINUMIM_WALKING_SECONDS_IN_MINUTE, 0, steps_cnt * 2),
      cadence = ifelse(walking_time_sec < MINUMIM_WALKING_SECONDS_IN_MINUTE, NA, cadence)
    )
  
  
  # ------------------------------------------------------------------------------
  # aggregate: average daily cadence, sum of steps, sum of walking time
  # ------------------------------------------------------------------------------
  
  dat_agg_sum <- 
    dat2 %>%
    group_by(
      beiwe_id,
      et_time_date
    ) %>%
    mutate(
      # to calculate weighted cadence, define its weighted (by number of steps in a minute) value
      cadence_w = cadence * steps_cnt
    ) %>%
    summarise(
      cadence = sum(cadence_w, na.rm = TRUE) / sum(steps_cnt, na.rm = TRUE),
      steps_cnt = sum(steps_cnt, na.rm = TRUE),
      walking_time_sec = sum(walking_time_sec, na.rm = TRUE),
      valid_minutes = sum(valid_minute),
      phone_worn_minutes = sum(act_above_noise_1s > 0, na.rm = TRUE)
    ) %>%
    ungroup()
  # checks
  print(summary(dat_agg_sum$steps_cnt))
  print(summary(dat_agg_sum$walking_time_sec))
  print(summary(dat_agg_sum$cadence))
  print(summary(dat_agg_sum$valid_minutes))
  print(summary(dat_agg_sum$phone_worn_minutes))
  
  # ------------------------------------------------------------------------------
  # aggregate: average minute value from X minutes with the highest value in a day (do not need to be consecutive)
  # ------------------------------------------------------------------------------
  
  # generate aggregates for all minutes
  minutes_grid <- c(1)
  
  # steps_cnt
  dat_agg_max_steps_cnt <- dat2 %>% select(beiwe_id, et_time_date) %>%  distinct()
  for (i in 1 : length(minutes_grid)){ # i <- 2
    message(paste0("i = ", i))
    # current number of minutes we consider
    minutes_i <- minutes_grid[i]
    # aggregate data 
    dat_agg_max_i <- 
      dat2 %>%
      group_by(beiwe_id, et_time_date) %>%
      arrange(beiwe_id, et_time_date, desc(steps_cnt)) %>%
      filter(row_number() <= minutes_i) %>%
      summarise(
        # deliberately does not have na.rm = TRUE 
        steps_cnt_max = mean(steps_cnt)
      ) %>%
      ungroup() %>%
      rename_with(~ c(paste0("steps_cnt_max_", minutes_i)), all_of(c("steps_cnt_max")))
    # append aggregated data
    dat_agg_max_steps_cnt <- 
      dat_agg_max_steps_cnt %>% 
      left_join(dat_agg_max_i, by = c("beiwe_id", "et_time_date"))
    print(dim(dat_agg_max_steps_cnt))
  }
  
  # check
  var_check_vec <- paste0("steps_cnt_max_", minutes_grid)
  for (var_check in var_check_vec){
    x <- dat_agg_max_steps_cnt %>% pull(var_check)
    print(var_check)
    print(summary(x))
  }
  
  
  # ------------------------------------------------------------------------------
  
  # cadence
  dat_agg_max_cadence <- dat2 %>% select(beiwe_id, et_time_date) %>%  distinct()
  for (i in 1 : length(minutes_grid)){ # i <- 2
    message(paste0("i = ", i))
    # current number of minutes we consider
    minutes_i <- minutes_grid[i]
    # aggregate data 
    dat_agg_max_i <- 
      dat2 %>%
      group_by(beiwe_id, et_time_date) %>%
      arrange(beiwe_id, et_time_date, desc(cadence)) %>%
      filter(row_number() <= minutes_i) %>%
      summarise(
        # deliberately does not have na.rm = TRUE 
        cadence_max = mean(cadence)
      ) %>%
      ungroup() %>%
      rename_with(~ c(paste0("cadence_max_", minutes_i)), all_of(c("cadence_max")))
    # append aggregated data
    dat_agg_max_cadence <- 
      dat_agg_max_cadence %>% 
      left_join(dat_agg_max_i, by = c("beiwe_id", "et_time_date"))
    print(dim(dat_agg_max_cadence))
  }
  dim(dat_agg_max_cadence)
  
  
  # ------------------------------------------------------------------------------
  # aggregate: 95th percentile
  # ------------------------------------------------------------------------------
  
  dat_agg_95thperc <- 
    dat2 %>%
    group_by(
      beiwe_id,
      et_time_date
    ) %>%
    summarise(
      steps_cnt_95thperc = quantile(steps_cnt, prob = 0.95, na.rm = TRUE),
      cadence_95thperc = quantile(cadence, prob = 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  # checks
  print(summary(dat_agg_95thperc$steps_cnt_95thperc))
  print(summary(dat_agg_95thperc$cadence_95thperc))
  
  
  # ------------------------------------------------------------------------------
  # combine all together
  # ------------------------------------------------------------------------------
  
  dat_agg_F <- 
    dat_agg_sum %>%
    inner_join(dat_agg_max_cadence) %>%
    inner_join(dat_agg_max_steps_cnt) %>%
    inner_join(dat_agg_95thperc) %>%
    as.data.frame()
  print(head(dat_agg_F))
  print(dim(dat_agg_F))

  
  # ------------------------------------------------------------------------------
  
  # save to file 
  dat_F <- dat_agg_F
  
  dat_F_name <- gsub("_t1min_", "_t24hr_", DAT_FNAME) 
  dat_F_name <- gsub("\\.rds", "", dat_F_name)
  dat_F_name <- paste0(dat_F_name, "_COND_", MINUMIM_WALKING_SECONDS_IN_MINUTE, ".rds")
  dat_F_path <- file.path(here(), "data_beiwe_processed", "accelerometer", dat_F_name)
  saveRDS(dat_F, dat_F_path)
  message(paste0("SAVED: ", dat_F_name))
}



