
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)

# path to remote (AWS) project directory
PROJ_DIR_REMOTE <- "foo"


# ------------------------------------------------------------------------------
# read, combine, format values --  t1sec data
# ------------------------------------------------------------------------------

# directory specific to harmonic
dat_dir <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed_0", "accelerometer", "oak_output_t1sec_sustainedharmonic_2022-11-23")
dat_paths <- list.files(dat_dir, full.names = TRUE)
length(dat_paths)

# combine all files into one file
dat_l <- vector(mode = "list", length = length(dat_paths))
for (i in 1 : length(dat_paths)){ # i <- 1
  print(i)
  dat_l_i <- fread(dat_paths[i], colClasses = "character")
  # append only if it wasn't empty data frame (kept just in case)
  if (unlist(dat_l_i[1,2]) != "cadence_timestamp") {
    dat_l[[i]] <- dat_l_i
  } 
}
# should rebind without any issue
dat0 <- rbindlist(dat_l) %>% as.data.frame()
nrow(dat0)

# format data
dat1 <- 
  dat0 %>%
  select(-V1) %>%
  mutate(cadence_value = as.numeric(cadence_value)) %>%
  mutate(utc_time = ymd_hms(cadence_timestamp))

# if there are 0 NAs (no issue parsing the date) => OK
sum(is.na(dat1$utc_time))


# ------------------------------------------------------------------------------
# aggregate into 1 minute level
# ------------------------------------------------------------------------------

#' Note: 
#' 
#' - "cadence_value" says, for each second, how many steps a person made in this 1 second
#'   (it is a rate of walking)

dat2 <- 
  dat1 %>%
  rename(utc_time_t1sec = utc_time) %>%
  mutate(
    utc_time = floor_date(utc_time_t1sec, "minute")
  ) %>%
  group_by(
    beiwe_id,
    utc_time
  ) %>%
  summarise(
    walking_time_sec = n(),
    steps_cnt = sum(cadence_value),
    cadence = mean(cadence_value)
  ) %>%
  ungroup() %>%
  mutate(utc_time_date = as.Date(utc_time)) %>%
  as.data.frame()

# check
nrow(dat2)
summary(dat2$walking_time_sec)
summary(dat2$steps_cnt)
summary(dat2$cadence)


# ------------------------------------------------------------------------------
# expand to a long form (each day has 1440 minute-level entries)
# ------------------------------------------------------------------------------

# expand to a long form
utc_time_df <-
  dat2 %>%
  filter(utc_time_date > as.Date("2020-01-01")) %>%
  group_by(beiwe_id) %>%
  summarise(
    utc_time_date_min = min(utc_time_date),
    utc_time_date_max = max(utc_time_date)
  ) %>%
  as.data.frame()
nrow(utc_time_df)
# [1] 62

utc_time_grid_list <- list()
for (i in 1 : nrow(utc_time_df)){ # i <- 1
  print(i)
  beiwe_id_i <- utc_time_df$beiwe_id[i]
  utc_time_i <- seq(ymd_hms(paste0(utc_time_df$utc_time_date_min[i], " 00:00:00")),
                    ymd_hms(paste0(utc_time_df$utc_time_date_max[i], " 23:59:59")),
                    by = '1 min')
  out_df_i <- data.frame(utc_time = utc_time_i)
  out_df_i$beiwe_id = beiwe_id_i
  utc_time_grid_list[[i]] <- out_df_i
}
utc_time_grid_df <-
  rbindlist(utc_time_grid_list) %>%
  as.data.frame()

# join date and time grid
dat3 <-
  utc_time_grid_df %>%
  left_join(dat2, by = c("utc_time", "beiwe_id")) %>%
  # fill up the values which were not filled up 
  mutate(
    utc_time_date = as.Date(utc_time),
    walking_time_sec = ifelse(is.na(walking_time_sec), 0, walking_time_sec),
    steps_cnt = ifelse(is.na(steps_cnt), 0, steps_cnt)
  )

nrow(dat3)
# should not see NA's present here 
summary(dat3$steps_cnt)
summary(dat3$walking_time_sec)
# may see NA's present here 
summary(dat3$cadence)


# ------------------------------------------------------------------------------
# add AI-based valid minute information; 
# if not a valid minute, replace acc-derived measures as NA 
# ------------------------------------------------------------------------------

dat_ai_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min.rds")
dat_ai <- readRDS(dat_ai_path) 
head(dat_ai)
dat_ai_join <- dat_ai %>% select(beiwe_id, utc_time, utc_time_date, act_above_noise_1s, act_above_noise_nocap_1s, valid_minute)

dat4 <- 
  dat3 %>%
  inner_join(dat_ai_join, by = c("beiwe_id", "utc_time", "utc_time_date"))

dat5 <- 
  dat4 %>%
  mutate(
    # if not a valid minute, replace acc-derived measures as NA 
    walking_time_sec = ifelse(valid_minute == 0, NA, walking_time_sec),
    steps_cnt = ifelse(valid_minute == 0, NA, steps_cnt),
    cadence = ifelse(valid_minute == 0, NA, cadence)
  ) 

# may see NA's present here 
summary(dat5$steps_cnt)
summary(dat5$walking_time_sec)
summary(dat5$cadence)


# ------------------------------------------------------------------------------
# reorder columns, arrange
# ------------------------------------------------------------------------------

dat6 <- 
  dat5 %>%
  mutate(cadence = round(cadence, 5)) %>%
  select(
    beiwe_id, 
    utc_time_date, 
    utc_time, 
    everything())


# ------------------------------------------------------------------------------
# save
# ------------------------------------------------------------------------------

dat_F <- dat6
head(dat6)
nrow(dat6)

dat_F_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_oak_t1min_sustainedharmonic.rds")
dat_F_path
saveRDS(dat_F, dat_F_path)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# add additional information: ET time zone info
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

dat_oak <- dat_F
head(dat_oak)
str(dat_oak)

t1 <- Sys.time()
et_time_vec <- format(dat_oak$utc_time, tz = "America/New_York", usetz = TRUE)
dat_oak$et_time <- ymd_hms(as.character(et_time_vec))
dat_oak$et_time_date <- as.Date(dat_oak$et_time)
t2 <- Sys.time()
t2 - t1
# Time difference of 1.050266 mins

# remove duplicates in ET datetime
# duplicates will only be in march and nov
dat_oak_A <- dat_oak %>% filter(month(et_time_date) %in% c(3, 11))
dat_oak_B <- dat_oak %>% filter(!month(et_time_date) %in% c(3, 11))
nrow(dat_oak_A) + nrow(dat_oak_B) == nrow(dat_oak)

t1 <- Sys.time()
dat_oak_A2 <- 
  dat_oak_A %>%
  group_by(beiwe_id, et_time) %>%
  filter(row_number() == 1) %>%
  ungroup()
t2 <- Sys.time()
t2 - t1
# Time difference of 1.054535 mins

dim(dat_oak_A)
dim(dat_oak_A2)

dat_oak_et <- 
  rbind(dat_oak_A2, dat_oak_B) %>%
  select(
    beiwe_id,
    utc_time,
    utc_time_date,
    et_time,
    et_time_date,
    everything()
  ) %>%
  arrange(beiwe_id, et_time)

# quick check
dat_oak_et_dt = as.data.table(dat_oak_et)
dat_oak_et_dt[ , count := .N, by = .(beiwe_id, et_time)]
summary(dat_oak_et_dt$count)
# should be all 1s


# ------------------------------------------------------------------------------
# save 

dat_F <- dat_oak_et

dat_F_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_oak_t1min_et_sustainedharmonic.rds")
saveRDS(dat_F, dat_F_path)