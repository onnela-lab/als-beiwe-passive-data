
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

rm(list = ls())
library(tidyverse)
library(data.table)
library(lubridate)

# path to remote (AWS) project directory
PROJ_DIR_REMOTE <- "foo"

# threshold of minimum number of valid seconds of raw accelerometry data 
# to make up a valid minute of raw accelerometry data 
valid_1s_cnt_THRESHOLD = 20


# ------------------------------------------------------------------------------
# read and combine t1min Bewie acc data
# ------------------------------------------------------------------------------

dat_dir <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed_0", "accelerometer", "ai_output_t1min")
dat_paths <- list.files(dat_dir, full.names = TRUE)
length(dat_paths)

# combine all files into one file
dat_l <- vector(mode = "list", length = length(dat_paths))
for (i in 1 : length(dat_paths)){
  print(i)
  dat_l[[i]] <- fread(dat_paths[i], colClasses = "character")
}
dat0 <- rbindlist(dat_l) %>% as.data.frame()
nrow(dat0)

# format data
dat1 <- 
  dat0 %>%
  select(-V1) %>%
  rename(all_1s_cnt = `1s_cnt`) %>%
  rename(utc_time_rounded1min0 = utc_time_rounded1min) %>%
  mutate(
    utc_time_rounded1min = ymd_hms(utc_time_rounded1min0),
    ai = as.numeric(ai),
    # valid_1s_cnt was created as follows on the second level in a Python script
    # df_out_agg1.loc[df_out_agg1['obs_cnt'] < 5, 'valid_1s'] = 0
    # 'valid_1s':'sum'
    valid_1s_cnt = as.integer(valid_1s_cnt),
    all_1s_cnt = as.integer(all_1s_cnt)
  )
# this likely gives warning "Problem while computing ... X failed to parse."
# (due to parsing datetime at midnight)
# the code below fixes the ones which "failed to parse" (due to parsing datetime at midnight)

# fix those who "failed to parse"
dat1a <- dat1 %>% filter(!is.na(utc_time_rounded1min))
dat1b <- dat1 %>% filter(is.na(utc_time_rounded1min))
dat1b <- 
  dat1b %>%
  mutate(
    utc_time_rounded1min0 = paste0(utc_time_rounded1min0, " 00:00:00"),
    utc_time_rounded1min = ymd_hms(utc_time_rounded1min0)
  )
dat2 <- 
  rbind(dat1a, dat1b) %>%
  select(-utc_time_rounded1min0) %>%
  rename(utc_time = utc_time_rounded1min) %>%
  mutate(utc_time_date = as.Date(utc_time))
# check
sum(is.na(dat2$utc_time_rounded1min))


# ------------------------------------------------------------------------------
# expand to a long form (each day has 1440 minute-level entries)
# ------------------------------------------------------------------------------

# expand to a long form
utc_time_df <-
  dat2 %>%
  group_by(beiwe_id) %>%
  filter(utc_time_date > as.Date("2020-01-01")) %>%
  summarise(
    utc_time_date_min = min(utc_time_date),
    utc_time_date_max = max(utc_time_date)
  ) %>%
  as.data.frame() %>%
  arrange(utc_time_date_min)
nrow(utc_time_df)
# 62

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
nrow(utc_time_grid_df)

# join date and time grid
dat3 <-
  utc_time_grid_df %>%
  left_join(dat2, by = c("utc_time", "beiwe_id")) %>%
  mutate(utc_time_date = as.Date(utc_time))
nrow(dat3)


# ------------------------------------------------------------------------------
# if not a valid minute, replace acc-derived measures as NA 
# ------------------------------------------------------------------------------

# define valid minute
dat4 <- 
  dat3 %>%
  mutate(
    valid_1s_cnt = ifelse(is.na(valid_1s_cnt), 0, valid_1s_cnt),
    all_1s_cnt = ifelse(is.na(all_1s_cnt), 0, all_1s_cnt),
    valid_minute = ifelse(valid_1s_cnt >= valid_1s_cnt_THRESHOLD, 1, 0),
    # fill in valid minutes with 0 
    valid_minute = ifelse(is.na(valid_minute), 0, valid_minute),
    # if not a valid minute, replace acc-derived measures as NA 
    ai = ifelse(valid_minute == 0, NA, ai),
    act_above_noise_1s = ifelse(valid_minute == 0, NA, act_above_noise_1s),
    act_above_noise_nocap_1s = ifelse(valid_minute == 0, NA, act_above_noise_nocap_1s)
  ) %>%
  mutate(
    act_above_noise_1s = as.numeric(act_above_noise_1s),
    act_above_noise_nocap_1s = as.numeric(act_above_noise_nocap_1s)
  )

# check: they should equal the same 
sum(is.na(dat4$utc_time_date))
sum(is.na(dat4$utc_time))

mean(dat4$valid_minute == 0)
mean(is.na(dat4$ai))
mean(is.na(dat4$act_above_noise_1s))
str(dat4)


# ------------------------------------------------------------------------------
# subset columns, arrange 
# ------------------------------------------------------------------------------

dat5 <-
  dat4 %>%
  select(
    beiwe_id,
    utc_time,
    utc_time_date,
    everything()
  ) %>%
  arrange(beiwe_id, utc_time)


# ------------------------------------------------------------------------------
# save to file 
# ------------------------------------------------------------------------------

dat_F <- dat5
dim(dat_F)

dat_F_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min.rds")
dat_F_path
saveRDS(dat_F, dat_F_path)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# add additional information: et time zone info
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read t1min AI processed data, pull valid minute information
dat_ai_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min.rds")
dat_ai <- readRDS(dat_ai_path) 
head(dat_ai)
str(dat_ai)

t1 <- Sys.time()
et_time_vec <- format(dat_ai$utc_time, tz = "America/New_York", usetz = TRUE)
dat_ai$et_time <- ymd_hms(as.character(et_time_vec))
dat_ai$et_time_date <- as.Date(dat_ai$et_time)
t2 <- Sys.time()
t2 - t1
# Time difference of 1.120166 mins

# remove duplicates in ET datetime
# duplicates will only be in march and nov
dat_ai_A <- dat_ai %>% filter(month(et_time_date) %in% c(3, 11))
dat_ai_B <- dat_ai %>% filter(!month(et_time_date) %in% c(3, 11))
nrow(dat_ai_A) + nrow(dat_ai_B) == nrow(dat_ai)

t1 <- Sys.time()
dat_ai_A2 <- 
  dat_ai_A %>%
  group_by(beiwe_id, et_time) %>%
  filter(row_number() == 1) %>%
  ungroup()
t2 <- Sys.time()
t2 - t1
# Time difference of 1.054535 mins

dim(dat_ai_A)
dim(dat_ai_A2)

dat_ai_et <- 
  rbind(dat_ai_A2, dat_ai_B) %>%
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
dat_ai_et_dt = as.data.table(dat_ai_et)
df_test = dat_ai_et_dt[ , count := .N, by = .(beiwe_id, et_time)]
summary(df_test$count)
# should be all 1s


# ------------------------------------------------------------------------------
# save 
# ------------------------------------------------------------------------------

dat_F <- dat_ai_et

dat_F_path <- file.path(PROJ_DIR_REMOTE, "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min_et.rds")
saveRDS(dat_F, dat_F_path)
  