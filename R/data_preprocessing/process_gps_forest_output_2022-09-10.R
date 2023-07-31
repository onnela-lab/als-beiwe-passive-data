
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

#' @descriptiom
#' Post-processing of Beiwe-derived and Forest jasmine-processed
#' GPS data. 
#' 
#' @author 
#' Marta Karas

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
options(digits.secs = 3)

# dir to remote (AWS) project directory 
PROJ_DIR = 'foo'


# ------------------------------------------------------------------------------
# combine participant-specific files
# ------------------------------------------------------------------------------

# define whether has gps data processed
dir_tmp <- file.path(PROJ_DIR, 'data_beiwe_processed_0', 'gps')
processed_fnames <- list.files(dir_tmp, full.names = TRUE, recursive = TRUE, pattern = ".csv")
length(processed_fnames)
# 140   # 202-09-10

dat_list <- list()
for (i in 1 : length(processed_fnames)){ # i <- 1
  print(i)
  path_tmp <- processed_fnames[i]
  dat <- fread(path_tmp) %>% as.data.frame()
  dat$beiwe_id <- gsub(".csv", "", basename(processed_fnames[i]))
  dat_list[[i]] <- dat
}
dat <- do.call("rbind", dat_list)
dim(dat)


# ------------------------------------------------------------------------------
# format data 
# ------------------------------------------------------------------------------

# fix for multiple (duplicate) participant-day entries 
# (due to with daylight saving change date)
dat <- 
  dat %>%
  mutate(local_time_date = make_date(year, month, day)) %>%
  group_by(beiwe_id, local_time_date) %>%
  filter(row_number() == 1) %>%
  ungroup()
nrow(dat)

# fix to replace with NA all stats where home_time>24h 
summary(dat$home_time)
sum(dat$home_time > 24, na.rm = TRUE)


# ------------------------------------------------------------------------------
# fill in data with intermediate missing dates in data (fill with NA measures values)
# this produces a nice output data set with no missingness within the values

beiwe_id_vec <- sort(unique(dat$beiwe_id))
date_list <- list()
for (i in 1 : length(beiwe_id_vec)){
  beiwe_id_i <- beiwe_id_vec[i]
  dat_i <- dat[dat$beiwe_id == beiwe_id_i, ]
  date_df_i <- data.frame(
    local_time_date = seq(min(dat_i$local_time_date), max(dat_i$local_time_date), by="days")
  )
  date_df_i$beiwe_id <- beiwe_id_i
  date_list[[i]] <- date_df_i
}
date_df <-
  rbindlist(date_list) %>%
  as.data.frame()
dim(date_df)

# left join dates grid data frame with previously generated gps data frame
# fill with 0's where no observation time was done (no join)
dat <-
  date_df %>%
  left_join(dat, by = c("local_time_date", "beiwe_id")) %>%
  mutate(
    obs_duration = ifelse(is.na(obs_duration), 0, obs_duration),
    obs_day      = ifelse(is.na(obs_day), 0, obs_day),
    obs_night    = ifelse(is.na(obs_night), 0, obs_night)
  )
nrow(dat)


# ------------------------------------------------------------------------------
# order columns

dat <- 
  dat %>% 
  select(beiwe_id, local_time_date, everything()) %>%
  arrange(beiwe_id, local_time_date) 
  # select(-other)


# ------------------------------------------------------------------------------
# save data 
# ------------------------------------------------------------------------------

path_tmp_dir <- file.path(PROJ_DIR, "data_beiwe_processed",  'gps') 
if (!dir.exists(path_tmp_dir)){
  dir.create(path_tmp_dir)
}
path_tmp <- file.path(path_tmp_dir, "gps_processed_daily.csv")
fwrite(dat, path_tmp)