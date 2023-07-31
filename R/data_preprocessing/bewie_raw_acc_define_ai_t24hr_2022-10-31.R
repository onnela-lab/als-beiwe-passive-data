
rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(runstats)
library(here)
options(dplyr.summarise.inform = FALSE)

# other params
WINSORIZE_QUANTILE_PROB = 0.999


# ------------------------------------------------------------------------------
# read preprocessed t1min data
# ------------------------------------------------------------------------------

dat_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min_et.rds")
dat <- readRDS(dat_path) 
dim(dat)

# check
head(dat)
dat_test <- dat %>% filter(!is.na(ai))
summary(dat_test$ai)
summary(dat$ai)


# ------------------------------------------------------------------------------
# define ai, rescalled 
# ------------------------------------------------------------------------------

# define ai, rescalled proportionally to number of data seconds 
dat2 <-
  dat %>%
  rename(ai_0 = ai) %>%
  mutate(ai = ai_0 * (60 / valid_1s_cnt))


# ------------------------------------------------------------------------------
# winsorize AI
# ------------------------------------------------------------------------------

# windsorize overview 
dat_test <- dat2 %>% filter(!is.na(ai))
quantile(dat_test$ai, probs = seq(0, 1, by = 0.001))
dat_test %>%
  group_by(beiwe_id) %>%
  summarise(
    ai_0997 = quantile(ai, probs = 0.997),
    ai_0998 = quantile(ai, probs = 0.998),
    ai_0999 = quantile(ai, probs = 0.999)
  ) %>%
  arrange(desc(ai_0999)) %>%
  as.data.frame()

# windsorize
win_value <- quantile(dat_test$ai, probs = WINSORIZE_QUANTILE_PROB)
win_value

dat3 <-
  dat2 %>%
  mutate(ai = ifelse(ai > win_value, win_value, ai))
# check
dat2 %>% filter(!is.na(ai)) %>% pull(ai) %>% summary()
dat3 %>% filter(!is.na(ai)) %>% pull(ai) %>% summary()


# ------------------------------------------------------------------------------
# aggregate: ai sum, ai sum, valid_minutes sum
# ------------------------------------------------------------------------------

dat_agg_sum <- 
  dat3 %>%
  group_by(
    beiwe_id,
    et_time_date
  ) %>%
  summarise(
    ai = sum(ai, na.rm = TRUE),
    valid_minutes = sum(valid_minute),
    phone_worn_minutes = sum(act_above_noise_1s > 0, na.rm = TRUE)
  ) %>%
  ungroup()
# checks
summary(dat_agg_sum$ai)
summary(dat_agg_sum$valid_minutes)
summary(dat_agg_sum$phone_worn_minutes)


# ------------------------------------------------------------------------------
# aggregate: average minute value from X minutes with the highest value in a day (do not need to be consecutive)
# ------------------------------------------------------------------------------

# generate aggregates for all minutes
minutes_grid <- c(1, 5, 10, 15, 30, 45, 60)

# prepare table to which subsequent aggregates will be appended
dat_agg_max <- dat3 %>% select(beiwe_id, et_time_date) %>% distinct()
for (i in 1 : length(minutes_grid)){ # i <- 2
  message(paste0("i = ", i))
  # current number of minutes we consider
  minutes_i <- minutes_grid[i]
  # aggregate data 
  dat_agg_max_i <- 
    dat3 %>%
    group_by(beiwe_id, et_time_date) %>%
    arrange(beiwe_id, et_time_date, desc(ai)) %>%
    filter(row_number() <= minutes_i) %>%
    summarise(
      # deliberately does not have na.rm = TRUE 
      ai_max = mean(ai)
    ) %>%
    ungroup() %>%
    rename_with(~ c(paste0("ai_max_", minutes_i)), all_of(c("ai_max")))
  # append aggregated data
  dat_agg_max <- 
    dat_agg_max %>% 
    left_join(dat_agg_max_i, by = c("beiwe_id", "et_time_date"))
  print(dim(dat_agg_max))
}

dat_agg_max %>% summary()


# ------------------------------------------------------------------------------
# aggregate: average minute value from X consecutive minutes with the highest value in a day 
# ------------------------------------------------------------------------------

# generate aggregates for all minutes
minutes_grid <- c(5, 10, 15, 30, 45, 60)

dat_agg_maxrm <- dat3 %>% select(beiwe_id, et_time_date) %>% distinct()

for (i in 1 : length(minutes_grid)){ # i <- 1
  message(paste0("i = ", i))
  # current number of minutes we consider
  minutes_i <- minutes_grid[i]
  # mutate data for aggregation
  dat_maxrm_i <- 
    dat3 %>%
    mutate(
      # temporarily replace NA with 0 to be able to compute RunningMean on a whole vector
      ai_repl0 = ifelse(is.na(ai), 0, ai)
    ) %>%
    group_by(beiwe_id, et_time_date) %>%
    arrange(beiwe_id, et_time_date, et_time) %>%
    mutate(
      ai_rm_0 = RunningMean(ai_repl0, W = minutes_i, circular = TRUE),
      valid_minute_rm = RunningMean(valid_minute, W = minutes_i, circular = TRUE),
    ) %>%
    ungroup() %>%
    mutate(
      # replace with NA if not all minutes are valid  
      ai_rm = ifelse(valid_minute_rm < 1, NA, ai_rm_0),
      ai_rm = ifelse(ai_rm == (-Inf), NA, ai_rm)
    ) %>% 
    select(beiwe_id, et_time_date, ai_rm)
  # aggregate data 
  dat_agg_maxrm_i <- 
    dat_maxrm_i %>%
    group_by(beiwe_id, et_time_date) %>% 
    summarise(
      ai_maxrm = max(ai_rm, na.rm = TRUE)
    ) %>% 
    mutate(
      # replace -Inf (that happens when taking max() from vector of all NAs via summarize)
      ai_maxrm = ifelse(ai_maxrm == -Inf, NA, ai_maxrm)
    ) %>%
    rename_with(~ c(paste0("ai_maxrm_", minutes_i)), all_of(c("ai_maxrm")))
  # append aggregated data
  dat_agg_maxrm <- 
    dat_agg_maxrm %>% 
    left_join(dat_agg_maxrm_i, by = c("beiwe_id", "et_time_date"))
  print(dim(dat_agg_maxrm))
}

# check
head(dat_agg_maxrm)
# check whether any is Inf or -Inf 
dat_agg_maxrm %>% summary()


# ------------------------------------------------------------------------------
# aggregate: 95th percentile
# ------------------------------------------------------------------------------

dat_agg_95thperc <- 
  dat3 %>%
  group_by(
    beiwe_id,
    et_time_date
  ) %>%
  summarise(
    ai_95thperc = quantile(ai, prob = 0.95, na.rm = TRUE)
  ) %>%
  ungroup()
# checks
summary(dat_agg_95thperc$ai_95thperc)



# ------------------------------------------------------------------------------
# combine all together
# ------------------------------------------------------------------------------

dat_agg_F <- 
  dat_agg_sum %>%
  inner_join(dat_agg_max) %>%
  inner_join(dat_agg_maxrm) %>%
  inner_join(dat_agg_95thperc) %>%
  as.data.frame()
head(dat_agg_F)
dim(dat_agg_F)


# ------------------------------------------------------------------------------

# save to file 
dat_F <- dat_agg_F

dat_F_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
saveRDS(dat_F, dat_F_path)



