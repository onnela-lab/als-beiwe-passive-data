#' @description 
#' Generate table 1: Baseline demographic and clinical characteristics 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# demographic data 
dat_demog_path <- file.path(here(), "data_participants_other_processed", "final_study_sample.csv")
dat_demog <- 
  fread(dat_demog_path) %>% 
  as.data.frame() %>%
  select(beiwe_id, sex, ethnicity, race, age, date_app_downloaded)

# phone OS
dat_os_path <- file.path(here(), "data_beiwe_processed", "identifiers", "beiwe_os.csv")
dat_os <- 
  fread(dat_os_path) %>% 
  as.data.frame() 

# ALSFRS-RSE at baseline
dat_alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
dat_alsfrsrse <- 
  fread(dat_alsfrsrse_path) %>% 
  as.data.frame() %>%
  group_by(beiwe_id) %>%
  filter(et_time_date == min(et_time_date)) %>%
  ungroup() %>%
  select(beiwe_id, frs_total_score)

# study analysis sample 
dat_analysis_sample_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
dat_analysis_sample <- 
  readRDS(dat_analysis_sample_path) 
dim(dat_analysis_sample)

# final df 
comb_df0 <- 
  dat_demog %>%
  inner_join(dat_os) %>%
  inner_join(dat_alsfrsrse) %>%
  filter(beiwe_id %in% dat_analysis_sample$beiwe_id)

# sanity check
length(unique(comb_df0$beiwe_id)) == nrow(dat_analysis_sample)

# range of enrollment
range(comb_df0$date_app_downloaded)
sort(comb_df0$date_app_downloaded)

# ------------------------------------------------------------------------------
# recode values
# ------------------------------------------------------------------------------

unique(comb_df0$sex)
unique(comb_df0$ethnicity)
unique(comb_df0$race)
unique(comb_df0$device_os)

comb_df <- 
  comb_df0 %>%
  mutate(
    sex = recode(sex, 'female'='Female', 'male'='Male'),
    ethnicity = recode(ethnicity, 'hispanic_or_latino'='Hispanic or Latino', 'not_hispanic_or_latino'= 'Not Hispanic or Latino', 'unknown_not_reported'= 'Unknown / Not Reported'),
    # race = recode(race, '0'= 'American Indian/Alaska Native', '1'= 'Asian', '2'= 'Native Hawaiian or Other Pacific Islander', '3'= 'Black or African American', '4'= 'White', '5'= 'More Than One Race', '6'= 'Unknown / Not Reported', '7'= 'Other'),
    race = recode(race, 
                  'native_hawaiian_or_other_pacific_islander'= 'Native Hawaiian or Other Pacific Islander', 
                  'white'= 'White', 
                  'more_than_one_race'= 'More Than One Race', 
                  'other'= 'Other'),
    device_os = recode(device_os, 'ios'= 'iOS', 'android'= 'Android')
  ) %>%
  rename(alsfrsrse_baseline = frs_total_score)

comb_df
dim(comb_df)


# ------------------------------------------------------------------------------
# calculate sample statistics
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# count 
tbl_cnt <- 
  comb_df %>%
  summarise(value_f = n()) %>%
  mutate(var_name = "Count all (n)", .before = everything())


# ------------------------------------------------------------------------------
# age -- mean_sd
tbl_age_mean_sd <- 
  comb_df %>%
  summarise(
    value_mean = mean(age),
    value_sd = sd(age)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Age Mean (SD)", .before = everything())

# age -- median_min_max
tbl_age_median_min_max <- 
  comb_df %>%
  summarise(
    value_median = median(age),
    value_min = min(age),
    value_max = max(age)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Age Median [min, max]", .before = everything())


# ------------------------------------------------------------------------------
# sex 
sex_levels <- names(sort(table(comb_df$sex), decreasing = TRUE))
tbl_sex <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(sex, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, sex) %>%
  rename(var_name = sex) %>%
  mutate(var_name = factor(var_name, levels = sex_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Sex ", var_name, " (n (%))"))
tbl_sex[is.na(tbl_sex)] <- "0 (0.0%)"
tbl_sex


# ------------------------------------------------------------------------------
# ethnicity 
ethnicity_levels <- names(sort(table(comb_df$ethnicity), decreasing = TRUE))
tbl_ethnicity <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(ethnicity, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, ethnicity) %>%
  rename(var_name = ethnicity) %>%
  mutate(var_name = factor(var_name, levels = ethnicity_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Ethnicity ", var_name, " (n (%))"))
tbl_ethnicity[is.na(tbl_ethnicity)] <- "0 (0.0%)"
tbl_ethnicity


# ------------------------------------------------------------------------------
# race 
race_levels <- names(sort(table(comb_df$race), decreasing = TRUE))
tbl_race <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(race, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, race) %>%
  rename(var_name = race) %>%
  mutate(var_name = factor(var_name, levels = race_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Race ", var_name, " (n (%))"))
tbl_race[is.na(tbl_race)] <- "0 (0.0%)"
tbl_race


# ------------------------------------------------------------------------------
# device_os 
device_os_levels <- names(sort(table(comb_df$device_os), decreasing = TRUE))
tbl_device_os <- 
  comb_df %>%
  mutate(cnt_all = n()) %>%
  group_by(device_os, cnt_all) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(value_f, device_os) %>%
  rename(var_name = device_os) %>%
  mutate(var_name = factor(var_name, levels = device_os_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("device_os ", var_name, " (n (%))"))
tbl_device_os[is.na(tbl_device_os)] <- "0 (0.0%)"
tbl_device_os


# ------------------------------------------------------------------------------
# alsfrsrse_baseline -- mean_sd
tbl_alsfrsrse_baseline_mean_sd <- 
  comb_df %>%
  summarise(
    value_mean = mean(alsfrsrse_baseline),
    value_sd = sd(alsfrsrse_baseline)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Baseline ALSFRS-RSE Mean (SD)", .before = everything())
tbl_alsfrsrse_baseline_mean_sd


# alsfrsrse_baseline -- median_min_max
tbl_alsfrsrse_baseline_median_min_max <- 
  comb_df %>%
  summarise(
    value_median = median(alsfrsrse_baseline),
    value_min = min(alsfrsrse_baseline),
    value_max = max(alsfrsrse_baseline)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(value_f) %>%
  mutate(var_name = "Baseline ALSFRS-RSE Median [min, max]", .before = everything())
tbl_alsfrsrse_baseline_median_min_max



# ------------------------------------------------------------------------------
# combine altogether 

tbl_out <- 
  tbl_cnt %>%
  rbind(tbl_age_mean_sd) %>%
  rbind(tbl_age_median_min_max) %>%
  rbind(tbl_sex) %>%
  rbind(tbl_ethnicity) %>%
  rbind(tbl_race) %>%
  rbind(tbl_device_os) %>%
  rbind(tbl_alsfrsrse_baseline_mean_sd) %>%
  rbind(tbl_alsfrsrse_baseline_median_min_max) 
tbl_out


# ------------------------------------------------------------------------------
# save to file: table 1
# ------------------------------------------------------------------------------

tbl_out_path <- file.path(here(), 'results_tables', "table_1.csv")
fwrite(tbl_out, tbl_out_path)

# view(tbl_out)


