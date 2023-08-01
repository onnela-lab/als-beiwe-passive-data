
#' @description 
#' Plot showing Beiwe accelerometry days -- included vs excluded

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(here)
options(digits.secs = 0)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# analysis sample: participants ID with meta data 
analysis_sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id_with_factor.rds")
analysis_sample_beiwe_id <- readRDS(analysis_sample_beiwe_id_path) 
dim(analysis_sample_beiwe_id)
head(analysis_sample_beiwe_id)

# acc data t24hr
beiwe_ai_t24hr_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
beiwe_ai_t24hr <- readRDS(beiwe_ai_t24hr_path) 

# analysis sample: acc data 
analysis_sample_acc_path <- file.path(here(), "results_objects", "analysis_sample_acc_et_time_date.rds")
analysis_sample_acc <- readRDS(analysis_sample_acc_path) 
head(analysis_sample_acc)

# ------------------------------------------------------------------------------
# prepare data
# ------------------------------------------------------------------------------

# make a data frame with number of valid_minutes per participant-day
plt_df_points0 <- 
  beiwe_ai_t24hr %>%
  inner_join(analysis_sample_beiwe_id) %>%
  filter(et_time_date >= date_min_meta) %>%
  filter(et_time_date <= date_max_meta) %>%
  select(beiwe_id, et_time_date, valid_minutes)
head(plt_df_points0)

# prepare to expand data to a long form 
date_grid_list <- list()
for (i in 1 : nrow(analysis_sample_beiwe_id)){ # i <- 1
  print(i)
  beiwe_id_i <- analysis_sample_beiwe_id$beiwe_id[i]
  et_time_date_i <- seq(
    as.Date(analysis_sample_beiwe_id$date_min_meta[i]),
    as.Date(analysis_sample_beiwe_id$date_max_meta[i]),
    by = "1 day"
  )
  out_df_i <- data.frame(et_time_date = et_time_date_i)
  out_df_i$beiwe_id = beiwe_id_i
  date_grid_list[[i]] <- out_df_i
}
date_grid_df <- rbindlist(date_grid_list) %>% as.data.frame()
head(date_grid_df)

# expand long form data, fill up NAs with 0 valid minutes
plt_df_points1 <- 
  date_grid_df %>%
  left_join(plt_df_points0) %>%
  mutate(valid_minutes = ifelse(is.na(valid_minutes), 0, valid_minutes))
!any(is.na(plt_df_points1))

# add the meta data info
plt_df_points2 <-
  plt_df_points1 %>%
  inner_join(analysis_sample_beiwe_id %>% select(-obs_duration)) %>% 
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(et_time_date, min(et_time_date), units = c("days"))),
    day_relative_true = as.numeric(difftime(et_time_date, date_min_meta, units = c("days")))
  ) %>%
  ungroup() 
head(plt_df_points2)
!any(is.na(plt_df_points2))

# get participant-specific band of number of valid minutes 
plt_df_bands <- 
  plt_df_points0 %>% 
  filter(valid_minutes > 0) %>%
  group_by(beiwe_id) %>%
  summarise(valid_minutes_mean = mean(valid_minutes)) %>%
  ungroup() %>%
  mutate(
    valid_minutes_lo = valid_minutes_mean - ACC_VALID_MINUTES_BAND_EPSILON,
    valid_minutes_up = valid_minutes_mean + ACC_VALID_MINUTES_BAND_EPSILON
  ) %>% 
  inner_join(analysis_sample_beiwe_id %>% select(beiwe_id, beiwe_id_fct))

# add information whether a day is included/excluded from statistics analysis
# (is in the participant-specific band of number of valid minutes , but also 
# meets other criteria defined earlier)
analysis_sample_acc_tojoin <- 
  analysis_sample_acc %>%
  select(beiwe_id, et_time_date) %>%
  mutate(is_included = 1)

plt_df_points3 <- 
  plt_df_points2 %>%
  left_join(analysis_sample_acc_tojoin) %>%
  mutate(is_included = ifelse(is.na(is_included), 0, is_included)) %>%
  mutate(is_included_fct = factor(is_included, levels = c(0,1), labels = c("Excluded", "Included")))
!any(is.na(plt_df_points3))

plt_df_points_F <- plt_df_points3


# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

y_breaks <- seq(0, 1440, by = 60 * 6)
x_breaks <- seq(0, 12, by = 2)

plt <- 
  ggplot() + 
  geom_point(data = plt_df_points_F, aes(x = day_relative_true / 30.5, y = valid_minutes, color = is_included_fct),
             size = 0.3, alpha = 0.5) + 
  geom_hline(data = plt_df_bands, aes(yintercept = valid_minutes_mean),
             size = 0.3, linetype = 1) + 
  geom_hline(data = plt_df_bands, aes(yintercept = valid_minutes_lo),
            size = 0.3, linetype = 2) + 
  geom_hline(data = plt_df_bands, aes(yintercept = valid_minutes_up),
            size = 0.3, linetype = 2) + 
  # scale_color_manual(values = c("blue", "black")) +
  scale_color_manual(values = c("red", "black")) +
  facet_wrap(~ beiwe_id_fct, ncol = 7) + 
  scale_y_continuous(breaks = y_breaks) + 
  scale_x_continuous(breaks = x_breaks) + 
  labs(
    x = "Relative time [months]",
    y = "Valid minutes per day",
    color = "Analysis sample: "
  ) + 
  theme(
    panel.background = element_rect(fill = "grey95", colour = "grey95", size = 0.5, linetype = "solid"),
    # panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.major = element_blank(),
    legend.position = "bottom"
  ) + 
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme(strip.text.x = element_text(margin = margin(0.05, 0, 0.05, 0, "cm")))
# plt


plt_fpath <- file.path(here(), "results_figures", "acc_valid_minutes_over_time_t24hr.jpeg")
ggsave(plt_fpath, plot = plt, width = 10, height = 8, units = "in", dpi = 150)


  
  
