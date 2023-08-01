
#' @description 
#' Plot showing Beiwe GPS days: included vs excluded in statistical analysis sample

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
beiwe_gps_t24hr_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_processed_daily.csv")
beiwe_gps_t24hr <- fread(beiwe_gps_t24hr_path) %>% as.data.frame()

# analysis sample: acc data 
analysis_sample_gps_path <- file.path(here(), "results_objects", "analysis_sample_gps_local_time_date.rds")
analysis_sample_gps <- readRDS(analysis_sample_gps_path) %>% select(beiwe_id, local_time_date)
head(analysis_sample_gps)

sort(beiwe_gps_t24hr$obs_duration, decreasing = TRUE)

summary(beiwe_gps_t24hr$obs_duration)


# ------------------------------------------------------------------------------
# prepare data
# ------------------------------------------------------------------------------


# make a data frame with number of valid_minutes per participant-day
plt_df_points0 <- 
  beiwe_gps_t24hr %>%
  rename(gps_obs_duration = obs_duration) %>%
  inner_join(analysis_sample_beiwe_id) %>%
  filter(local_time_date >= date_min_meta) %>%
  filter(local_time_date <= date_max_meta) %>%
  select(beiwe_id, local_time_date, gps_obs_duration)
head(plt_df_points0)

# prepare to expand data to a long form 
date_grid_list <- list()
for (i in 1 : nrow(analysis_sample_beiwe_id)){ # i <- 1
  print(i)
  beiwe_id_i <- analysis_sample_beiwe_id$beiwe_id[i]
  local_time_date_i <- seq(
    as.Date(analysis_sample_beiwe_id$date_min_meta[i]),
    as.Date(analysis_sample_beiwe_id$date_max_meta[i]),
    by = "1 day"
  )
  out_df_i <- data.frame(local_time_date = local_time_date_i)
  out_df_i$beiwe_id = beiwe_id_i
  date_grid_list[[i]] <- out_df_i
}
date_grid_df <- rbindlist(date_grid_list) %>% as.data.frame()
head(date_grid_df)

# expand long form data, fill up NAs with 0 valid minutes
plt_df_points1 <- 
  date_grid_df %>%
  left_join(plt_df_points0 %>% mutate(local_time_date = as.Date(local_time_date))) %>%
  mutate(gps_obs_duration = ifelse(is.na(gps_obs_duration), 0, gps_obs_duration))
!any(is.na(plt_df_points1))

# add the meta data info
plt_df_points2 <-
  plt_df_points1 %>%
  inner_join(analysis_sample_beiwe_id %>% select(-obs_duration)) %>% 
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    day_relative_true = as.numeric(difftime(local_time_date, date_min_meta, units = c("days")))
  ) %>%
  ungroup() 
head(plt_df_points2)
!any(is.na(plt_df_points2))

# add information whether a day is included/excluded from statistics analysis
# (is in the participant-specific band of number of valid minutes , but also 
# meets other criteria defined earlier)
analysis_sample_gps_tojoin <- 
  analysis_sample_gps %>%
  select(beiwe_id, local_time_date) %>%
  mutate(is_included = 1) %>%
  mutate(local_time_date = as.Date(local_time_date))

plt_df_points3 <- 
  plt_df_points2 %>%
  left_join(analysis_sample_gps_tojoin) %>%
  mutate(is_included = ifelse(is.na(is_included), 0, is_included)) %>%
  mutate(is_included_fct = factor(is_included, levels = c(0,1), labels = c("Excluded", "Included")))
!any(is.na(plt_df_points3))

plt_df_points_F <- plt_df_points3


# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

y_breaks <- seq(0, 60 * 4, by = 60 * 1)
y_limits <- c(0, 60 * 4)
x_breaks <- seq(0, 12, by = 2)

plt <- 
  ggplot() + 
  geom_point(data = plt_df_points_F, aes(x = day_relative_true / 30.5, y = gps_obs_duration * 60, color = is_included_fct),
             size = 0.3, alpha = 0.5) + 
  scale_color_manual(values = c("red", "black")) + 
  facet_wrap(~ beiwe_id_fct, ncol = 7) + 
  scale_y_continuous(limits = y_limits, breaks = y_breaks) +
  scale_x_continuous(breaks = x_breaks) + 
  labs(
    x = "Relative time [months]",
    y = "GPS data collection minutes per day",
    color = "Analysis sample: "
  ) + 
  theme(
    panel.background = element_rect(fill = "grey95", colour = "grey95", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    legend.position = "bottom"
  ) + 
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme(strip.text.x = element_text(margin = margin(0.05, 0, 0.05, 0, "cm"))) 
# plt


plt_fpath <- file.path(here(), "results_figures", "gps_valid_minutes_over_time_t24hr.jpeg")
ggsave(plt_fpath, plot = plt, width = 10, height = 8, units = "in", dpi = 150)


