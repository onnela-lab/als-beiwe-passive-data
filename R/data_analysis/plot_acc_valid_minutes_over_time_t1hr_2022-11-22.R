
#' @description 
#' Plot showing Beiwe accelerometry days with hour-level information about the 
#' # number of valid minutes  

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
# read and prepare data
# ------------------------------------------------------------------------------

# study analysis sample 
dat_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
dat_beiwe_id <- readRDS(dat_beiwe_id_path) 
dim(dat_beiwe_id)
head(dat_beiwe_id)

# acc data 
beiwe_ai_t1min_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t1min_et.rds")
beiwe_ai_t1min <- readRDS(beiwe_ai_t1min_path) 
plt_df0 <- 
  beiwe_ai_t1min %>%
  inner_join(dat_beiwe_id) %>%
  filter(et_time_date >= date_min_meta) %>%
  filter(et_time_date <= date_max_meta) %>%
  mutate(et_time_hour = floor_date(et_time, "hour"))

# aggregate at day-hour level
plt_df1 <- 
  plt_df0 %>% 
  # exclude: date_min_meta, date_max_meta on purpose
  group_by(beiwe_id, et_time_hour, et_time_date) %>%
  summarise(valid_minutes = sum(valid_minute)) %>%
  ungroup()
  
# expand to a long form 
time_date_grid_list <- list()
for (i in 1 : nrow(dat_beiwe_id)){ # i <- 1
  print(i)
  beiwe_id_i <- dat_beiwe_id$beiwe_id[i]
  et_time_hour_i <- seq(
    ymd_hms(paste0(dat_beiwe_id$date_min_meta[i], " 00:00:00")),
    ymd_hms(paste0(dat_beiwe_id$date_max_meta[i], " 23:00:00")),
    by = "1 hour"
  )
  out_df_i <- data.frame(et_time_hour = et_time_hour_i)
  out_df_i$beiwe_id = beiwe_id_i
  time_date_grid_list[[i]] <- out_df_i
}
time_date_grid_df <- rbindlist(time_date_grid_list) %>% as.data.frame()

# join with aggregated day-hour level data
plt_df2 <- 
  time_date_grid_df %>%
  left_join(plt_df1) %>%
  mutate(valid_minutes = ifelse(is.na(valid_minutes), 0, valid_minutes)) %>%
  mutate(et_time_date = as.Date(et_time_hour)) %>%
  inner_join(dat_beiwe_id %>% select(-obs_duration))
# check
nrow(plt_df2) == nrow(time_date_grid_df)
!any(is.na(plt_df2))
head(plt_df2)

# add additional variables
plt_df3 <- 
  plt_df2 %>% 
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(et_time_date, min(et_time_date), units = c("days"))),
    day_relative_true = as.numeric(difftime(et_time_date, date_min_meta, units = c("days"))),
    et_time_hour2 = hour(et_time_hour)
  ) %>%
  ungroup() 
# check
head(plt_df3) %>% as.data.frame()
!any(is.na(plt_df3))


# ------------------------------------------------------------------------------
# define plot label ID 
# ------------------------------------------------------------------------------

# this is the first plot and it will guide the definition of anonymized "Participant ID" 
# and its ordering 

valid_minutes_global_mean_df <-
  plt_df3 %>% 
  group_by(beiwe_id) %>%
  summarise(valid_minutes_global_mean = round(mean(valid_minutes), 3)) %>%
  ungroup()

dat_beiwe_id_ext <- 
  dat_beiwe_id %>%
  inner_join(valid_minutes_global_mean_df) %>%
  arrange(desc(valid_minutes_global_mean)) %>%
  mutate(beiwe_id_fct = paste0("ID ", row_number())) %>%
  mutate(beiwe_id_fct = factor(beiwe_id_fct, levels = .$beiwe_id_fct))
# save
saveRDS(dat_beiwe_id_ext, file.path(here(), "results_objects", "analysis_sample_beiwe_id_with_factor.rds"))

# add beiwe ID factor to the data 
plt_df4 <- 
  plt_df3 %>% 
  inner_join(dat_beiwe_id_ext %>% select(beiwe_id, beiwe_id_fct))


# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

y_breaks <- seq(0, 24, by = 6)
x_breaks <- seq(0, 12, by = 2)

plt <- 
  ggplot() + 
  geom_tile(data = plt_df4, aes(x = day_relative_true / 30.5, y = et_time_hour2, fill = valid_minutes)) + 
  facet_wrap(~ beiwe_id_fct, ncol = 7) + 
  scale_y_continuous(breaks = y_breaks) + 
  scale_x_continuous(breaks = x_breaks) + 
  labs(
    x = "Relative time [months]",
    y = "Hour of a day",
    fill = "Valid minutes per hour: "
  ) + 
  theme(
    panel.background = element_rect(fill = "grey95", colour = "grey95", size = 0.5, linetype = "solid"),
    # panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.major = element_blank(), 
    legend.position = "bottom"
  ) + 
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme(strip.text.x = element_text(margin = margin(0.05, 0, 0.05, 0, "cm"))) + 
  scale_fill_continuous(low = "yellow", high = "darkblue")
# plt

plt_fpath <- file.path(here(), "results_figures", "acc_valid_minutes_over_time_t1hr.jpeg")
# ggsave(plt_fpath, plot = plt, width = 10, height = 7.2, units = "in", dpi = 150)
ggsave(plt_fpath, plot = plt, width = 10, height = 8, units = "in", dpi = 150)


