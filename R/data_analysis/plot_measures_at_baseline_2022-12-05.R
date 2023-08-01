
#' @description 
#' Plot Beiwe data-derived measures at a baseline (4 weeks after the first survey). 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(here)
library(ggsci)
options(digits.secs = 0)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
source(file.path(here(), "R", "data_analysis", "utils.R"))
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# study analysis sample: Beiwe ID 
sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id_with_factor.rds")
sample_beiwe_id <- readRDS(sample_beiwe_id_path) 
dim(sample_beiwe_id)
head(sample_beiwe_id)

# study analysis sample: Beiwe acc
sample_acc_path <- file.path(here(), "results_objects", "analysis_sample_acc_et_time_date.rds")
sample_acc <- readRDS(sample_acc_path) %>% select(beiwe_id, et_time_date)
dim(sample_acc)
length(unique(sample_acc$beiwe_id))

# study analysis sample: Beiwe GPS
sample_gps_path <- file.path(here(), "results_objects", "analysis_sample_gps_local_time_date.rds")
sample_gps <- readRDS(sample_gps_path) %>% select(beiwe_id, local_time_date)
dim(sample_gps)
length(unique(sample_gps$beiwe_id))

# survey data
dat_alsfrsrse_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
dat_alsfrsrse <- 
  fread(dat_alsfrsrse_path) %>% 
  as.data.frame() %>%
  filter(beiwe_id %in% sample_beiwe_id$beiwe_id)

# daily measures: Beiwe acc AI 
dat_acc_ai_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_ai_t24hr.rds")
dat_acc_ai <- 
  readRDS(dat_acc_ai_path) %>%
  inner_join(sample_acc, by = c("beiwe_id", "et_time_date"))
dim(dat_acc_ai)
length(unique(dat_acc_ai$beiwe_id))

# daily measures: Beiwe acc walking 
dat_acc_oak_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_oak_t24hr_et_sustainedharmonic_COND_10.rds")
dat_acc_oak <- 
  readRDS(dat_acc_oak_path) %>%
  inner_join(sample_acc, by = c("beiwe_id", "et_time_date"))
dim(dat_acc_oak)
length(unique(dat_acc_oak$beiwe_id))

# daily measures: Beiwe GPS
dat_gps_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_processed_daily.csv")
dat_gps <- 
  fread(dat_gps_path) %>%
  as.data.frame() %>%
  inner_join(sample_gps, by = c("beiwe_id", "local_time_date"))
dim(dat_gps)
length(unique(dat_gps$beiwe_id))


# ------------------------------------------------------------------------------
# make data baseline data sets which use 28 days of data after the first survey 
# ------------------------------------------------------------------------------

DAYS_DIFF_MIN = 0 
DAYS_DIFF_MAX = 27

# survey data baseline
dat_alsfrsrse_bsln <- 
  dat_alsfrsrse %>% 
  group_by(beiwe_id) %>%
  filter(et_time_date == min(et_time_date)) %>%
  ungroup() %>%
  select(beiwe_id, survey_et_time_date_bsln = et_time_date, frs_total_score_bsln = frs_total_score)

# daily measures: Beiwe acc AI 
dat_acc_ai_bsln <- 
  dat_acc_ai %>%
  left_join(dat_alsfrsrse_bsln, by = "beiwe_id") %>%
  ungroup() %>%
  mutate(days_diff = as.numeric(as.Date(et_time_date) - as.Date(survey_et_time_date_bsln))) %>%
  filter(days_diff >= DAYS_DIFF_MIN, days_diff <= DAYS_DIFF_MAX)
dim(dat_acc_ai_bsln)
length(unique(dat_acc_ai_bsln$beiwe_id))

# daily measures: Beiwe acc walking 
dat_acc_oak_bsln <- 
  dat_acc_oak %>%
  left_join(dat_alsfrsrse_bsln, by = "beiwe_id") %>%
  ungroup() %>%
  mutate(days_diff = as.numeric(as.Date(et_time_date) - as.Date(survey_et_time_date_bsln))) %>%
  filter(days_diff >= DAYS_DIFF_MIN, days_diff <= DAYS_DIFF_MAX)
dim(dat_acc_oak_bsln)
length(unique(dat_acc_oak_bsln$beiwe_id))

# daily measures: Beiwe GPS
dat_gps_bsln <- 
  dat_gps %>%
  left_join(dat_alsfrsrse_bsln, by = "beiwe_id") %>%
  ungroup() %>%
  mutate(days_diff = as.numeric(as.Date(local_time_date) - as.Date(survey_et_time_date_bsln))) %>%
  filter(days_diff >= DAYS_DIFF_MIN, days_diff <= DAYS_DIFF_MAX)
dim(dat_gps_bsln)
length(unique(dat_gps_bsln$beiwe_id))


# ------------------------------------------------------------------------------
# define set of Beiwe IDS which is a union of the above data frames 
# ------------------------------------------------------------------------------

# beiwe_id_set <- unique(c(dat_acc_ai_bsln$beiwe_id, dat_acc_oak_bsln$beiwe_id, dat_gps_bsln$beiwe_id))

# redefine beiwe id levels
sample_beiwe_id_bsln <- 
  sample_beiwe_id %>%
  # filter(beiwe_id %in% beiwe_id_set) %>%
  left_join(dat_alsfrsrse_bsln) %>%
  arrange(frs_total_score_bsln) %>%
  mutate(beiwe_id_label_trim = gsub("ID ", "", as.character(beiwe_id_fct))) %>% 
  mutate(beiwe_id_fct2 = factor(beiwe_id_label_trim, levels = .$beiwe_id_label_trim)) %>%
  select(beiwe_id, beiwe_id_fct2)
levels(sample_beiwe_id_bsln$beiwe_id_fct2)
length(levels(sample_beiwe_id_bsln$beiwe_id_fct2))

# add beiwe ID factor to the data frames
dat_acc_ai_bsln2 <- dat_acc_ai_bsln %>% left_join(sample_beiwe_id_bsln)
dat_acc_oak_bsln2 <- dat_acc_oak_bsln %>% left_join(sample_beiwe_id_bsln)
dat_gps_bsln2 <- dat_gps_bsln %>% left_join(sample_beiwe_id_bsln)


# ------------------------------------------------------------------------------
# generate plot
# ------------------------------------------------------------------------------

plt_list <- list()
plt_df_list <- list()

# ------------------------------------------------------------------------------
# PLOT 1

plt_df <- 
  dat_gps_bsln2 %>% 
  filter(home_time > 8) %>% 
  rename(y = home_time) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  scale_y_continuous(breaks = seq(8, 24, by = 4)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[1]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "home_time")


# ------------------------------------------------------------------------------
# PLOT 2

plt_df <- 
  dat_gps_bsln2 %>% 
  rename(y = dist_traveled) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[2]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "dist_traveled")



# ------------------------------------------------------------------------------
# PLOT 3

plt_df <- 
  dat_acc_ai_bsln2 %>% 
  rename(y = ai) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  # scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[3]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "ai")


# ------------------------------------------------------------------------------
# PLOT 4

plt_df <- 
  dat_acc_ai_bsln2 %>% 
  rename(y = ai_max_1) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  # scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[4]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))
# plt

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "ai_max_1")


# ------------------------------------------------------------------------------
# PLOT 5

plt_df <- 
  dat_acc_oak_bsln2 %>% 
  rename(y = cadence) %>%
  filter(!is.na(y)) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  scale_y_continuous(breaks = seq(1.5, 2.1, by = 0.1), limits = c(1.5, 2.1)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[5]) + 
  guides(color = "none") +
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))
# plt

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "cadence")


# ------------------------------------------------------------------------------
# PLOT 6

plt_df <- 
  dat_acc_oak_bsln2 %>% 
  rename(y = cadence_max_1) %>%
  filter(!is.na(y)) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  scale_y_continuous(breaks = seq(1.5, 2.1, by = 0.1), limits = c(1.5, 2.1)) +
  labs(x = "", y ="", title =  Y_LABEL_VEC_MEASURES[6]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))
# plt

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "cadence_max_1")


# ------------------------------------------------------------------------------
# PLOT 7

plt_df <- 
  dat_acc_oak_bsln2 %>% 
  rename(y = steps_cnt) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  # scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  labs(x = "Participant ID", y ="", title =  Y_LABEL_VEC_MEASURES[7], color = "ALSFRS-RSE") + 
  # guides(color = "none") + 
  scale_color_gsea(reverse=TRUE, breaks = seq(15, 50, by = 5), limits = c(19, 48)) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10), 
        legend.position = c(0.33, 0.83), legend.direction = "horizontal")
# plt

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "steps_cnt")


# ------------------------------------------------------------------------------
# PLOT 8

plt_df <- 
  dat_acc_oak_bsln2 %>% 
  rename(y = steps_cnt_max_1) %>%
  group_by(beiwe_id_fct2, frs_total_score_bsln) %>%
  summarise(y_median = median(y), y_iqr_lo = quantile(y, prob = 0.25), y_iqr_up = quantile(y, prob = 0.75), cnt = n()) %>%
  ungroup()
summary(plt_df$cnt)
sort(plt_df$cnt)

plt <- 
  plt_df %>%
  ggplot() + 
  geom_segment(aes(x = beiwe_id_fct2, xend = beiwe_id_fct2, y = y_iqr_lo, yend = y_iqr_up, color = frs_total_score_bsln), 
               size = 2.1) + 
  geom_point(aes(x = beiwe_id_fct2, y_median), size = 1.7) + 
  scale_x_discrete(guide = guide_axis(angle = 90), drop = FALSE) + 
  # scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  labs(x = "Participant ID", y ="", title =  Y_LABEL_VEC_MEASURES[8]) + 
  guides(color = "none") + 
  scale_color_gsea(reverse=TRUE) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        plot.title = element_text(size = 12), 
        axis.title.x = element_text(size = 10))

plt_list[[length(plt_list) + 1]] <- plt
plt_df_list[[length(plt_df_list) + 1]] <- plt_df %>% mutate(y_name = "steps_cnt_max_1")



# ------------------------------------------------------------------------------
# combine plots
# ------------------------------------------------------------------------------

plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "measures_at_baseline.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10, dpi = 150)



# ------------------------------------------------------------------------------
# combine plots tables
# ------------------------------------------------------------------------------

plt_df_all <- rbindlist(plt_df_list) %>% as.data.frame()
tbl_out <- 
  plt_df_all %>% 
  rename(value = y_median) %>%
  group_by(y_name) %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value),
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.2f", value_mean), " (", sprintf("%.2f", value_sd), "), ", sprintf("%.2f", value_median), " [", sprintf("%.2f", value_min), ", ", sprintf("%.2f", value_max), "]")
  ) %>%
  mutate(y_label = factor(y_name, levels = Y_NAME_VEC_MEASURES, labels = Y_LABEL_VEC_MEASURES)) %>%
  arrange(y_label) %>%
  mutate(y_label = paste0(y_label, " (median)")) %>%
  select(y_label, value_f)
tbl_out

# View(tbl_out)

# ------------------------------------------------------------------------------
# save to file

tbl_out_path <- file.path(here(), 'results_tables', "table_measures_at_baseline.csv")
fwrite(tbl_out, tbl_out_path)

