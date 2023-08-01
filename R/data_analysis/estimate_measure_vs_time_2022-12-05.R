
#' @description 
#' Estimate models that quantify measures change over time.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(here)
library(ggsci)
library(lme4)
library(lmerTest)
library(cowplot)
library(broom.mixed)
library(MuMIn)
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


# ------------------------------------------------------------------------------
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
# dat_acc_oak_path <- file.path(here(), "data_beiwe_processed", "accelerometer", "beiwe_oak_t24hr_allwalking_cond10sec.rds")
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
# make a combined data set
# ------------------------------------------------------------------------------

# prepare data sets for joining
dat_acc_ai_JOIN <- dat_acc_ai %>% rename(time_date = et_time_date)
dat_acc_oak_JOIN <- dat_acc_oak %>% rename(time_date = et_time_date) %>% select(-c(valid_minutes, phone_worn_minutes))
dat_gps_JOIN <- dat_gps %>% rename(time_date = local_time_date) %>% mutate(time_date = as.Date(time_date))
sample_beiwe_id_JOIN <- sample_beiwe_id %>% select(beiwe_id, date_min_meta, date_max_meta, beiwe_id_fct)
dat_comb <- 
  dat_acc_ai_JOIN %>%
  full_join(dat_acc_oak_JOIN) %>%
  full_join(dat_gps_JOIN) %>%
  full_join(sample_beiwe_id_JOIN) %>% 
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(time_date, min(time_date), units = c("days"))),
    month_relative = day_relative/30.5,
    day_relative_true = as.numeric(difftime(time_date, date_min_meta, units = c("days"))),
    month_relative_true = day_relative_true/30.5,
  ) %>%
  ungroup() 
  
# check
nrow(dat_comb)
nrow(dat_comb) == dat_comb %>% select(time_date, beiwe_id) %>% distinct() %>% nrow()
summary(dat_comb$day_relative)
summary(dat_comb$day_relative_true)


# ------------------------------------------------------------------------------
# other mutate
# ------------------------------------------------------------------------------

dat_comb <- dat_comb %>% mutate(home_time = ifelse(home_time < 8, NA, home_time))


# ------------------------------------------------------------------------------
# generate indicator variables "whether or not higher than at baseline" 
# ------------------------------------------------------------------------------

Y_NAME_VEC_TO_RELATIVEPROB <- c(
  "dist_traveled",
  "ai",
  "ai_max_1",
  "steps_cnt",
  "steps_cnt_max_1"
)

# create participant-specific baseline period subset df

dat_comb2 <- dat_comb
for (y_name in Y_NAME_VEC_TO_RELATIVEPROB){ # y_name <- "dist_traveled"
  # create participant-specific baseline for that variable
  dat_comb_bsln <- 
    dat_comb2 %>% 
    rename_with(~ c("y"), all_of(y_name)) %>%
    filter(!is.na(y)) %>% 
    group_by(beiwe_id) %>%
    mutate(day_relative = as.numeric(difftime(time_date, min(time_date), units = c("days")))) %>%
    filter(day_relative <= 27) %>%
    summarise(
      !!paste0(y_name, "_bsln_pt25") := quantile(y, prob = 0.25),
      !!paste0(y_name, "_bsln_pt50") := quantile(y, prob = 0.5),
      !!paste0(y_name, "_bsln_pt75") := quantile(y, prob = 0.75)
    ) %>%
    ungroup()
  dat_comb2 <- dat_comb2 %>% left_join(dat_comb_bsln) %>% as.data.frame()
  dat_comb2[, paste0(y_name, "_above_bsln_pt25")] = as.numeric(dat_comb2[, y_name] > dat_comb2[, paste0(y_name, "_bsln_pt25")])
  dat_comb2[, paste0(y_name, "_above_bsln_pt50")] = as.numeric(dat_comb2[, y_name] > dat_comb2[, paste0(y_name, "_bsln_pt50")])
  dat_comb2[, paste0(y_name, "_above_bsln_pt75")] = as.numeric(dat_comb2[, y_name] > dat_comb2[, paste0(y_name, "_bsln_pt75")])
}

summary(dat_comb2$dist_traveled_above_bsln_pt25)
summary(dat_comb2$dist_traveled_above_bsln_pt50)
summary(dat_comb2$dist_traveled_above_bsln_pt75)


# ------------------------------------------------------------------------------
# generate log-transformed variables
# ------------------------------------------------------------------------------

Y_NAME_VEC_TO_LOGTRANSFORM <- c(
  "dist_traveled",
  "ai",
  "ai_max_1",
  "steps_cnt",
  "steps_cnt_max_1"
)

for (y_name in Y_NAME_VEC_TO_LOGTRANSFORM){ # y_name <- "dist_traveled"
  vec_tmp <- dat_comb2 %>% pull(y_name)
  vec_tmp_log <- log(vec_tmp + 1)
  dat_comb2 <- mutate(dat_comb2, !!paste0("log_", y_name) := vec_tmp_log)
}


# ------------------------------------------------------------------------------
# define vectors of responses 
# ------------------------------------------------------------------------------

Y_LABEL_VEC_TO_RELATIVEPROB <- c(
  "Distance travelled [km]",
  "Activity Index",
  "Activity Index -- top 1 minute",
  "Steps count",
  "Steps count -- top 1 minute"
)

y_name_vec_lmm <- c(
  "home_time",
  "cadence",
  "cadence_max_1",
  paste0("log_", Y_NAME_VEC_TO_LOGTRANSFORM)
)
y_label_vec_lmm <- c(
  "Home time [hours]",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] -- top 1 min",
  paste0("log(", Y_LABEL_VEC_TO_RELATIVEPROB, ")")
)


val_tmp <- lapply(Y_NAME_VEC_TO_RELATIVEPROB, function(val) {
  paste0(val, c("_above_bsln_pt25", "_above_bsln_pt50", "_above_bsln_pt75"))
})
y_name_vec_glmm_binom <- unlist(val_tmp)

val_tmp <- lapply(Y_LABEL_VEC_TO_RELATIVEPROB, function(val) {
  paste0("P(", val, " > ", c("bsln pt25", "bsln pt50", "bsln pt75"), ')')
})
y_label_vec_glmm_binom <- unlist(val_tmp)


# ------------------------------------------------------------------------------
# final data frame
# ------------------------------------------------------------------------------

dat_comb_F <- dat_comb2; rm(dat_comb, dat_comb2)


# ------------------------------------------------------------------------------
# fit models: LMM  
# ------------------------------------------------------------------------------

dat = dat_comb_F
y_name_vec <- y_name_vec_lmm
y_label_vec <- y_label_vec_lmm
x_name <- "month_relative"
x_label <- "Time [months]"
mod_formula <- as.formula("y ~ x  + (1 + x | beiwe_id)")
y_scale = FALSE
x_scale = FALSE

put_list <- get_lmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out_1 <- put_list$out_df
mod_out_1


# ------------------------------------------------------------------------------
# fit models: GLMM binomial 
# ------------------------------------------------------------------------------

dat = dat_comb_F
y_name_vec <- y_name_vec_glmm_binom
y_label_vec <- y_label_vec_glmm_binom
x_name <- "month_relative"
x_label <- "Time [months]"
mod_formula <- as.formula("y ~ x  + (1 + x | beiwe_id)")

t1 <- Sys.time()
mod_out_2 <- get_glmm_yvec_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label,
                                mod_formula, y_scale = FALSE, x_scale = FALSE,
                                mod_family = "binomial", y_fct = TRUE)
mod_out_2
t2 <- Sys.time()
t2-t1


# ------------------------------------------------------------------------------
# save
# ------------------------------------------------------------------------------

# save table to file
mod_out <- rbind(mod_out_1, mod_out_2)
fwrite(mod_out, file.path(here(), "results_tables", "table_lmm_estimate_measure_vs_time.csv"))

# save data objects with values from the model to use them in plotting in a separate script
saveRDS(put_list$plt_df_subj_all, file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_time_plt_df_subj_all.rds"))
saveRDS(put_list$plt_df_popul_all, file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_time_plt_df_popul_all.rds"))



