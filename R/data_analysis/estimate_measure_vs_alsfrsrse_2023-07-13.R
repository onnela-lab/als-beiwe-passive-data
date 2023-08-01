
#' @description 
#' Quantify association between ALSFRS-RSE outcome and Beiwe data-derived measures.

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
  filter(beiwe_id %in% sample_beiwe_id$beiwe_id) %>%
  mutate(
    frs_q123 = frs1 + frs2 + frs3,
    frs_q456 = frs4 + frs5 + frs6,
    frs_q789 = frs7 + frs8 + frs9,
    frs_q101112 = frs10 + frs11 + frs12
  ) 

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
dat_alsfrsrse_JOIN <- dat_alsfrsrse %>% 
  rename(survey_time_date = et_time_date) %>% 
  select(
    beiwe_id, 
    survey_time_date, 
    frs_total_score, 
    frs_q123,
    frs_q456,
    frs_q789,
    frs_q101112
)
  
# join 
# rename data date 
dat_comb <- 
  dat_acc_ai_JOIN %>%
  full_join(dat_acc_oak_JOIN) %>%
  full_join(dat_gps_JOIN) %>%
  full_join(sample_beiwe_id_JOIN) %>%
  rename(sensordata_time_date = time_date) %>%
  left_join(dat_alsfrsrse_JOIN) %>%
  mutate(
    days_diff = abs(as.numeric(difftime(sensordata_time_date, survey_time_date, units = c("days"))))
  ) %>%
  group_by(beiwe_id, sensordata_time_date) %>%
  arrange(days_diff) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(days_diff <= 10) 
  
# check
nrow(dat_comb)
# [1] 4980
nrow(dat_comb) == dat_comb %>% select(sensordata_time_date, beiwe_id) %>% distinct() %>% nrow()
length(unique(dat_comb$beiwe_id))


# ------------------------------------------------------------------------------
# other mutate
# ------------------------------------------------------------------------------

dat_comb <- dat_comb %>% mutate(home_time = ifelse(home_time < 8, NA, home_time))


# ------------------------------------------------------------------------------
# generate log-transformed variables
# ------------------------------------------------------------------------------

dat_comb2 <- dat_comb

Y_NAME_VEC_TO_LOGTRANSFORM <- c(
  "dist_traveled",
  "ai",
  "ai_max_1",
  "steps_cnt",
  "steps_cnt_max_1"
)

Y_LABEL_VEC_TO_LOGTRANSFORM <- c(
  "Distance travelled [km]",
  "Activity Index",
  "Activity Index from top 1 minute",
  "Step count",
  "Step count from top 1 minute"
)

for (y_name in Y_NAME_VEC_TO_LOGTRANSFORM){ # y_name <- "dist_traveled"
  vec_tmp <- dat_comb2 %>% pull(y_name)
  vec_tmp_log <- log(vec_tmp + 1)
  dat_comb2 <- mutate(dat_comb2, !!paste0("log_", y_name) := vec_tmp_log)
}


# ------------------------------------------------------------------------------
# define vectors of responses 
# ------------------------------------------------------------------------------

y_name_vec_lmm <- c(
  "home_time",
  "cadence",
  "cadence_max_1",
  paste0("log_", Y_NAME_VEC_TO_LOGTRANSFORM)
)
y_label_vec_lmm <- c(
  "Home time [hours]",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] from top 1 min",
  paste0("log(", Y_LABEL_VEC_TO_LOGTRANSFORM, ")")
)



# ------------------------------------------------------------------------------
# final data frame
# ------------------------------------------------------------------------------

dat_comb_F <- dat_comb2; rm(dat_comb, dat_comb2)


# ------------------------------------------------------------------------------
# fit models: util function
# ------------------------------------------------------------------------------

# function to fit models separately for each predictor listed in var_name_vec
get_lmm_yvecagg_vs_x <- function(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula, x_scale = FALSE, y_scale = FALSE){
 
  pp <- length(y_name_vec)
  # objects to store model estimation results
  out_intcp_est <- rep(NA, pp)
  out_slope_est <- rep(NA, pp)
  out_intcp_pval <- rep(NA, pp)
  out_slope_pval <- rep(NA, pp)
  out_intcp_ci_lo <- rep(NA, pp)
  out_intcp_ci_up <- rep(NA, pp)
  out_slope_ci_up <- rep(NA, pp)
  out_slope_ci_lo <- rep(NA, pp)
  out_R2m <- rep(NA, pp)
  out_R2c <- rep(NA, pp)
  out_conv <- rep(NA, pp)
  # data frames to store plot resutls
  plt_df_subj_all <- data.frame()
  plt_df_popul_all <- data.frame()
  # iterate over different y's
  for (i in 1 : pp){ # i <- 1
    print(paste0("i = ", i))
    y_name <- y_name_vec[i]
    y_label <- y_label_vec[i]
    # prepare model data frame
    mod_df <- 
      dat %>%
      rename_with(~ c("y"), all_of(y_name))  %>%
      rename_with(~ c("x"), all_of(x_name)) %>% 
      filter(!is.na(y)) %>%
      group_by(beiwe_id, survey_time_date, x) %>%
      summarise(y = mean(y, na.rm = TRUE)) %>%
      ungroup()
    if (x_scale){
      message("Scaling x...")
      mod_df <- mod_df %>% mutate(x = scale(x)[, 1])
    }
    if (y_scale){
      message("Scaling y...")
      mod_df <- mod_df %>% mutate(y = scale(y)[, 1])
    }
    # fit model
    mod_out <- lmer(mod_formula, data = mod_df, control = lmerControl(optimizer = "bobyqa"))
    # generate additional model information / pull model parameters
    mod_out_conv    <- merMod_has_converged(mod_out)
    # mod_out_ci      <- get_mod_ci(mod_out, mod_df)
    round_fct = 10
    mod_out_ci     <- get_mod_ci1_B(mod_out, mod_df, ci_confint = TRUE)
    mod_out_s      <- summary(mod_out)
    mod_out_sc     <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est    <- round(mod_out_sc$estimate, round_fct)
    mod_out_pvalue <- round(mod_out_sc$pr_t, round_fct)
    mod_out_r2     <- round(r.squaredGLMM(mod_out), round_fct)
    # mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$beiwe_id_fct, skewness), 3)
    # append model parameters
    out_intcp_est[i]  <- mod_out_est[1]
    out_slope_est[i]  <- mod_out_est[2]
    out_intcp_pval[i] <- mod_out_pvalue[1]
    out_slope_pval[i] <- mod_out_pvalue[2]
    out_intcp_ci_lo[i] <- mod_out_ci[1]
    out_intcp_ci_up[i] <- mod_out_ci[2]
    out_slope_ci_lo[i] <- mod_out_ci[3]
    out_slope_ci_up[i] <- mod_out_ci[4]
    out_R2m[i]        <- mod_out_r2[1]
    out_R2c[i]        <- mod_out_r2[2]
    out_conv[i]       <- mod_out_conv
    # append plot df
    plt_df_subj <- mod_df %>% select(-survey_time_date)
    plt_df_subj$mu <- getME(mod_out, "mu")
    plt_df_popul <- expand.grid(x = range(mod_df$x))
    plt_df_popul$mu <- predict(mod_out, newdata = plt_df_popul, re.form = NA)
    plt_df_subj <- mutate(plt_df_subj, y_name = y_name, y_label = y_label)
    plt_df_popul <- mutate(plt_df_popul, y_name = y_name, y_label = y_label)
    plt_df_subj_all <- rbind(plt_df_subj_all, plt_df_subj)
    plt_df_popul_all <- rbind(plt_df_popul_all, plt_df_popul)
  }
  # combine into model df
  out_df <- data.frame(
    # out_dataset_name = rep(dataset_name, pp),
    out_y_label = y_label_vec,
    out_x_label = rep(x_label, pp),
    out_intcp_est,
    out_intcp_pval,
    out_slope_est,
    out_slope_pval,
    out_intcp_ci_lo,
    out_intcp_ci_up,
    out_slope_ci_up,
    out_slope_ci_lo,
    out_R2m,
    out_R2c,
    out_conv
  ) 
  names(out_df) <- gsub("out_", "", names(out_df))
  out_list <- list(out_df = out_df, plt_df_subj_all = plt_df_subj_all, plt_df_popul_all = plt_df_popul_all)
  return(out_list)
}


# ------------------------------------------------------------------------------
# fit models: run
# ------------------------------------------------------------------------------

# data frame to store model estimation for all outcomes
mod_out_combined = data.frame()

# shared params
dat = dat_comb_F
x_scale = FALSE
y_scale = FALSE
y_name_vec = y_name_vec_lmm
y_label_vec = y_label_vec_lmm
mod_formula = as.formula("y ~ x  + (1 + x | beiwe_id)")


# ------------------------------------------------------------------------------
# ALSFRS-RSE total score

x_name = "frs_total_score"
x_label = "ALSFRS-RSE total score"

# estimate
out_list = get_lmm_yvecagg_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out = out_list$out_df
mod_out_combined = rbind(mod_out_combined, mod_out)

# save other objects to file for future plot making
saveRDS(out_list$plt_df_subj_all, file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_alsfrsrse_plt_df_subj_all.rds"))
saveRDS(out_list$plt_df_popul_all, file.path(here(), "results_objects", "table_lmm_estimate_measure_vs_alsfrsrse_plt_df_popul_all.rds"))


# ------------------------------------------------------------------------------
# ALSFRS-RSE Q1-3

x_name = "frs_q123"
x_label = "ALSFRS-RSE Q1-3 (bulbar)"

out_list = get_lmm_yvecagg_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out = out_list$out_df
mod_out_combined = rbind(mod_out_combined, mod_out)


# ------------------------------------------------------------------------------
# ALSFRS-RSE Q4-6

x_name = "frs_q456"
x_label = "ALSFRS-RSE Q4-6 (fine motor)"

out_list = get_lmm_yvecagg_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out = out_list$out_df
mod_out_combined = rbind(mod_out_combined, mod_out)


# ------------------------------------------------------------------------------
# ALSFRS-RSE Q7-9

x_name = "frs_q789"
x_label = "ALSFRS-RSE Q7-9 (gross motor)"

out_list = get_lmm_yvecagg_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out = out_list$out_df
mod_out_combined = rbind(mod_out_combined, mod_out)


# ------------------------------------------------------------------------------
# ALSFRS-RSE Q10-12

x_name = "frs_q101112"
x_label = "ALSFRS-RSE Q10-12 (respiratory)"

out_list = get_lmm_yvecagg_vs_x(dat, y_name_vec, y_label_vec, x_name, x_label, mod_formula)
mod_out = out_list$out_df
mod_out_combined = rbind(mod_out_combined, mod_out)


# ------------------------------------------------------------------------------
# save combined table
# ------------------------------------------------------------------------------

mod_out_combined = mod_out_combined %>%
  arrange(x_label, y_label)
mod_out_combined

fwrite(mod_out_combined, file.path(here(), "results_tables", "table_lmm_estimate_measure_vs_alsfrsrse.csv"))
