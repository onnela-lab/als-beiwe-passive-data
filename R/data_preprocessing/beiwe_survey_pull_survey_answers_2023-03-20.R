
#' @description 
#' This script processes surveys from "survey_answers" Beiwe data stream. 
#' These will be later combined with surveys from "survey_timings" Beiwe data stream
#' (as neither stream is perfect, hence by combining two we assure we get all
#' data available).

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(here)
options(digits.secs = 3)

# define Beiwe raw data dir
BEIWE_RAW_DATA_DIR <- file.path(here(), "data_beiwe_raw")

# source config script
source(file.path(here(), "R", "config_private.R"))

# ------------------------------------------------------------------------------
# READ AND COMBINE DATA
# ------------------------------------------------------------------------------

# create empty list to store survey_answers responses
dat_all_list <- list()

beiwe_ids <- list.dirs(BEIWE_RAW_DATA_DIR, recursive = FALSE, full.names = FALSE)
user_dirs <- file.path(BEIWE_RAW_DATA_DIR, beiwe_ids)

# checks
length(user_dirs)

for (i in 1 : length(beiwe_ids)){ # i = 30; j = 1; k = 39
  beiwe_id <- beiwe_ids[i]
  user_dir <- user_dirs[i]
  message(paste0("i = ", i, ", beiwe_id = ", beiwe_id))
  survey_answers_path <- file.path(user_dir, "survey_answers")
  # if no survey_answers dir, go to the next user
  if (!dir.exists(survey_answers_path)) next
  survey_ids <- list.dirs(survey_answers_path, recursive = FALSE, full.names = FALSE)
  # if no question directories in survey_answers dir, go to the next user
  if (length(survey_ids) == 0) next 
  
  # iterate over question-specific directories 
  for (j in 1 : length(survey_ids)){ # j <- 1
    # get vector of response filenames whitin this survey_id directory 
    survey_id <- survey_ids[j]
    survey_id_dir <- file.path(survey_answers_path, survey_id)
    response_dat_fnames <- list.files(survey_id_dir)
    if (length(response_dat_fnames) == 0) next
    
    for (k in 1 : length(response_dat_fnames)){ # k <- 1
      response_dat_fname <- response_dat_fnames[k]
      # get UTC and local time of answers submission from the filename 
      utc_time_val <- ymd_hms(gsub("_", ":", str_sub(response_dat_fname, 1, 19)), tz = "UTC")
      # et_time <- format(utc_time, tz = tz, usetz = TRUE, format = "%Y-%m-%d %H:%M:%S")
      # define data path
      dat_path <- file.path(survey_id_dir, response_dat_fname)
      # check if corrupted file and skip if yes 
      dat0 <- readLines(dat_path, n = 1)
      if(!dat0 == "question id,question type,question text,question answer options,answer"){
        print(paste0("did not match column text for  i = ", i, "; j = ", j, "; k = ", k))
        print(dat0)
        next
      } 
      # read the answers data 
      dat <- 
        fread(dat_path) %>% 
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(dat_idx = k) %>%
        mutate(dat_row_idx = row_number()) %>%
        mutate(beiwe_id = beiwe_id) %>%
        mutate(survey_id = survey_id) 
      dat$utc_time <- utc_time_val
      # append to data frame of all data 
      dat_all_list[[length(dat_all_list) + 1]] <- dat
      
    }
  }
}

# checks
length(dat_all_list)
dat_all_df <- rbindlist(dat_all_list) %>% as.data.frame() 
dim(dat_all_df)
head(dat_all_df)


# ------------------------------------------------------------------------------
# FURTHER FORMATTING
# ------------------------------------------------------------------------------

#' For all surveys, assume the local time zone is "America/New_York". 
#' This may be imperfect (participants may travel etc.) but essentially does not
#' matter in practice as we only use date. 
tz_et <- "America/New_York"

dat_all_df_nrow <- nrow(dat_all_df)
et_time_vec <- numeric(dat_all_df_nrow)
for (i in 1 : dat_all_df_nrow){
  print(paste0(i , " / ", dat_all_df_nrow))
  et_time_vec[i] <- format(dat_all_df$utc_time[i], tz = tz_et, usetz = TRUE, format = "%Y-%m-%d %H:%M:%S")
}
head(et_time_vec)

dat_all_df$et_time <- et_time_vec

# checks
head(dat_all_df)
data.frame(
  v1 = dat_all_df$utc_time[c(1, 10, 100)],
  v2 = dat_all_df$et_time[c(1, 10, 100)]
)

# combine data into one df 
dat_all_df2 <- 
  dat_all_df %>%
  arrange(beiwe_id, utc_time, survey_id, dat_idx, dat_row_idx) %>%
  mutate(source = "survey_answers") %>%
  select(
    beiwe_id,
    utc_time,
    et_time,
    survey_id,
    question_id,
    question_type,
    question_text,
    question_answer_options,
    answer,
    dat_idx,
    dat_row_idx,
    source
  )

# checks
names(dat_all_df2)
dim(dat_all_df2)


# ------------------------------------------------------------------------------
# Fix NA into "" to be consistent with "survey_answers" stream
# ------------------------------------------------------------------------------

# check number of NA across data frame columns
sapply(dat_all_df2, function(vec) sum(is.na(vec)))
table(dat_all_df2$question_id, useNA = "always")

dat_all_df3 <- 
  dat_all_df2 %>% 
  # fix NA into "" to be consistent 
  replace(is.na(.), "")

sapply(dat_all_df3, function(vec) sum(is.na(vec)))


# ------------------------------------------------------------------------------
# Fix manualy data for two BEIWE IDs
# ------------------------------------------------------------------------------

#' Note the below code chunk uses variables (BEIWE_ID_1, BEIWE_ID_2) predefined in 
#' config_private.R script that is deliberately not public

# checks
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_1) %>% pull(dat_idx) %>% range()
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_2) %>% pull(dat_idx) %>% range()

dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_1) %>% pull(et_time) %>% range()
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_2) %>% pull(et_time) %>% range()

# increase dat_idx for BEIWE_ID_2 by the largest dat_idx found for BEIWE_ID_1
dat_idx_max_eck9dzpn <- dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_1) %>% pull(dat_idx) %>% max()
dat_all_df3 <- 
  dat_all_df3 %>%
  mutate(
    dat_idx = ifelse(beiwe_id == BEIWE_ID_2, dat_idx + dat_idx_max_eck9dzpn, dat_idx)
  )

# checks
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_1) %>% pull(dat_idx) %>% range()
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_2) %>% pull(dat_idx) %>% range()

# replace the beiwe ID from BEIWE_ID_1 to BEIWE_ID_2  
dat_all_df3 <- 
  dat_all_df3 %>%
  mutate(
    beiwe_id = ifelse(beiwe_id == BEIWE_ID_1, BEIWE_ID_2, beiwe_id)
  )

# checks
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_1) %>% pull(dat_idx) %>% range()
dat_all_df3 %>% filter(beiwe_id == BEIWE_ID_2) %>% pull(dat_idx) %>% range()


# ------------------------------------------------------------------------------
# SAVE DATA 
# ------------------------------------------------------------------------------

# save processed data 
dat_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_answers.rds")
saveRDS(dat_all_df3, dat_path)






