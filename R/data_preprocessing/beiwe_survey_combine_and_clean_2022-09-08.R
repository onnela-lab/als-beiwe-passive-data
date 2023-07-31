
#' @description 
#' This script combines surveys from "survey_answers" and "survey_timings" 
#' Beiwe data streams.
#' Then some preprocessing cleaning is done to assure only one final answer 
#' per survey question is kept for participant for a day. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(here)
options(digits.secs = 3)


# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------

answers_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_answers.rds")
answers <- readRDS(answers_path)
dim(answers)

timings_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_timings.rds")
timings <- readRDS(timings_path)
dim(timings)

# check these two data frames have the same column names
names(answers) == names(timings)

# read mapping for survey names
# survey_id_map_path <- file.path(PROJ_PAPER1_DIR, "data_participants_other", "subject ids.xlsx")
survey_id_map_path <- file.path(here(), "data_participants_other", "subject ids.xlsx")
survey_id_map <- read_excel(survey_id_map_path, sheet = "survey ID-name map")

# read pre-defined map of ALSFRS-R answer text and ALSFRS-R answer value
ALSFRSR_answer_map_path <- file.path(here(), "doc", "Surveys", "ALSFRSR_answer_value_map.csv")
ALSFRSR_answer_map <- read_csv(ALSFRSR_answer_map_path)


# ------------------------------------------------------------------------------
# get final answers
# ------------------------------------------------------------------------------

#' From Zachary Clement: 
#' (1) if their question type looks like this, it was made on an Android device. 
#'     Radio button questions are the only ones with the integer instead of text
#'     (Also, avoiding any possible text outputs)
#' (2) if a semicolon appears in an answer choice option,
#'     our regexp sub/split operation would think there are
#'     way more answers than there really are. 
#'     We will pull from the responses from iPhone users and switch semicolons 
#'     within an answer to commas. 
#' (3) Android users have "; " seperating answer choices. iPhone users have ";" separating choices.

# get all answer choices text options now that Android and iPhone have the same ones 
radio_answer_choices_list <- 
  answers %>%
  as.data.frame() %>%
  filter(question_type == "radio_button") %>%
  select(question_id, question_answer_options) %>%
  distinct() %>%
  # there is one case where for one question_id there are two question_answer_options,
  # one of which is with some random stuff inside 
  mutate(qao_nchar = nchar(question_answer_options)) %>%
  group_by(question_id) %>%
  arrange(qao_nchar) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-qao_nchar) %>%
  as.data.frame()
  
# function to preprocess answers 
process_answer <- function(question_type, question_id, answer){
  # if their question type looks like this, it was made on an Android device
  question_type_android <- "Radio Button Question"
  answer_android <- as.character(0 : 100)
  if (!(question_type == question_type_android & answer %in% answer_android)){
    return(answer)
  } else {
    # preprocess question answer options
    # get question_answer_options from iPhone answers list
    qao_vec <- radio_answer_choices_list[radio_answer_choices_list$question_id == question_id, "question_answer_options"]
    # remove square brackets at the beginning and at the end
    qao_vec <- gsub("^\\[|\\]$", "", qao_vec)
    # replace " ;" with ";" in question_answer_options (rare cases)
    qao_vec <- gsub(" ;", ";", qao_vec)
    # split by semicolon immediately followed by another string
    qao_vec <- strsplit(qao_vec, ";(?!\\s)", perl = TRUE)[[1]]
    # note the answers values start at 0
    qao_vec_idx <- as.numeric(answer) + 1 
    out <- qao_vec[qao_vec_idx]
    return(out)
  }
}

answers_F <-
  answers %>%
  mutate(et_time_date = as.Date(et_time)) %>% 
  group_by(beiwe_id, et_time_date, survey_id, question_id) %>%
  # choose the last answer for a given question_id (if multiple present)
  arrange(dat_idx, dat_row_idx) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(
    -dat_idx,
    -dat_row_idx
  ) %>% 
  rowwise() %>%
  mutate(answer = process_answer(question_type, question_id, answer)) %>%
  ungroup()
dim(answers_F)

timings_F <- 
  timings %>%
  mutate(et_time_date = as.Date(et_time)) %>% 
  group_by(beiwe_id, et_time_date, survey_id, question_id) %>%
  # choose the last answer for a given question_id (if multiple present)
  arrange(dat_idx, dat_row_idx) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(
    -dat_idx,
    -dat_row_idx
  )
dim(timings_F)

# check if all column names agree
all(names(timings_F) == names(answers_F))

# combine the data 
df_rbind <- 
  rbind(answers_F, timings_F) %>%
  group_by(beiwe_id, et_time_date, survey_id, question_id) %>%
  mutate(cnt = n_distinct(answer)) 
dim(df_rbind)

# define subset where either 
# (a) no duplicates from answers and timings
# (b) duplicates present, but they do agree in answer (pick up one only then)
df_rbind_1 <- 
  df_rbind %>% 
  filter(cnt == 1) %>%
  filter(row_number() == 1) %>%
  ungroup()
dim(df_rbind_1)

# define subset where duplicates present and answers do not answer
df_rbind_2 <- 
  df_rbind %>% 
  filter(cnt > 1) %>%
  mutate(
    is_valid = ifelse(is.na(answer), 0, 1),
    is_valid = ifelse(answer %in% c("", "NO_ANSWER_SELECTED", "NOT_PRESENTED"), 0, is_valid)
  ) %>%
  arrange(is_valid, utc_time) %>%
  filter(row_number() == n())  %>%
  ungroup() %>%
  select(-is_valid)
dim(df_rbind_2)

# combine
dat <- 
  rbind(df_rbind_1, df_rbind_2) %>%
  select(- cnt)
dim(dat)


# ------------------------------------------------------------------------------
# STEP: clean "question_text"
# ------------------------------------------------------------------------------
sngl_quot_rx = "[ʻʼʽ٬‘’‚‛՚︐]"

dat$question_text <- sapply(dat$question_text, function(val){
  val <- gsub(pattern = ":\\s+", ": ", val)
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val
})
# CHECK ------------------------------------------------------------------------
# check if there are multiple distinct question_text within question_id
dat_tmp <- 
  dat %>% 
  group_by(question_id) %>%
  summarise(cnt_distinct = n_distinct(question_text)) %>%
  ungroup() %>%
  filter(cnt_distinct > 1) %>%
  arrange(desc(cnt_distinct)) 
dat_tmp_agg <- 
  dat %>% 
  inner_join(dat_tmp, by = "question_id") %>%
  group_by(survey_id, question_id, question_text) %>%
  summarise(cnt = n()) %>%
  inner_join(survey_id_map, by = "survey_id") %>% 
  ungroup()  %>%
  select(survey_name, question_id, question_text, cnt) %>% 
  arrange(desc(question_id))

# check
vec_tmp <- dat_tmp_agg %>% pull(question_text)
names(vec_tmp) <- NULL
vec_tmp


# ------------------------------------------------------------------------------
# STEP: clean "question_answer_options"
# ------------------------------------------------------------------------------
dat$question_answer_options <- sapply(dat$question_answer_options, function(val){
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val <- gsub(pattern = ";\\s+", ";", val)
  val <- gsub(pattern = "None;3 Some", "None;Some", val)
  # val <- gsub(pattern = "n’t", "n't", val)
  val
})
# CHECK ------------------------------------------------------------------------
dat %>% 
  select(question_id, question_text, question_answer_options) %>%
  distinct() %>%
  group_by(question_id, question_text) %>%
  filter(n() > 1) %>%
  as.data.frame()

# ------------------------------------------------------------------------------
# STEP: clean "answer"
# ------------------------------------------------------------------------------
dat$answer <- sapply(dat$answer, function(val){
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val <- gsub(pattern = "3 Some", "Some", val)
  val
})


# ------------------------------------------------------------------------------
# STEP: add "survey_name" 
# ------------------------------------------------------------------------------
dim(dat)
dat <- 
  dat %>% 
  inner_join(survey_id_map, by = "survey_id")
dim(dat)


# -----------------------------------------------------------------------------
# STEP: remove invalid answers
# ------------------------------------------------------------------------------

dat <- 
  dat %>% 
  filter(!(answer %in% c("", "NO_ANSWER_SELECTED", "NOT_PRESENTED"))) %>% 
  filter(!is.na(answer))
dim(dat)


# ------------------------------------------------------------------------------
# STEP: reorder columns
# ------------------------------------------------------------------------------
names(dat) 
length(names(dat))
dat <- 
  dat %>%
  select(
    beiwe_id,
    # timestamp, 
    utc_time,
    et_time,
    survey_id,
    survey_name,
    question_id,
    everything()
  ) 


# ------------------------------------------------------------------------------
# STEP: arrange
# ------------------------------------------------------------------------------
dat <- 
  dat %>%
  arrange(
    beiwe_id,
    et_time
  )
# CHECK ------------------------------------------------------------------------
head(dat)
dim(dat)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# save processed data 
dat_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly.csv")
fwrite(dat, dat_path)

