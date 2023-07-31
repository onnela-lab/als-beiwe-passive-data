
#' @description 
#' This script subsets survey answers to keep ALSFRS-R (complete answers to these) only.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
options(digits.secs = 3)


# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------

# Beiwe surveys all 
survey_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly.csv")
survey_beiwe <- 
  fread(survey_beiwe_path) %>%
  as.data.frame() %>%
  mutate(et_time_date = as.Date(et_time_date))

# checks
head(survey_beiwe)


# ------------------------------------------------------------------------------
# prepare clean --  Beiwe ALSFRS-R 
# ------------------------------------------------------------------------------

# read pre-defined map of ALSFRS-R answer text and ALSFRS-R answer value
alsfrs_answer_map_path <- file.path(here(), "doc", "Surveys", "ALSFRSR_answer_value_map.csv")
alsfrs_answer_map <- read_csv(alsfrs_answer_map_path)

val0_vec <- c(
  "NPO; exclusively parenteral or enteral feedings",
  "Unable to perform any aspect of task"
)

# define numeric {0,1,2,3,4} value of an answer
alsfrsr_beiwe0 <- 
  survey_beiwe %>% 
  filter(survey_name == "ALSFRS-R") %>%
  filter(!grepl("Do you have a g-tube or feeding tube that you use for", question_text)) %>%
  left_join(alsfrs_answer_map, by = c("question_id", "question_text", "answer")) %>%
  mutate(answer_value = ifelse(is.na(answer_value) & answer == "NPO; exclusively parenteral or enteral feedings", 0, answer_value)) %>%
  mutate(answer_value = ifelse(is.na(answer_value) & answer == "Unable to perform any aspect of task", 0, answer_value)) %>%
  mutate(answer_value = ifelse(is.na(answer_value) & answer == "Normal", 4, answer_value)) %>%
  mutate(answer_value = ifelse(is.na(answer_value) & answer == "Somewhat slow and clumsy; but no help needed", 3, answer_value))
summary(alsfrsr_beiwe0$answer_value)

# filter to keep only those that have 12 answers
alsfrsr_beiwe1 <- 
  alsfrsr_beiwe0 %>% 
  mutate(et_time_date = as.Date(et_time)) %>%
  group_by(beiwe_id, et_time_date) %>%
  filter(n() == 12) %>%
  ungroup() %>% 
  select(beiwe_id, et_time_date, question_text, answer_value) 
nrow(alsfrsr_beiwe1) / 12
unique(alsfrsr_beiwe1$question_text)

# map to FRS question
alsfrsr_question_no_map <- data.frame(question_text = sort(unique(unique(alsfrsr_beiwe1$question_text))))
alsfrsr_question_no_map$question_text
question_no_vec <- c(
  5, 5, 9, 5, 5, 
  6, 10, 4, 11, 12,
  2, 1, 3, 7, 8
)
question_no_vec <- paste0("frs", question_no_vec)
alsfrsr_question_no_map$question_no <- question_no_vec

alsfrsr_beiwe2 <- 
  alsfrsr_beiwe1 %>%
  inner_join(alsfrsr_question_no_map, by = "question_text") %>%
  select(-question_text) 

# check
alsfrsr_beiwe2 %>%
  group_by(beiwe_id, et_time_date) %>%
  summarise(
    unique_cnt = n_distinct(question_no)
  ) %>%
  ungroup() %>%
  pull(unique_cnt) %>%
  summary()

alsfrsr_beiwe3 <- 
  alsfrsr_beiwe2 %>%
  pivot_wider(names_from = question_no, values_from = answer_value) %>%
  rowwise() %>%
  mutate(frs_total_score = sum(across(frs1 : frs12))) %>%
  as.data.frame()

# checks 
dim(alsfrsr_beiwe3)
head(alsfrsr_beiwe3)
summary(alsfrsr_beiwe3$frs_total_score)
length(unique(alsfrsr_beiwe3$beiwe_id))
alsfrsr_beiwe3 %>% count(beiwe_id)


# ------------------------------------------------------------------------------
# save processed data 
# ------------------------------------------------------------------------------

alsfrsr_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrsrse.csv")
fwrite(alsfrsr_beiwe3, alsfrsr_beiwe_path)

