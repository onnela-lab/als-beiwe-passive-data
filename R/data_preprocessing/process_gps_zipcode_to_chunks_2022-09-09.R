#' @description 
#' Process timezone output raw to get data frame in the form applicable for 
#' analysis. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(runstats)
library(here)
options(digits.secs = 3)
options(scipen = 999)


# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------

dat_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_zip_tz_state_country.csv")
dat <- fread(dat_path) %>% as.data.frame()
dat[dat == ""] <- NA
head(dat)
str(dat)
dim(dat)


# ------------------------------------------------------------------------------
# PROCESS
# ------------------------------------------------------------------------------

# - define "local majority" zip (zip which is most common within a person) 
dat_1 <- 
  dat %>%
  group_by(subj_id, beiwe_id, zip_last) %>%
  add_count()  %>% 
  group_by(subj_id, beiwe_id) %>%
  mutate(zip_global_majority = zip_last[n == max(n)][1]) %>%
  select(-n) 

# define "local majority" zip (zip which is common locally; might be different than local majority") 
dat_2 <- data.frame()
subj_id_vec <- sort(unique(dat_1$subj_id))

for (i in 1 : length(subj_id_vec)){ # i <- 4
  message(paste0("i = ", i))
  
  # pull 1 subject
  subj_id_i <- subj_id_vec[i]
  dat_i <- dat_1 %>% filter(subj_id == subj_id_i) %>% arrange(utc_time_date)
  
  # define local tz 
  dat_i <- mutate(dat_i, zip_last = ifelse(is.na(zip_last), 0, zip_last))
  j_zip_global_major <- as.numeric(names(sort(table(dat_i$zip_last), decreasing = TRUE))[1])
  dat_i$zip_local_major <- j_zip_global_major
  nn <- nrow(dat_i)
  for (j in 1 : nn){ # j <- 119
    # print(paste0("j = ", j))
    j_look_back <- (j - 6) : j
    j_look_back <- j_look_back[j_look_back > 0]
    j_look_up <- j : (j + 6)
    j_look_up <- j_look_up[j_look_up <= nn]
    j_look_backandup <- (j - 6) : (j + 6)
    j_look_backandup <- j_look_backandup[j_look_backandup > 0]
    j_look_backandup <- j_look_backandup[j_look_backandup <= nn]
    j_zip_local_major_back_val      <- sort(table(dat_i$zip_last[j_look_back]), decreasing = TRUE)[1]
    j_zip_local_major_up_val        <- sort(table(dat_i$zip_last[j_look_up]), decreasing = TRUE)[1]
    j_zip_local_major_backandup_val <- sort(table(dat_i$zip_last[j_look_backandup]), decreasing = TRUE)[1]
    j_zip_local_major_back      <- as.numeric(names(j_zip_local_major_back_val))
    j_zip_local_major_up        <- as.numeric(names(j_zip_local_major_up_val))
    j_zip_local_major_backandup <- as.numeric(names(j_zip_local_major_backandup_val))
    j_zip <- dat_i$zip_last[j]
    j_cond_1 <- (j_zip != j_zip_global_major) & (j_zip %in% c(j_zip_local_major_back, j_zip_local_major_up)) #& (j_zip_local_major_back_val >= 6 | j_zip_local_major_up_val >= 6)
    if (j_cond_1){
      dat_i$zip_local_major[j] <- j_zip
    } 
    j_cond_2 <- (j_zip != j_zip_global_major) & !(j_zip %in% c(j_zip_local_major_back, j_zip_local_major_up)) #& (j_zip_local_major_backandup_val >= 7)
    if (j_cond_2){
      dat_i$zip_local_major[j] <- dat_i$zip_local_major[j-1] 
    } 
  }
  dat_i$zip_local_major
  dat_i[, c("zip_local_major", "zip_last")] %>% as.data.frame()
  
  # define whether is switch 
  dat_i$zip_switch <- 0
  dat_i$zip_switch[c(1, nn)] <- 1
  for (j in 2 : nn){ # j <- 8
    jm1_zip_local_major <- dat_i$zip_local_major[j-1]
    j_zip_local_major <- dat_i$zip_local_major[j]
    j_look_up <- j : (j + 6)
    j_look_up <- j_look_up[j_look_up <= nn]
    j_look_back <- (j-7) : (j-1)
    j_look_back <- j_look_back[j_look_back > 0]
    if (length(j_look_up) < 7) next
    # 7 up of the same type 
    cond_1 <- (j_zip_local_major != jm1_zip_local_major & all(dat_i$zip_local_major[j_look_up] == j_zip_local_major)) & (length(unique(dat_i$zip_local_major[j_look_back])) == 1)
    if (cond_1){
      dat_i$zip_switch[j] <- 1
    }
  }
  
  # append 
  dat_2 <- rbind(dat_2, dat_i)
}

# restore original time zone 
dat_2 <- as.data.frame(dat_2)
dim(dat_2)
dim(dat_1)


# ------------------------------------------------------------------------------
# SAVE DATA WITH SWITCHES
# ------------------------------------------------------------------------------

dat_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_zip_tz_state_country_switch.csv")
fwrite(dat_2, dat_path)


# ------------------------------------------------------------------------------
# PRCESS DATA TO GENERATE START AND END OF GPS PROCESSING
# ------------------------------------------------------------------------------

dat_3 <-
  dat_2 %>% 
  filter(zip_switch == 1) %>%
  select(beiwe_id, date_start = utc_time_date, tz = tz_last) %>% 
  group_by(beiwe_id) %>%
  arrange(beiwe_id, date_start) %>%
  mutate(date_end = lead(date_start), .before = tz) %>%
  filter(!is.na(date_end)) %>% 
  filter(!is.na(beiwe_id)) %>% 
  as.data.frame()
dat_3

dat_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_zip_tz_state_country_chunks.csv")
fwrite(dat_3, dat_path)

