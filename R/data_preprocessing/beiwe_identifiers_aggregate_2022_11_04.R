
#' @description 
#' This script aggregates identifiers Beiwe data to generate one file with 
#' OS information for everyone.

rm(list = ls())
library(tidyverse)
library(data.table)
library(here)


# ------------------------------------------------------------------------------
# read data 

dat_dir <- file.path(here(), "data_beiwe_raw")
dat_path_vec <- list.files(dat_dir, full.names = TRUE, recursive = TRUE)
dat_path_vec <- dat_path_vec[grepl("identifiers", dat_path_vec)]

dat_all_l <- lapply(dat_path_vec, function(dat_path){
  read_csv(dat_path, col_types = cols()) %>% 
    select(beiwe_id = patient_id, utc_time = `UTC time`, device_os)
})
dat <- data.table::rbindlist(dat_all_l) %>% as.data.frame()


# ------------------------------------------------------------------------------
# process

dat2 <- 
  dat %>%
  select(beiwe_id, device_os) %>%
  distinct() %>%
  mutate(device_os = tolower(device_os)) %>%
  as.data.frame()
head(dat2)

# check if only one OS per person
nrow(dat2) == length(unique(dat2$beiwe_id))


# ------------------------------------------------------------------------------
# save

fwrite(dat2, file.path("data_beiwe_processed", "identifiers", "beiwe_os.csv"))
