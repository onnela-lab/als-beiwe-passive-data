#' @description 
#' Process timezone output raw to get data frame in the form applicable for 
#' analysis. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(lutz)
library(sf)
library(zoo)
library(maps)
library(here)

options(digits.secs = 3)
options(scipen = 999)


# ------------------------------------------------------------------------------
# UTIL FUNCTIONS
# ------------------------------------------------------------------------------

# add state information (col 1: longitudes, col 2: latitudes)
lonlat_to_state <- function(pointsDF, states = spData::us_states, name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


# read zip file 
ziplkp <- 
  fread(file.path(here(), "data_other", "US.txt")) %>% 
  as.data.frame() %>%
  select(
    f_country = V1,
    f_zip = V2,
    f_city = V3,
    f_state = V4,
    f_county = V6,
    f_latitude = V10,
    f_longitude = V11
  ) %>%
  filter(f_state != "") %>%
  as.data.frame()


# (val 1: longitudes, val 2: latitudes)
ZipLooker <- function(obs_longitude, obs_latitude){
  tryCatch({
    x <- ziplkp
    x$latdiff = abs(obs_latitude - x$f_latitude)
    x$londiff = abs(obs_longitude - x$f_longitude)
    x$totdiff = x$latdiff + x$londiff
    # z <- head(top_n(x, 1, -totdiff), n = 1)$zip
    # z <- head(top_n(x, 1, -totdiff), n = 1)[, c("f_zip", "f_city", "f_state", "f_country")]
    z <- top_n(x, 1, -totdiff)[1, c("f_zip", "f_city", "f_state", "f_country")]
    return(z)
  }, error = function(e) rep(NA, 4))
}
# # test
# obs_longitude = -76.60683; obs_latitude = 39.47689; ZipLooker(obs_longitude, obs_latitude)



# ------------------------------------------------------------------------------
# PROCESS RAW GPS SUBSET WITH ZipLooker
# ------------------------------------------------------------------------------

raw_gps_subset_path <- file.path(here(), "data_beiwe_processed", "gps", "raw_gps_subset.csv")
dat_0 <- 
  fread(raw_gps_subset_path) %>% 
  as.data.frame() %>%
  janitor::clean_names()
dim(dat_0)
length(unique(dat_0$beiwe_id))


# ------------------------------------------------------------------------------
# DEFINE NEW GEOLOC VARIABLES
# ------------------------------------------------------------------------------

# read from file 
dat_geo <- readRDS(file.path(here(), "data_beiwe_processed", "gps", "raw_gps_subset_geomatched.rds"))
# "c" stands for "closest"; even if you are in Brasil, the closest area will be found
names(dat_geo) <- c("c_zip", "c_city", "c_state", "c_country")
dat_1 <- cbind(dat_0, dat_geo)

# add state information (col 1: longitudes, col 2: latitudes)
dat_1$state <- lonlat_to_state(dat_1[, c("longitude", "latitude")])

# add time zone information 
# dat_1$tz <- tz_lookup_coords(lat = dat_1$latitude, lon = dat_1$longitude, method = "fast")
dat_1$tz <- tz_lookup_coords(lat = dat_1$latitude, lon = dat_1$longitude, method = "accurate")

# add country information
dat_1$country <- map.where(x = dat_1$longitude, y = dat_1$latitude)
dat_1$country <- gsub("USA.*", "USA", dat_1$country)

# - filter where country was not found 
dat_2 <- dat_1 %>% filter(!is.na(country))
# - if "country" not "USA", then put NA to "c_" variables
# - if "country "USA" and "state" empty and "c_state" non_empty, replace "state" with "c_state"
# - if "country "USA" and "state" non_empty and "c_state" empty, replace "c_state" with "state"
# - if country "USA" and "state" non_empty and "c_state" non_empty and ("state" != "c_state"), put NA to (c_zip, c_city) and replace "c_state" with "state" 
dat_2a <- dat_1 %>% filter(country != "USA")
dat_2b <- dat_1 %>% filter(country == "USA", is.na(state), is.na(c_state))
dat_2c <- dat_1 %>% filter(country == "USA", !is.na(state), !is.na(c_state), state == c_state)
dat_2d <- dat_1 %>% filter(country == "USA", !is.na(state), !is.na(c_state), state != c_state)
dat_2e <- dat_1 %>% filter(country == "USA", is.na(state), !is.na(c_state))
dat_2f <- dat_1 %>% filter(country == "USA", !is.na(state), is.na(c_state))

nrow(dat_2)
nrow(dat_2a) + nrow(dat_2b) + nrow(dat_2c) + nrow(dat_2d) + nrow(dat_2e) + nrow(dat_2f) 
nrow(dat_2a) 
nrow(dat_2b) # EMPTY
nrow(dat_2c) # no changes to be made here
nrow(dat_2d) 
nrow(dat_2e)
nrow(dat_2f) # EMPTY

# - if "country" not "USA", then put NA to "c_" variables
dat_2a <- dat_2a %>% mutate(c_zip = NA, c_city = NA, c_state = NA, c_country = NA)

# - if country "USA" and "state" non_empty and "c_state" non_empty and ("state" != "c_state"), put NA to (c_zip, c_city) and replace "c_state" with "state" 
dat_2d <- dat_2d %>% mutate(c_zip = NA, c_city = NA, c_state = state, c_country = country)
  
# - if "country "USA" and "state" empty and "c_state" non_empty, replace "state" with "c_state"
dat_2e <- dat_2e %>% mutate(state = c_state)

# combine after fixes
dat_3 <- dat_2a %>% rbind(dat_2c) %>% rbind(dat_2d) %>% rbind(dat_2e)
nrow(dat_3) == nrow(dat_2)

# checks
table(dat_3$tz, useNA = "always")
table(dat_3$country, useNA = "always")
table(dat_3$state, useNA = "always")
table(dat_3$c_zip, useNA = "always")


# ------------------------------------------------------------------------------
# FURTHER PREPROCESSING. INPUTE TZ, STATEm ETC -- WHERE WE DO NOT HAVE GPS DATA
# ------------------------------------------------------------------------------

# rename columns
# use existing time zone information to format UTC time into local time  
dat_4 <- 
  dat_3 %>%
  # janitor::clean_names() %>%
  mutate(
    utc_time = ymd_hms(utc_time),
  ) %>%
  rowwise() %>%
  mutate(
    local_time = with_tz(utc_time, tzone = tz)
  ) %>%
  ungroup() %>%
  mutate(
    utc_time_date = as.Date(utc_time),
    local_time_date = as.Date(local_time)
  ) %>%
  as.data.frame()
  
length(unique(dat_4$beiwe_id))


# ------------------------------------------------------------------------------

# read data frame with start and end of data collection
start_end_dates_df_path <- file.path(here(), "data_participants_other_processed", "beiwe_2_0_start_and_end_dates_clean.csv")
start_end_dates_df <- fread(start_end_dates_df_path) %>% as.data.frame() 
length(unique(start_end_dates_df$beiwe_id))

# create data frame with sequence of dates
beiwe_id_vec <- sort(unique(start_end_dates_df$beiwe_id))
date_list <- list()
for (i in 1 : length(beiwe_id_vec)){ # i <- 1
  beiwe_id_i <- beiwe_id_vec[i] 
  start_end_dates_df_i <- start_end_dates_df[start_end_dates_df$beiwe_id == beiwe_id_i, ]
  subj_id_i <- start_end_dates_df_i$subj_id[1]
  date_start_i <- as.Date(start_end_dates_df_i$beiwe_data_query_start[1])
  date_ended_i <- as.Date(start_end_dates_df_i$beiwe_data_query_end[1])
  date_df_i <- data.frame(utc_time_date = seq(date_start_i, date_ended_i, by="days"))
  date_df_i$beiwe_id <- beiwe_id_i
  date_df_i$subj_id <- subj_id_i
  date_list[[i]] <- date_df_i
}
date_df <- rbindlist(date_list) %>% as.data.frame() 

dat_5 <- 
  date_df %>%
  left_join(dat_4, by = c("beiwe_id", "utc_time_date")) %>%
  arrange(beiwe_id, utc_time_date)

# checks
head(dat_5)
tail(dat_5)
length(unique(dat_5$beiwe_id))
nrow(dat_5)

# ------------------------------------------------------------------------------
# SUMMARIZE 

# function to summarize columns of string values
toString2 <- function(x){
  x <- x[!is.na(x)]
  out <- rle(x)$values
  out <- paste0(out, collapse = ";")
  if (nchar(out) == 0) return(NA)
  return(out)
}

first2 <- function(x){
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(x[1])
}

last2 <- function(x){
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(tail(x, 1))
}


# get summary of a column
get_col_s <- function(df_col, df_col_name){
  df_out <- data.frame(
    first2(df_col),
    last2(df_col),
    n_distinct(df_col, na.rm = TRUE),
    toString2(df_col)
  )
  names(df_out) <- paste0(df_col_name, "_", c("first","last", "dist_n", "dist_list"))
  return(df_out)
}


t1 <- Sys.time()
dat_6 <- 
  dat_5 %>%
  group_by(subj_id, beiwe_id, utc_time_date) %>%
  arrange(beiwe_id, utc_time_date, timestamp) %>%
  mutate(
    same_as_prev = (state == dplyr::lag(state)) & (tz == dplyr::lag(tz)),
    same_as_prev = ifelse(is.na(same_as_prev), 0, same_as_prev),
    entry_cnt = n()
  ) %>%
  filter(same_as_prev == 0) %>%
  group_by(subj_id, beiwe_id, utc_time_date) %>%
  arrange(beiwe_id, utc_time_date, timestamp) %>%
  summarise(
    get_col_s(tz, "tz"),
    get_col_s(country, "country"),
    get_col_s(c_zip, "zip"),
    get_col_s(state, "state")
  )
t2 <- Sys.time()
t2 - t1
# Time difference of 39.91535 secs


# checks
length(unique(dat_6$subj_id))
dim(dat_6)

# checks
table(dat_6$tz_first, useNA = "always")
table(dat_6$tz_last, useNA = "always")
table(dat_6$state_first, useNA = "always")
table(dat_6$state_last, useNA = "always")

# checks (should all be true)
nrow(dat_6 %>% filter(is.na(zip_first), zip_dist_n > 0)) == 0
nrow(dat_6 %>% filter(is.na(zip_last), zip_dist_n > 0)) == 0
nrow(dat_6 %>% filter(is.na(state_first), state_dist_n > 0)) == 0
nrow(dat_6 %>% filter(is.na(state_last), state_dist_n > 0)) == 0
nrow(dat_6 %>% filter(is.na(tz_first), tz_dist_n > 0)) == 0
nrow(dat_6 %>% filter(is.na(tz_last), tz_dist_n > 0)) == 0

dat_6 %>% filter(zip_dist_n > 1)
dat_6 %>% filter(state_dist_n > 1)
dat_6 %>% filter(tz_dist_n > 1)
dat_6 %>% filter(country_dist_n > 1)


# ------------------------------------------------------------------------------
# define confidence level in gps-derived info

# 1 - gps observed at the date
# 2 - gps observed within 7 days before the date 
# 3 - gps not observed within 7 days before the date 
# 4 - gps never observed at all for that individual

dat_7 <- 
  dat_6 %>%
  group_by(beiwe_id) %>%
  arrange(beiwe_id, utc_time_date) %>% 
  mutate(
    gps_status = ifelse(!is.na(tz_first), 1, 0),
    gps_lag_observed_cnt_past_7days = rollsumr(gps_status, k = 8, fill = 0, align = 'right'),
    gps_lag_observed_any_past_7days = (gps_lag_observed_cnt_past_7days > 0) * 1,
    gps_status = ifelse(!is.na(tz_first), 1, ifelse(gps_lag_observed_any_past_7days, 2, 3)),
    days_with_any_gps = sum(!is.na(tz_first)),
    gps_status = ifelse(days_with_any_gps == 0, 4, gps_status)
  ) %>%
  select(-c(gps_lag_observed_cnt_past_7days, gps_lag_observed_any_past_7days, days_with_any_gps)) %>%
  as.data.frame()

table(dat_7$gps_status, useNA = "always")


# ------------------------------------------------------------------------------

# fill up missing data 
dat_8 <- 
  dat_7 %>%
  group_by(beiwe_id) %>%
  arrange(beiwe_id, utc_time_date) %>% 
  # impute zip -- with previous value
  tidyr::fill(zip_last, .direction = "down") %>% 
  tidyr::fill(zip_last, .direction = "up") %>% 
  mutate(
    zip_first = ifelse(is.na(zip_first), zip_last, zip_first)
  ) %>%
  # impute tz -- with previous value
  tidyr::fill(tz_last, .direction = "down") %>% 
  tidyr::fill(tz_last, .direction = "up") %>% 
  mutate(
    tz_first = ifelse(is.na(tz_first), tz_last, tz_first),
    tz_first = ifelse(is.na(tz_first), "America/New_York", tz_first),
    tz_last = ifelse(is.na(tz_last), "America/New_York", tz_last)
  ) %>%
  # impute state -- with previous value
  tidyr::fill(state_last, .direction = "down") %>% 
  tidyr::fill(state_last, .direction = "up") %>% 
  mutate(
    state_first = ifelse(is.na(state_first), state_last, state_first)
  ) %>%
  # impute country -- with previous value
  tidyr::fill(country_last, .direction = "down") %>% 
  tidyr::fill(country_last, .direction = "up") %>% 
  mutate(
    country_first = ifelse(is.na(country_first), country_last, country_first),
    country_first = ifelse(is.na(country_first), "USA", country_first),
    country_last = ifelse(is.na(country_last), "USA", country_last)
  ) %>%
  as.data.frame()

sort(table(dat_8$tz_first, useNA = "always"))
sort(table(dat_8$tz_last, useNA = "always"))

sort(table(dat_8$state_first, useNA = "always"))
sort(table(dat_8$state_last, useNA = "always"))

sort(table(dat_8$country_first, useNA = "always"))
sort(table(dat_8$country_last, useNA = "always"))

nrow(dat_8)

# ------------------------------------------------------------------------------
# SAVE TO FILE 
# ------------------------------------------------------------------------------

dat_F <- dat_8

dat_F_path <- file.path(here(), "data_beiwe_processed", "gps", "gps_zip_tz_state_country.csv")
fwrite(dat_F, dat_F_path)
