

ACC_VALID_MINUTES_BAND_EPSILON = 3 * 60

OBSERVATION_PERIOD_DATE_CAP <- as.Date("2022-09-06")

Y_LABEL_VEC_MEASURES <- c(
  "Home time [hours]",
  "Distance travelled [km]",
  "Activity Index",
  "Activity Index from top 1 minute",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] from top 1 minute",
  "Step count",
  "Step count from top 1 minute"
)
Y_NAME_VEC_MEASURES <- c(
  "home_time",
  "dist_traveled",
  "ai",
  "ai_max_1",
  "cadence",
  "cadence_max_1",
  "steps_cnt",
  "steps_cnt_max_1"
)


Y_NAME_VEC_MEASURES_2 <- c(
  "home_time",
  "log_dist_traveled",
  "log_ai",
  "log_ai_max_1",
  "cadence",
  "cadence_max_1",
  "log_steps_cnt",
  "log_steps_cnt_max_1"
)
Y_LABEL_VEC_MEASURES_2 <- c(
  "Home time [hours]",
  "log(Distance travelled [km])",
  "log(Activity Index)",
  "log(Activity Index from top 1 minute)",
  "Walking cadence [steps/s]",
  "Walking cadence [steps/s] from top 1 minute",
  "log(Step count)",
  "log(Step count from top 1 minute)"
)


message("Y_LABEL_VEC_MEASURES")
message("Y_NAME_VEC_MEASURES")
message("Y_LABEL_VEC_MEASURES_2")
message("Y_NAME_VEC_MEASURES_2")
message("OBSERVATION_PERIOD_DATE_CAP")
message("Have read config file.")