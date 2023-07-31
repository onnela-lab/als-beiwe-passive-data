
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

from forest.jasmine.traj2stats import gps_stats_main
from forest.constants import Frequency
from platform import python_version
from datetime import date, datetime, timedelta
import time
import sys
import os
import pandas as pd
import numpy as np
import warnings
warnings.simplefilter(action ='ignore', category = FutureWarning)

print(sys.argv)
job_id = int(sys.argv[1])
# job_id = 0


# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

beiwe_id_list_path    = "foo/data_beiwe_processed/gps_zip_tz_state_country_chunks.csv"
beiwe_data_dir        = 'foo'
beiwe_data_proc_dir   = 'foo'
beiwe_data_proc_dir_touched = 'foo'

# create if does not exist
if not os.path.isdir(beiwe_data_proc_dir):
    os.makedirs(beiwe_data_proc_dir)

if not os.path.isdir(beiwe_data_proc_dir_touched):
    os.makedirs(beiwe_data_proc_dir_touched)


#tz_str    = "America/New_York"
save_traj = False 
quality_threshold = 0.05


# -------------------------------------------------------------------------------
# run
# -------------------------------------------------------------------------------

# read data frame with study users
beiwe_id_df = pd.read_csv(beiwe_id_list_path, sep =',')
beiwe_id_df = beiwe_id_df[beiwe_id_df['beiwe_id'].notna()]
beiwe_id_vec = beiwe_id_df['beiwe_id'].unique().tolist()
# len(beiwe_id_vec)

beiwe_id_vec_idx = list(range(len(beiwe_id_vec)))
good_indices = [i for i in beiwe_id_vec_idx if i % 15 == job_id]

# iterate over subset of indexes of beiwe_id_df
for row_i in good_indices: # row_i = 0
  # pull user_id specific to this loop
  #user_id = beiwe_id_df['beiwe_id'].tolist()[row_i]
  user_id = beiwe_id_vec[row_i]
  print('Starting user_id = ' + user_id)
  # save that we touched that user
  file_tmp = os.path.join(beiwe_data_proc_dir_touched, user_id)
  if not os.path.exists(file_tmp):
    # Creating a file at specified location
    with open(file_tmp, 'w') as fp:
      pass
  # get subset of data frame for that user
  beiwe_id_df_i = beiwe_id_df[beiwe_id_df['beiwe_id'] == user_id]
  # iterate over rows 
  j_range_max = beiwe_id_df_i.shape[0]
  for j in range(j_range_max): # j = 0
    date_start = beiwe_id_df_i['date_start'].tolist()[j]
    date_end = beiwe_id_df_i['date_end'].tolist()[j]
    tz_str = beiwe_id_df_i['tz'].tolist()[j]
    date_start_d = datetime.strptime(date_start, '%Y-%m-%d').date()
    date_end_d = datetime.strptime(date_end, '%Y-%m-%d').date()
    time_start_list = [date_start_d.year, date_start_d.month, date_start_d.day, 0, 0, 0]
    time_end_list   = [date_end_d.year, date_end_d.month, date_end_d.day, 0, 0, 0]
    output_folder_j = os.path.join(beiwe_data_proc_dir, user_id + "_" + str(date_start_d))
    if not os.path.exists(output_folder_j):
      os.makedirs(output_folder_j)
    for try_i in range(0, 2):
      try:
        gps_stats_main(study_folder = beiwe_data_dir, 
                       output_folder = output_folder_j, 
                       time_start = time_start_list,
                       time_end = time_end_list,
                       tz_str = tz_str, 
                       frequency = Frequency.DAILY, 
                       save_traj = save_traj, 
                       participant_ids = [user_id],
                       quality_threshold = quality_threshold)
        # remove all_memory_dict.pkl
        file_path_tmp = os.path.join(output_folder_j, "all_memory_dict.pkl")
        if os.path.exists(file_path_tmp):
          os.remove(file_path_tmp)
        # remove all_bv_set.pkl
        file_path_tmp = os.path.join(output_folder_j, "all_bv_set.pkl")
        if os.path.exists(file_path_tmp):
          os.remove(file_path_tmp)
        # if success
        break
      except BaseException as e:
        print(str(e))
        print(user_id + 'error in  try_i = ' + str(try_i))
        continue
  # print success log for that user
  print('FINISHED user_id = ' + user_id)


