
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

# NOTE: The code for estimating noise level has been authored by Marcin Straczkiewicz. 

import sys
import os
import pandas as pd
import numpy as np
import math
import datetime
from io import StringIO
from io import BytesIO
from collections import Counter

# add credentials and code to access data from beiwe server
sys.path.insert(0, "/home/marta/Documents/data_beiwe_settings")
import decrypt_MK_smart as dcr

# job_id = int(sys.argv[1])
# job_id = 0


# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

beiwe_id_list_path    = "foo/beiwe_2_0_start_and_end_dates_clean.csv"
project_dir           = "foo"
out_df_path           = "foo/noise_level.csv"
out_df_files_path     = "foo/noise_level_files.csv"

 # define variables
earth_acc = 9.80665
analysis_window = 7

# delete outcome file if an older version exists
if os.path.exists(out_df_path):
  os.remove(out_df_path)

if os.path.exists(out_df_files_path):
  os.remove(out_df_files_path)


# -------------------------------------------------------------------------------
# run
# -------------------------------------------------------------------------------

# read data frame with study users
beiwe_id_df = pd.read_csv(beiwe_id_list_path, sep =',')
beiwe_id_df = beiwe_id_df[beiwe_id_df['beiwe_id'].notna()]

# preallocate outcome
df = pd.DataFrame({"beiwe_id": [], "noise_level": [], "noise_level_before_cap": [] })
df_files = pd.DataFrame({"beiwe_id": [], "file_name": [], "file_row_count": [], "files_with_same_date": [] })

# iterate over participants
for row_i in range(beiwe_id_df.shape[0]): # row_i = 2
  print("Entering row_i = " + str(row_i))

  beiwe_id = beiwe_id_df['beiwe_id'].tolist()[row_i]
  # list files for current beiwe_id
  lpf_out = dcr.list_processed_files(beiwe_id)
  # convert generator object to list
  lpf_out_l = list(lpf_out)
  # filter to keep paths with 'accelerometer' word
  lpf_out_l = [k for k in lpf_out_l if '/accelerometer/' in k]
  lpf_out_l = [k for k in lpf_out_l if '/1970' not in k]
  
  if len(lpf_out_l) < 1:
    # if empty sequence 
    df_files.loc[df_files.shape[0] + 1] = [beiwe_id, "", -1, -1]
    df.loc[df.shape[0] + 1] = [beiwe_id, -1, -1]
    continue
  
  # filter to keep only first "analysis_window" number of full days 
  lpf_out_l_fnames = [os.path.basename(k).replace(".csv", "") for k in lpf_out_l]
  lpf_out_l_dt = [datetime.datetime.strptime(k, "%Y-%m-%dT%H:%M:%S").date() for k in lpf_out_l_fnames]
  lpf_out_l_dt_c = Counter(lpf_out_l_dt).most_common(analysis_window)
  lpf_out_l_dt_c_el = [el for el, count in lpf_out_l_dt_c]
  # keep indexes of these files for which day-dates fall into the set of day-dates with largest number of hourly files
  lpf_out_l_idx = [i for i in range(len(lpf_out_l_dt)) if lpf_out_l_dt[i] in lpf_out_l_dt_c_el]
  lpf_out_l_F = [lpf_out_l[i] for i in range(len(lpf_out_l)) if i in lpf_out_l_idx]
  
  if len(lpf_out_l_F) < 1:
    df_files.loc[df_files.shape[0] + 1] = [beiwe_id, "", -2, -2]
    df.loc[df.shape[0] + 1] = [beiwe_id, -2, -2]
    continue
              
  # preallocate variable specific to current participant
  act_lev_vec = []
    
  # iterate over participant files
  for j in range(len(lpf_out_l_F)):
  # for j in range(10):
    print(str(j))
    fpath = lpf_out_l_F[j]
    # how many files with the same date 
    fpath_dt = datetime.datetime.strptime(os.path.basename(fpath).replace(".csv", ""), "%Y-%m-%dT%H:%M:%S").date() 
    fpath_dt_cnt = [count for el, count in lpf_out_l_dt_c if el == fpath_dt][0]
    try:
      download_out = dcr.download(fpath)
      # convert bytes stream into Pandas data frame
      data = pd.read_csv(BytesIO(download_out))
      x = np.array(data["x"])
      y = np.array(data["y"])
      z = np.array(data["z"])
      vm = np.sqrt(x**2 + y**2 + z**2)
      # standardize measurement to gravity units (g)
      if np.mean(vm) > 5:
        x = x/earth_acc
        y = y/earth_acc
        z = z/earth_acc
      
      # sum of peak-to-peak amplitude in each axis # CORRECT
      act_lev = np.sum([np.std(x), np.std(y), np.std(z)])
      # append noise values
      act_lev_vec.append(act_lev)
      # append file information
      df_files.loc[df_files.shape[0] + 1] = [beiwe_id, os.path.basename(fpath), data.shape[0], fpath_dt_cnt]
    except: 
      print('FAILED')
  
  # estimate noise level as 5 times the lowest observed noise level
  temp = act_lev_vec
  temp = [x for x in temp if math.isnan(x) == False]
  temp.sort()
  typical_noise_level = 5 * temp[np.nonzero(temp)[0][0]]
  typical_noise_level_before_cap = typical_noise_level
  if typical_noise_level > 0.01:
    typical_noise_level = 0.01
  
  # append to data frame
  df.loc[df.shape[0] + 1] = [beiwe_id, typical_noise_level, typical_noise_level_before_cap]
  # save current version of data frames
  df.to_csv(out_df_path, index=False)
  df_files.to_csv(out_df_files_path, index=False)

# final save
df.to_csv(out_df_path, index=False)
df_files.to_csv(out_df_files_path, index=False)
