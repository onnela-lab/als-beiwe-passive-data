
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

from datetime import date, datetime
from tzwhere import tzwhere
import sys
import os
import pandas as pd
import numpy as np
import warnings
warnings.simplefilter(action ='ignore', category = FutureWarning)

# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

BEIWE_DATA_DIR   = "foo
PROJ_DIR         = "foo"

# define path with imput data
out_path = os.path.join(PROJ_DIR, 'data_beiwe_processed', 'gps', 'raw_gps_subset.csv') 


# -------------------------------------------------------------------------------
# process
# -------------------------------------------------------------------------------

# initiate timezone function
# tz = tzwhere.tzwhere(forceTZ = True)

# get subjects from study folder
subjects = os.listdir(BEIWE_DATA_DIR)
len(subjects) 
# 64  # 2022-09-08

# create data frame to append data for all subjects
df_all = pd.DataFrame()

# run code over all subjects
for subject in subjects:
  # sys.stdout.write("Processing subject " + subject + "\n")
  # define path to subject's GPS data directory
  subject_gps_path = os.path.join(BEIWE_DATA_DIR, subject, 'gps')  
  # if the GPS directory does not exist for that user, skip to next user
  if not os.path.exists(subject_gps_path):
    print(subject + " has no GPS directory")
    continue 
  # get GPS files from subject folder (filter to keep .csv only)
  gps_files_list = os.listdir(subject_gps_path)
  gps_files_list = list(filter(lambda f: f.endswith('.csv'), gps_files_list))
  if len(gps_files_list) == 0:
    # sys.stdout.write(subject + " has no GPS data\n")
    print(subject + " has no GPS data CSV files")
  else:
    print("processing subject " + subject)
    # transform files to datelike format
    if "+00_00.csv" in gps_files_list[0]:
      gps_files_dates = [file.replace("+00_00.csv", "") for file in gps_files_list]
    else:
      gps_files_dates = [file.replace(".csv", "") for file in gps_files_list]
    #dat_out = np.empty((len(gps_files_dates), 2), dtype=object)
    for fi, file_name in enumerate(gps_files_list):
      #print("file_name = " + str(file_name))
      # read lat and log data
      dat_i = pd.read_csv(os.path.join(subject_gps_path, file_name))
      # filter to keep good accuracy
      dat_i = dat_i.loc[dat_i['accuracy'] < 100]
      if len(dat_i) < 2:
        continue
        #sys.stdout.write("Subject: " + subject + ", " + "file: " + file_name + " is empty.\n")
      else:
        dat_i_sub = dat_i.take([0, -1])
        dat_i_sub["beiwe_id"] = subject
        df_all = df_all.append(dat_i_sub, ignore_index = True)
    print(str(df_all.shape))

# check
df_all.head()
df_all.shape


# -------------------------------------------------------------------------------
# save
# -------------------------------------------------------------------------------

df_all.to_csv(out_path, index = False)  
