
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

import sys
import os
import pandas as pd
import numpy as np
from io import StringIO
from io import BytesIO

# add credentials and code to access data from beiwe server
sys.path.insert(0, "foo")
import decrypt_MK_smart as dcr

job_id = int(sys.argv[1])
# job_id = 0


# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

beiwe_id_list_path    = "foo/beiwe_2_0_start_and_end_dates_clean.csv"
beiwe_data_proc_dir   = 'foo'
out_file_dir          = os.path.join(beiwe_data_proc_dir, "ai_output_t1min")

# create paths from tge above if do not exist
if not os.path.isdir(beiwe_data_proc_dir):
    os.makedirs(beiwe_data_proc_dir)

if not os.path.isdir(out_file_dir):
    os.makedirs(out_file_dir)


# -------------------------------------------------------------------------------
# run
# -------------------------------------------------------------------------------

# read data frame with study users
beiwe_id_df = pd.read_csv(beiwe_id_list_path, sep =',')
beiwe_id_df = beiwe_id_df[beiwe_id_df['beiwe_id'].notna()]
beiwe_id_df_rows = list(range(beiwe_id_df.shape[0]))
good_indices = [i for i in beiwe_id_df_rows if i % 15 == job_id]

for row_i in good_indices: # row_i = 1
    # pull beiwe_id specific to this loop
    beiwe_id = beiwe_id_df['beiwe_id'].tolist()[row_i]
    # list files for current beiwe_id
    lpf_out = dcr.list_processed_files(beiwe_id)
    # convert generator object to list
    lpf_out_l = list(lpf_out)
    # filter to keep paths with 'accelerometer' word
    # Note these are 1h-long data
    lpf_out_l = [k for k in lpf_out_l if '/accelerometer/' in k]
    # If no data file for that user, continue to next user
    if len(lpf_out_l) < 1:
      continue
    for j in range(len(lpf_out_l)):   # j = 1
        # pull one path
        fpath = lpf_out_l[j]
        try:
            # read data from that file from S3, read, decrypt
            download_out = dcr.download(fpath)
            # convert bytes stream into Pandas data frame
            df_out = pd.read_csv(BytesIO(download_out))
            df_out = df_out.rename(columns={'UTC time': 'utc_time'})
            # format date time
            df_out['utc_time'] = pd.to_datetime(df_out['utc_time'], errors = 'coerce')
            # floor to the nearest second
            df_out['utc_time_rounded1s'] = df_out['utc_time'].dt.floor('1s')
            # ------------------------------------------------------------------
            # AGGREGATE: get 1sec-level AI
            # define activity index
            df_out_agg1 = df_out.groupby(['utc_time_rounded1s']).agg({'timestamp':'count','x':'var','y':'var','z':'var'}).reset_index().rename(columns={'timestamp': 'obs_cnt', 'x':'x_var', 'y':'y_var', 'z':'z_var'})
            df_out_agg1['ai'] = df_out_agg1.apply (lambda row: np.sqrt((row.x_var + row.y_var + row.z_var) * 1/3), axis=1)
            # replace ai values with 0 if number of observations less than 5
            df_out_agg1.loc[df_out_agg1['obs_cnt'] < 5, 'ai'] = 0
            # define valid second (by default 1, replace with 0 if less than 5 raw observations out of 10 expected)
            df_out_agg1['valid_1s'] = 1
            df_out_agg1.loc[df_out_agg1['obs_cnt'] < 5, 'valid_1s'] = 0
            # ------------------------------------------------------------------
            # AGGREGATE: get 1min-level AI
            # floor to the nearest minute
            df_out_agg1['utc_time_rounded1min'] = df_out_agg1['utc_time_rounded1s'].dt.floor('1min')
            # aggregate
            df_out_agg2 = df_out_agg1.groupby(['utc_time_rounded1min']).agg({'ai':'sum','valid_1s':'sum','x_var':'count'}).reset_index().rename(columns={'valid_1s': 'valid_1s_cnt', 'x_var':'1s_cnt'})
            df_out_agg2['beiwe_id'] = beiwe_id
            df_out_agg2 = df_out_agg2.round({'ai': 5})
            # df_out_agg2
            # ------------------------------------------------------------------
            # save to file
            out_fname = beiwe_id + "_" + os.path.basename(fpath)
            out_fname = out_fname.replace(":", "-")
            out_fpath = os.path.join(out_file_dir, out_fname)
            df_out_agg2.to_csv(out_fpath)
        except:
            print('FAILED')

