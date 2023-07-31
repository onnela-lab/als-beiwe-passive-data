# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

import sys
import os
import pandas as pd
import numpy as np
import warnings
from io import StringIO
from io import BytesIO
#pip uninstall forest
#pip install git+https://github.com/onnela-lab/forest.git@develop 

# add credentials and code to access data from beiwe server
sys.path.insert(0, "foo")
import decrypt_MK_smart as dcr

# add oak utils (includes the function to run oak per file)
sys.path.insert(0, "foo")
import oak_utils 

job_id = int(sys.argv[1])


# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

beiwe_id_list_path    = "foo/beiwe_2_0_start_and_end_dates_clean.csv"
project_dir           = "foo"
beiwe_data_proc_dir   = 'foo'
output_folder         = os.path.join(beiwe_data_proc_dir, "oak_output_t1sec_sustainedharmonic_2022-11-23")

# create paths from tge above if do not exist
if not os.path.isdir(beiwe_data_proc_dir):
    os.makedirs(beiwe_data_proc_dir)

if not os.path.isdir(output_folder):
    os.makedirs(output_folder)
    

# -------------------------------------------------------------------------------
# pull data frame
# -------------------------------------------------------------------------------

# read data frame with study users
beiwe_id_df = pd.read_csv(beiwe_id_list_path, sep =',')
beiwe_id_df = beiwe_id_df[beiwe_id_df['beiwe_id'].notna()]
beiwe_id_df_rows = list(range(beiwe_id_df.shape[0]))
# good_indices = [i for i in beiwe_id_df_rows if i % 30 == job_id]
good_indices = [i for i in beiwe_id_df_rows if i % 15 == job_id]

for row_i in good_indices: # row_i = 0
    # pull beiwe_id specific to this loop
    beiwe_id = beiwe_id_df['beiwe_id'].tolist()[row_i]
    # list files for current beiwe_id
    lpf_out = dcr.list_processed_files(beiwe_id)
    # convert generator object to list
    lpf_out_l = list(lpf_out)
    # filter to keep paths with 'accelerometer' word
    # Note these are 1h-long data
    lpf_out_l = [k for k in lpf_out_l if '/accelerometer/' in k]
    if (len(lpf_out_l) == 0):
        continue
    print('[job_id = ' + str(job_id) + '] [beiwe_id = ' + str(beiwe_id) + ']: len(lpf_out_l) = ' + str(len(lpf_out_l)))
    # pull one path
    for j in range(len(lpf_out_l)):   # j = 0
        print('[job_id = ' + str(job_id) + '] [beiwe_id = ' + str(beiwe_id) + ']: j = ' + str(j))
        fpath = lpf_out_l[j]
        try:
            # read data from that file from S3, read, decrypt
            download_out = dcr.download(fpath)
            # convert bytes stream into Pandas data frame
            data = pd.read_csv(BytesIO(download_out))
            # run oak (while suspending warnings)
            warnings.simplefilter('ignore')
            # walkingtime_hourly, steps_hourly, cadence_hourly = oak_utils.run_oak_1file(data, min_amp = 0.2, alpha = 2, beta = 5)
            # oak_out = oak_utils.run_oak_1file_t1sec(data, min_amp = 0.2, alpha = 2, beta = 5)
            oak_out = oak_utils.run_oak_1file_t1sec(data, min_amp = 0.2, alpha = 2, beta = 5, step_freq = (1.1, 2.2), delta = 2)
            warnings.resetwarnings()
            if oak_out.shape[0] > 0:
                # put oak results to summary file
                oak_out['beiwe_id'] = beiwe_id
                oak_out['fname'] = os.path.basename(fpath)
                # save to file
                out_fname = beiwe_id + "_" + os.path.basename(fpath)
                out_fname = out_fname.replace(":", "-")
                out_fpath = os.path.join(output_folder, out_fname)
                oak_out.to_csv(out_fpath)
        except:
            print('FAILED')

print("FINISHED")
