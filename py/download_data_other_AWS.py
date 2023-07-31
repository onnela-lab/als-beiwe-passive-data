
# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

from platform import python_version
from datetime import date, datetime
import sys
import os
import pandas as pd
import mano
import mano.sync as msync
import numpy as np

print(sys.argv)
job_id = int(sys.argv[1]) # job_id = 0
print('job_id = ' + str(job_id))

# -------------------------------------------------------------------------------
# manually edited params
# -------------------------------------------------------------------------------

beiwe_id_list_path    = "foo/beiwe_2_0_start_and_end_dates_clean.csv"
beiwe_data_dir        = 'foo'
beiwe_data_error_dir  = 'foo'
keyring_dir           = 'foo'

# study id
study_id = 'foo'

# set up the keyring
sys.path.insert(0, keyring_dir)
import keyring_studies_MK
Keyring = mano.keyring(None)

data_streams = ['calls', 'texts', 'survey_answers', 'survey_timings', 'power_state', 'identifiers']


# -------------------------------------------------------------------------------
# run
# -------------------------------------------------------------------------------

# read data frame with study users
beiwe_id_df = pd.read_csv(beiwe_id_list_path, sep =',')
beiwe_id_df = beiwe_id_df[beiwe_id_df['beiwe_id'].notna()]
# beiwe_id_df.columns.values
beiwe_id_df_rows = list(range(beiwe_id_df.shape[0]))
execute_indices = [i for i in beiwe_id_df_rows if i % 15 == job_id]

# iterate over subset of indexes of beiwe_id_df
for row_i in execute_indices: # row_i = 0

    # -------------------------------------------------------------------------------
    # pull data specific to particular user
    # -------------------------------------------------------------------------------
    # beiwe_id_df.index[beiwe_id_df['beiwe_id'] == '114akcoc'].tolist()
    user_id = beiwe_id_df['beiwe_id'].tolist()[row_i]
    # bug fixed: replaced [job_id] with [row_i]
    day_date_min = beiwe_id_df['beiwe_data_query_start'].tolist()[row_i]
    day_date_max = beiwe_id_df['beiwe_data_query_end'].tolist()[row_i]
    print('Starting user_id = ' + user_id)

    # create sequence of days over which we will be quering data day by day
    download_rn_start = datetime.strptime(day_date_min, '%Y-%m-%d').date()
    # download_rn_end   = date.today()
    download_rn_end   = datetime.strptime(day_date_max, '%Y-%m-%d').date()
    download_days_seq = pd.date_range(download_rn_start, download_rn_end, freq = 'd').tolist()
    download_days_seq = [str(i.date()) + "T00:00:00" for i in download_days_seq]

    # -------------------------------------------------------------------------------
    # run download day by day: gps
    # -------------------------------------------------------------------------------
    j_range_max = len(download_days_seq) - 1
    for j in range(j_range_max): # j = 2
    #for j in range(10):
        #print(str(j) + "/", j_range_max)
        # define currently considered
        time_start = download_days_seq[j]
        time_end = download_days_seq[j + 1]
        # wheter keep repeating
        repeat_cnt = 0
        while True:
            # download GPS
            try:
                zf = msync.download(Keyring, study_id, user_id, data_streams = data_streams, time_start = time_start, time_end = time_end)
                zf.extractall(beiwe_data_dir)
                # if successful, break
                break
            except BaseException as e:
                print(str(e))
                repeat_cnt = repeat_cnt + 1
                print('repeat_cnt = repeat_cnt + 1 [gps] -- ' + user_id + " " + str(j))
            # if exceeded the number of breaks
            if repeat_cnt > 10:
                 # append error log
                if not os.path.exists(beiwe_data_error_dir):
                    os.makedirs(beiwe_data_error_dir)
                file_tmp = os.path.join(beiwe_data_error_dir, user_id + "_gps_" + str(j))
                open(file_tmp, mode = 'a').close()
                break

    print('FINISHED user_id = ' + user_id)

print('--- FINISHED PYTHON SCRIPT RUN --- job_id = ' + str(job_id))


