# NOTE: in publicly available version of this file, some variable values 
# have been replaced by a placeholder / "foo" value. 

# NOTE: The code has been adopted from the core code authored by Marcin Straczkiewicz. 

import sys
import os
import pandas as pd
import numpy as np

sys.path.insert(0, "foo")
from oak_base import rle, preprocess_bout, adjust_bout, get_pp, compute_interpolate_cwt, identify_peaks_in_cwt, find_walking, find_continuous_dominant_peaks, ismember

from datetime import datetime, timedelta
import numpy.typing as npt
from scipy import interpolate
from scipy.signal import find_peaks
from scipy.signal.windows import tukey
from ssqueezepy import ssq_cwt
from forest.utils import get_ids

def run_oak_1file(data, fs = 10, min_amp = 0.3, alpha = 0.6, beta = 2.5, step_freq = (1.4, 2.3), delta = 20):
  
    # pull data columns
    t = data["UTC time"].tolist()
    timestamp = np.array(data["timestamp"])
    x = np.array(data["x"], dtype="float64")  # x-axis acc.
    y = np.array(data["y"], dtype="float64")  # y-axis acc.
    z = np.array(data["z"], dtype="float64")  # z-axis acc.
    
    # process time format to allow identification of bout samples
    t = [t_ind.replace("T", " ") for t_ind in t]
    t = [datetime.strptime(t_ind, '%Y-%m-%d %H:%M:%S.%f') for t_ind in t]
    t_shifted = [t_i-timedelta(microseconds=t_i.microsecond) for t_i in t]
    
    # find seconds with enough samples
    hour_start = t_shifted[0]
    hour_start = (hour_start - timedelta(minutes=hour_start.minute) - timedelta(seconds=hour_start.second))
    hour_end = hour_start + timedelta(hours=1)
    t_sec_bins = pd.date_range(hour_start, hour_end, freq='S').tolist()
    samples_per_sec, t_sec_bins = np.histogram(t_shifted, t_sec_bins)
    
     # seconds with enough samples / 9 should be in fact fs
    samples_enough = samples_per_sec >= (fs - 1)
    
    # find bouts with sufficient duration (here, minimum 5s)
    run_length, start_ind, val = rle(samples_enough)
    bout_start = start_ind[val & (run_length >= 5)]
    bout_duration = run_length[val & (run_length >= 5)]
    
    # initiate temporal metric
    cadence_temp_hourly = []    
    
    for b_ind, b_datetime in enumerate(bout_start):
        #print(str([b_ind, b_datetime]))
        # create a list with second-level timestamps
        bout_time = pd.date_range(
            t_sec_bins[bout_start[b_ind]],
            t_sec_bins[bout_start[b_ind] + bout_duration[b_ind]],
            freq='S').tolist()
        bout_time = [t_i.to_pydatetime() for t_i in bout_time[:-1]]
        # find observations in this bout
        acc_ind = np.isin(t_shifted, bout_time)
        t_bout = timestamp[acc_ind]/1000
        x_bout = x[acc_ind]
        y_bout = y[acc_ind]
        z_bout = z[acc_ind]
        # compute only if phone is on the body
        if np.sum([np.std(x_bout), np.std(y_bout), np.std(z_bout)]) > 0.1:
            #print("preprocess_bout ...")
            # interpolate bout to 10Hz and calculate vm
            vm_bout = preprocess_bout(t_bout, x_bout, y_bout, z_bout)[3]
            # find walking and estimate steps
            cadence_bout = find_walking(vm_bout, min_amp = min_amp, alpha = alpha, beta = beta, step_freq = step_freq, delta = delta)
            cadence_bout = cadence_bout[np.where(cadence_bout > 0)]
            #cadence_temp_daily.append(cadence_bout)
            cadence_temp_hourly.append(cadence_bout)        
    
    # store hourly values
    cadence_temp_hourly = [item for sublist in cadence_temp_hourly for item in sublist]
    walkingtime_hourly = len(cadence_temp_hourly)
    steps_hourly = int(np.sum(cadence_temp_hourly))
    cadence_hourly = np.mean(cadence_temp_hourly)
    
    # return 3 objects
    return walkingtime_hourly, steps_hourly, cadence_hourly;



def run_oak_1file_t1sec(data, fs = 10, min_amp = 0.3, alpha = 0.6, beta = 2.5, step_freq = (1.4, 2.3), delta = 20, min_t = 3):
  
    # pull data columns
    t = data["UTC time"].tolist()
    timestamp = np.array(data["timestamp"])
    x = np.array(data["x"], dtype="float64")  # x-axis acc.
    y = np.array(data["y"], dtype="float64")  # y-axis acc.
    z = np.array(data["z"], dtype="float64")  # z-axis acc.
    
    # process time format to allow identification of bout samples
    t = [t_ind.replace("T", " ") for t_ind in t]
    t = [datetime.strptime(t_ind, '%Y-%m-%d %H:%M:%S.%f') for t_ind in t]
    t_shifted = [t_i-timedelta(microseconds=t_i.microsecond) for t_i in t]
    
    # find seconds with enough samples
    hour_start = t_shifted[0]
    hour_start = (hour_start - timedelta(minutes=hour_start.minute) - timedelta(seconds=hour_start.second))
    hour_end = hour_start + timedelta(hours=1)
    t_sec_bins = pd.date_range(hour_start, hour_end, freq='S').tolist()
    samples_per_sec, t_sec_bins = np.histogram(t_shifted, t_sec_bins)
    
     # seconds with enough samples / 9 should be in fact fs
    samples_enough = samples_per_sec >= (fs - 1)
    
    # find bouts with sufficient duration (here, minimum 5s)
    run_length, start_ind, val = rle(samples_enough)
    bout_start = start_ind[val & (run_length >= 5)]
    bout_duration = run_length[val & (run_length >= 5)]
    
    # initiate temporal metric
    cadence_temp_hourly = []    
    timestamp_temp_hourly = []    
    
    for b_ind, b_datetime in enumerate(bout_start):
        #print(str([b_ind, b_datetime]))
        # create a list with second-level timestamps
        bout_time = pd.date_range(
            t_sec_bins[bout_start[b_ind]],
            t_sec_bins[bout_start[b_ind] + bout_duration[b_ind]],
            freq='S').tolist()
        bout_time = [t_i.to_pydatetime() for t_i in bout_time[:-1]]
        # find observations in this bout
        acc_ind = np.isin(t_shifted, bout_time)
        t_bout = timestamp[acc_ind]/1000
        x_bout = x[acc_ind]
        y_bout = y[acc_ind]
        z_bout = z[acc_ind]
        # compute only if phone is on the body
        if np.sum([np.std(x_bout), np.std(y_bout), np.std(z_bout)]) > 0.1:
            #print("preprocess_bout ...")
            # interpolate bout to 10Hz and calculate vm
            vm_bout = preprocess_bout(t_bout, x_bout, y_bout, z_bout)[3]
            # find walking and estimate steps
            cadence_bout = find_walking(vm_bout, min_amp = min_amp, alpha = alpha, beta = beta, step_freq = step_freq, delta = delta, min_t = min_t)
            cadence_bout_non0 = cadence_bout[np.where(cadence_bout > 0)]
            bout_time_non0 = np.array(bout_time)[np.where(cadence_bout > 0)]
            #append
            timestamp_temp_hourly = timestamp_temp_hourly + bout_time_non0.tolist()   
            cadence_temp_hourly = cadence_temp_hourly + cadence_bout_non0.tolist()
    
    # store in a data frame
    out = pd.DataFrame() 
    out['cadence_timestamp'] = timestamp_temp_hourly
    out['cadence_value'] = cadence_temp_hourly
    
    # return pandas df
    return out;

