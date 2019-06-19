# Load libraries
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import datetime 
import pickle

# from IPython.display import display
# pd.options.display.max_columns = None

# Get file paths for data
data_dir = "data/crash_data/"
data_files = os.listdir(data_dir)
data_files = [file for file in data_files if "txdot" in file]

# Find columns that are different among files
col0 = list(pd.read_csv(data_dir+data_files[0]).columns)
col1 = list(pd.read_csv(data_dir+data_files[1]).columns)
col2 = list(pd.read_csv(data_dir+data_files[2]).columns)
col3 = list(pd.read_csv(data_dir+data_files[3]).columns)

# Combine data frames with appropriate columns
agg_df = pd.read_csv(data_dir+data_files[0])

for file in data_files[1::]:
    df = pd.read_csv(data_dir+file)
    if "CrashNonSuspectedSeriousInjuryCount" in list(df.columns):
        df = df.rename(index = str, 
                       columns = {"CrashNonSuspectedSeriousInjuryCount":"CrashNonincapacitatingInjuryCount"})
    if "CrashSuspectedSeriousInjuryCount" in list(df.columns):
        df = df.rename(index = str, 
                       columns = {"CrashSuspectedSeriousInjuryCount":"CrashIncapacitatingInjuryCount"})
    agg_df = pd.concat([agg_df, df])


# Limit the dataframe and filter
keep_cols = ['CrashID',
'County', # filter for Harris, Montgomery, Fort Bend
'IntersectionRelated', # filter for intersection or intersection related
'SpeedLimit',
'RoadbedWidth',
'CrashSeverity',
'NumberofLanes',
'NumberofEnteringRoads',
'TrafficControlType',
'n_bike',
'n_cars',
'n_peds',
'n_train',
'RoadwayType',
'MedianWidth']

limit_df = agg_df.loc[:,keep_cols]

limit_df = limit_df.loc[(limit_df['County'].str.contains('Fort Bend')) |
                       (limit_df['County'].str.contains('Harris')) |
                       (limit_df['County'].str.contains('Montgomery'))]

limit_df = limit_df.loc[((limit_df['IntersectionRelated'].str.contains('Intersection')) |
                       (limit_df['IntersectionRelated'].str.contains('Intersection Related'))) &
                       ~(limit_df['IntersectionRelated'].str.contains('Non Intersection'))]

### Data Cleaning ###

# SpeedLimit - turn all negatives into NaN
# Round all numbers with %5 != 0
limit_df.loc[limit_df['SpeedLimit']<0, 'SpeedLimit'] = np.nan
limit_df.loc[limit_df['SpeedLimit']%5 != 0, 'SpeedLimit'] = round(limit_df.loc[limit_df['SpeedLimit']%5 != 0, 'SpeedLimit'],-1)

# RoadbedWidth - all No Data and nan to NaN
limit_df.loc[limit_df['RoadbedWidth']=='No Data', 'RoadbedWidth'] = np.nan
limit_df.loc[limit_df['RoadbedWidth']=='nan', 'RoadbedWidth'] = np.nan
limit_df['RoadbedWidth'] = limit_df['RoadbedWidth'].astype(float)

# Clean Severity
limit_df['CrashSeverity'] = limit_df['CrashSeverity'].str.upper()
limit_df.loc[limit_df['CrashSeverity']=='N - NOT INJURED', 'CrashSeverity'] = "NOT INJURED"
limit_df.loc[limit_df['CrashSeverity']=='C - POSSIBLE INJURY', 'CrashSeverity'] = "POSSIBLE INJURY"
limit_df.loc[limit_df['CrashSeverity']=='99 - UNKNOWN', 'CrashSeverity'] = "UNKNOWN"
limit_df.loc[limit_df['CrashSeverity']=='B - NON-INCAPACITATING INJURY', 'CrashSeverity'] = "NON-INCAPACITATING INJURY"
limit_df.loc[limit_df['CrashSeverity']=='A - SUSPECTED SERIOUS INJURY', 'CrashSeverity'] = "SUSPECTED SERIOUS INJURY"
limit_df.loc[limit_df['CrashSeverity']=='K - KILLED', 'CrashSeverity'] = "KILLED"

# Create a binary for CrashSeverity
limit_df['CrashSeverity_Binary'] = "NON-SEVERE"
limit_df.loc[(limit_df['CrashSeverity']=='SUSPECTED SERIOUS INJURY') |
             (limit_df['CrashSeverity']=='KILLED'), 'CrashSeverity_Binary'] = "SEVERE"

# NumberofLanes - all No Data and nan to NaN
limit_df.loc[limit_df['NumberofLanes']=='No Data', 'NumberofLanes'] = np.nan
limit_df.loc[limit_df['NumberofLanes']=='nan', 'NumberofLanes'] = np.nan
limit_df['NumberofLanes'] = limit_df['NumberofLanes'].astype(float)

# RoadwayType - all No Data and nan to NaN
limit_df.loc[limit_df['RoadwayType']=='No Data', 'RoadwayType'] = np.nan
limit_df.loc[limit_df['RoadwayType']=='nan', 'RoadwayType'] = np.nan

# MedianWidth - all No Data and nan to NaN
limit_df.loc[limit_df['MedianWidth']=='No Data', 'MedianWidth'] = np.nan
limit_df.loc[limit_df['MedianWidth']=='nan', 'MedianWidth'] = np.nan
limit_df['MedianWidth'] = limit_df['MedianWidth'].astype(float)

limit_df = limit_df.reset_index(drop = True)

# Write to csv
limit_df.to_csv('limited_clean_txdot_dataset.csv')
