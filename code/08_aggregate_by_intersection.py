# Load libraries
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import datetime 
import pickle

# Combine data frames with appropriate columns
agg_df = pd.read_csv('data/crashes_2014_2018_to_aggregate.csv')

# Limit the dataframe and filter
keep_cols = ['Crash.ID',
'Intersection.ID',
'Speed.Limit',
'Crash.Severity',
'Number.of.Lanes',
'Number.of.Entering.Roads',
'Traffic.Control.Type',
'Road.Class',
'Roadbed.Width',
'Roadway.Alignment',
'Roadway.Function',
'Roadway.Relation',
'Roadway.Part',
'Roadway.Type',
'Light.Condition',
'First.Harmful.Event',
'n_bikes',
'n_cars',
'n_peds',
'Median.Width',
'Median.Type',
'Highway.Lane.Design',
'Right.of.Way.Usual.Width']

limit_df = agg_df.loc[:,keep_cols]

### Data Cleaning ###

# Speed.Limit - turn all negatives into NaN
# Round all numbers with %5 != 0
limit_df.loc[limit_df['Speed.Limit']<0, 'Speed.Limit'] = np.nan
limit_df.loc[limit_df['Speed.Limit']%5 != 0, 'Speed.Limit'] = round(limit_df.loc[limit_df['Speed.Limit']%5 != 0, 'Speed.Limit'],-1)

# Roadbed.Width - all No Data and nan to NaN
limit_df.loc[limit_df['Roadbed.Width']=='No Data', 'Roadbed.Width'] = np.nan
limit_df.loc[limit_df['Roadbed.Width']=='nan', 'Roadbed.Width'] = np.nan
limit_df['Roadbed.Width'] = limit_df['Roadbed.Width'].astype(float)

# Clean Severity
limit_df['Crash.Severity'] = limit_df['Crash.Severity'].str.upper()
limit_df.loc[limit_df['Crash.Severity']=='N - NOT INJURED', 'Crash.Severity'] = "NOT INJURED"
limit_df.loc[limit_df['Crash.Severity']=='C - POSSIBLE INJURY', 'Crash.Severity'] = "POSSIBLE INJURY"
limit_df.loc[limit_df['Crash.Severity']=='99 - UNKNOWN', 'Crash.Severity'] = "UNKNOWN"
limit_df.loc[limit_df['Crash.Severity']=='B - NON-INCAPACITATING INJURY', 'Crash.Severity'] = "NON-INCAPACITATING INJURY"
limit_df.loc[limit_df['Crash.Severity']=='A - SUSPECTED SERIOUS INJURY', 'Crash.Severity'] = "SUSPECTED SERIOUS INJURY"
limit_df.loc[limit_df['Crash.Severity']=='K - KILLED', 'Crash.Severity'] = "KILLED"

# Create a binary for Crash.Severity
limit_df['Crash.Severity_Binary'] = "NON-SEVERE"
limit_df.loc[(limit_df['Crash.Severity']=='SUSPECTED SERIOUS INJURY') |
             (limit_df['Crash.Severity']=='KILLED'), 'Crash.Severity_Binary'] = "SEVERE"

# Number.of.Lanes - all No Data and nan to NaN
limit_df.loc[limit_df['Number.of.Lanes']=='No Data', 'Number.of.Lanes'] = np.nan
limit_df.loc[limit_df['Number.of.Lanes']=='nan', 'Number.of.Lanes'] = np.nan
limit_df['Number.of.Lanes'] = limit_df['Number.of.Lanes'].astype(float)

# Roadway.Type - all No Data and nan to NaN
limit_df.loc[limit_df['Roadway.Type']=='No Data', 'Roadway.Type'] = np.nan
limit_df.loc[limit_df['Roadway.Type']=='nan', 'Roadway.Type'] = np.nan

# Median.Width - all No Data and nan to NaN
limit_df.loc[limit_df['Median.Width']=='No Data', 'Median.Width'] = np.nan
limit_df.loc[limit_df['Median.Width']=='nan', 'Median.Width'] = np.nan
limit_df['Median.Width'] = limit_df['Median.Width'].astype(float)

limit_df = limit_df.reset_index(drop = True)

#Aggregations - First step, make dummy variables for categorical data
#Rename to have the original column name included
#This totally could have been done in a function, but I was tired
CrashSeverity_dummies = pd.get_dummies(limit_df['Crash.Severity'])
CrashSeverity_dummies.columns = ['Crash.Severity_'+col for col in CrashSeverity_dummies.columns.values]

NumberofEnteringRoads_dummies = pd.get_dummies(limit_df['Number.of.Entering.Roads'])
NumberofEnteringRoads_dummies.columns = ['Number.of.Entering.Roads_'+col for col in NumberofEnteringRoads_dummies.columns.values]

TrafficControlType_dummies = pd.get_dummies(limit_df['Traffic.Control.Type'])
TrafficControlType_dummies.columns = ['Traffic.Control.Type_'+col for col in TrafficControlType_dummies.columns.values]

RoadClass_dummies = pd.get_dummies(limit_df['Road.Class'])
RoadClass_dummies.columns = ['Road.Class_'+col for col in RoadClass_dummies.columns.values]

RoadwayAlignment_dummies = pd.get_dummies(limit_df['Roadway.Alignment'])
RoadwayAlignment_dummies.columns = ['Roadway.Alignment_'+col for col in RoadwayAlignment_dummies.columns.values]

RoadwayFunction_dummies = pd.get_dummies(limit_df['Roadway.Function'])
RoadwayFunction_dummies.columns = ['Roadway.Function_'+col for col in RoadwayFunction_dummies.columns.values]

RoadwayRelation_dummies = pd.get_dummies(limit_df['Roadway.Relation'])
RoadwayRelation_dummies.columns = ['Roadway.Relation_'+col for col in RoadwayRelation_dummies.columns.values]

RoadwayPart_dummies = pd.get_dummies(limit_df['Roadway.Part'])
RoadwayPart_dummies.columns = ['Roadway.Part_'+col for col in RoadwayPart_dummies.columns.values]

LightCondition_dummies = pd.get_dummies(limit_df['Light.Condition'])
LightCondition_dummies.columns = ['Light.Condition_'+col for col in LightCondition_dummies.columns.values]

FirstHarmfulEvent_dummies = pd.get_dummies(limit_df['First.Harmful.Event'])
FirstHarmfulEvent_dummies.columns = ['First.Harmful.Event_'+col for col in FirstHarmfulEvent_dummies.columns.values]

RoadwayType_dummies = pd.get_dummies(limit_df['Roadway.Type'])
RoadwayType_dummies.columns = ['Roadway.Type_'+col for col in RoadwayType_dummies.columns.values]

CrashSeverity_Binary_dummies = pd.get_dummies(limit_df['Crash.Severity_Binary'])
CrashSeverity_Binary_dummies.columns = ['Crash.Severity_Binary_'+col for col in CrashSeverity_Binary_dummies.columns.values]

#Create a new dataframe that has the categorical variables now all as dummy variables
crash = limit_df.loc[:,['Intersection.ID',
                        'Crash.ID',
                        'Speed.Limit',
                        'Roadbed.Width', 
                        'Number.of.Lanes',
                        'Median.Width',
                        'n_bikes', 
                        'n_cars',
                        'n_peds']]

crash = pd.concat([crash, 
                  CrashSeverity_dummies,
                  NumberofEnteringRoads_dummies,
                  TrafficControlType_dummies,
                  RoadClass_dummies,
                  RoadwayAlignment_dummies,
                  RoadwayFunction_dummies,
                  RoadwayRelation_dummies,
                  RoadwayPart_dummies,
                  LightCondition_dummies,
                  FirstHarmfulEvent_dummies,
                  RoadwayType_dummies,
                  CrashSeverity_Binary_dummies], axis = 1)

#Create a column for the number of crashes per intersection
crash_num = crash.loc[:,['Intersection.ID',
            'Crash.ID']].groupby(['Intersection.ID']).count().reset_index()
crash_num = crash_num.rename(index = str, columns = {'Crash.ID':'crash_num'})

#Create a dataframe for the aggregated crash data by intersection
#Probably should have named this variable intersection_df, but oh well
crash_agg_df = crash.loc[:,['Intersection.ID']]
crash_agg_df = pd.merge(crash_agg_df, crash_num, how = "left", on = "Intersection.ID")

#Functions to aggregate the continuous and categorical variable columns
#Continuous - get min, max, mean, and sum (not all are relevant to all variables)
#Categorical - get mean (which is essentially a proportion, good for modeling)

def continuous_agg(df, agg_col):
    grouped_df = df.loc[:,['Intersection.ID',
            agg_col]].groupby(['Intersection.ID']).agg([np.min, np.max, np.mean, np.sum])
    grouped_df.columns = ['_'.join(col).strip() for col in grouped_df.columns.values]
    grouped_df = grouped_df.reset_index()
    return grouped_df

def categorical_agg(df, agg_col):
    grouped_df = df.loc[:,['Intersection.ID',
            agg_col]].groupby(['Intersection.ID']).agg([np.mean])
    grouped_df.columns = ['_'.join(col).strip() for col in grouped_df.columns.values]
    grouped_df = grouped_df.reset_index()
    return grouped_df

#Run through the continuous variables and run the continuous aggregator
for col in crash.columns[2:6]:
    agged_df = continuous_agg(crash, col)
    crash_agg_df = pd.merge(crash_agg_df, agged_df, how = "left", on = "Intersection.ID")

#Run through the categorical variables and get the means    
for col in crash.columns[6::]:
    agged_df = categorical_agg(crash, col)
    crash_agg_df = pd.merge(crash_agg_df, agged_df, how = "left", on = "Intersection.ID")

#Drop all duplicate rows
crash_agg_df = crash_agg_df.drop_duplicates()

#Remove columns that are not useful for modeling
crash_agg_df_to_model = crash_agg_df.drop(["Speed.Limit_amax","Speed.Limit_sum",
          "Number.of.Lanes_amin","Number.of.Lanes_amax", "Number.of.Lanes_mean",
          "Number.of.Lanes_sum", "Roadbed.Width_amin","Roadbed.Width_amax",
          "Roadbed.Width_mean", "Roadbed.Width_sum",
          "Crash.Severity_KILLED_mean","Crash.Severity_NON-INCAPACITATING INJURY_mean",
          "Crash.Severity_NOT INJURED_mean","Crash.Severity_POSSIBLE INJURY_mean",
          "Crash.Severity_SUSPECTED SERIOUS INJURY_mean",
          "Median.Width_amin","Median.Width_amax","Median.Width_mean","Median.Width_sum"], axis = 1)

#Save the new intersection crash aggregated for modeling
crash_agg_df_to_model.to_csv('data/intersection_aggregated_2014_2018.csv', index = False)

