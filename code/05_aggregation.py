import os
import pandas as pd
import numpy as np

wd = "/Users/bhm/january_advisors/vision_zero/data/"

#Get the data
int_coords = pd.read_csv(wd+"intersections_coords.csv")
crash_int_cw = pd.read_csv(wd+"crashes_intersections_crosswalk.csv")
int_trans = pd.read_csv(wd+"intersections_transit.csv")
crash_df = pd.read_csv(wd+"txdot_cris_crashes_harris_fortbend_montgomery_2014_2019.csv")
int_sw = pd.read_csv(wd+"intersections_sidewalks.csv")

#Creating merged dataframe
full_df = pd.merge(int_coords,
        int_trans,
        how = "left",
        on = ['Intersection.ID'])

full_df = pd.merge(int_sw, 
                   full_df, 
                   how = "left", 
                   on = ['Intersection.ID'])

full_df = pd.merge(crash_int_cw,
        full_df,
        how = "left",
        on = ['Intersection.ID'])

full_df = pd.merge(crash_df,
        full_df,
        how = "left",
        on = ['Crash.ID'])

#List of columns to keep for downstream analysis, as determined by Lauren and Kelsey
keep_cols = ['Intersection.ID',
        'Crash.ID', # this is needed
    'Int.Distance.Ft', # filter for crashes within 200 feet of intersections
    'Speed.Limit',
    'Crash.Severity',
    'Number.of.Lanes',
    'Number.of.Entering.Roads',
    'Traffic.Control.Type',
    'Roadbed.Width',
    'Roadway.Alignment',
    'Roadway.Function',
    'Roadway.Relation',
    'Roadway.Part',
    'Roadway.Type',
    'Light.Condition',
    'n_bikes',
    'n_cars',
    'n_peds',
    'Median.Width',
    'n_roads',
    'n_street_names', 
    'n_transit_routes_200ft',
    'n_transit_routes_400ft', 
    'n_transit_routes_closest',
    'n_transit_stops_200ft', 
    'n_transit_stops_400ft',
    'n_transit_stops_closest', 
    'n_transit_trips_200ft',
    'n_transit_trips_400ft', 
    'n_transit_trips_closest',
    'sidewalk_dist_50ft', 
    'sidewalk_50ft',
    'sidewalk_dist_100ft', 
    'sidewalk_100ft',
    'sidewalk_dist_200ft', 
    'sidewalk_200ft',
    'year',
    'road_classes'] 

limit_df = full_df.loc[:,keep_cols]

#Filter the dataframe for crashes within 200 feet of intersections (afterwards, don't need this column)
limit_df = limit_df.loc[limit_df['Int.Distance.Ft'] <= 200]

#Data Cleaning
#SpeedLimit - turn all negatives into NaN
#Round all numbers with %5 != 0
limit_df.loc[limit_df['Speed.Limit']<0, 'Speed.Limit'] = np.nan
limit_df.loc[limit_df['Speed.Limit']%5 != 0, 'Speed.Limit'] = round(limit_df.loc[limit_df['Speed.Limit']%5 != 0, 'Speed.Limit'],-1)

#Clean Severity
limit_df['Crash.Severity'] = limit_df['Crash.Severity'].str.upper()
limit_df.loc[limit_df['Crash.Severity']=='N - NOT INJURED', 'Crash.Severity'] = "NOT INJURED"
limit_df.loc[limit_df['Crash.Severity']=='C - POSSIBLE INJURY', 'Crash.Severity'] = "POSSIBLE INJURY"
limit_df.loc[limit_df['Crash.Severity']=='99 - UNKNOWN', 'Crash.Severity'] = "UNKNOWN"
limit_df.loc[limit_df['Crash.Severity']=='B - NON-INCAPACITATING INJURY', 'Crash.Severity'] = "NON-INCAPACITATING INJURY"
limit_df.loc[limit_df['Crash.Severity']=='A - SUSPECTED SERIOUS INJURY', 'Crash.Severity'] = "SUSPECTED SERIOUS INJURY"
limit_df.loc[limit_df['Crash.Severity']=='K - KILLED', 'Crash.Severity'] = "KILLED"

#Create a binary for CrashSeverity based on injured vs non-injured, and excluding all unknown
limit_df['Crash.Severity_Binary'] = "INJURED"
limit_df.loc[(limit_df['Crash.Severity']=='NOT INJURED'), 'Crash.Severity_Binary'] = "NON-INJURED"
limit_df = limit_df.loc[~(limit_df['Crash.Severity']=="UNKNOWN")] 

#RoadbedWidth - all No Data and nan to NaN
limit_df.loc[limit_df['Roadbed.Width']=='No Data', 'Roadbed.Width'] = np.nan
limit_df.loc[limit_df['Roadbed.Width']=='nan', 'Roadbed.Width'] = np.nan
limit_df['Roadbed.Width'] = limit_df['Roadbed.Width'].astype(float)

#NumberofLanes - all No Data and nan to NaN
limit_df.loc[limit_df['Number.of.Lanes']=='No Data', 'Number.of.Lanes'] = np.nan
limit_df.loc[limit_df['Number.of.Lanes']=='nan', 'Number.of.Lanes'] = np.nan
limit_df['Number.of.Lanes'] = limit_df['Number.of.Lanes'].astype(float)

#RoadwayType - all No Data and nan to NaN
limit_df.loc[limit_df['Roadway.Type']=='No Data', 'Roadway.Type'] = np.nan
limit_df.loc[limit_df['Roadway.Type']=='nan', 'Roadway.Type'] = np.nan

#MedianWidth - all No Data and nan to NaN
limit_df.loc[limit_df['Median.Width']=='No Data', 'Median.Width'] = np.nan
limit_df.loc[limit_df['Median.Width']=='nan', 'Median.Width'] = np.nan
limit_df['Median.Width'] = limit_df['Median.Width'].astype(float)

#Get the number from the Number.of.Entering.Roads
def get_first_num(row):
    return row.split(" - ")[0]

limit_df['num_entering_roads'] = limit_df['Number.of.Entering.Roads'].apply(lambda x: get_first_num(x))
limit_df['num_entering_roads'] = limit_df['num_entering_roads'].replace("97", np.nan)

#Create a new dataframe that has the categorical variables now all as dummy variables
crash_df = limit_df.loc[:,['Intersection.ID',
                           'Crash.ID',
                           'year',
                           'road_classes',
                           'Speed.Limit',
                           'Roadbed.Width',
                           'Number.of.Lanes',
                           'Median.Width',
                           'n_bikes', 
                           'n_cars',
                           'n_peds',
                           'n_roads', 
                           'n_street_names',
                           'n_transit_routes_200ft',
                           'n_transit_routes_400ft',
                           'n_transit_routes_closest',
                           'n_transit_stops_200ft',
                           'n_transit_stops_400ft',
                           'n_transit_stops_closest',
                           'n_transit_trips_200ft',
                           'n_transit_trips_400ft',
                           'n_transit_trips_closest']]

crash_df_2018 = crash_df.loc[crash_df['year'] == 2018]
crash_df = crash_df.loc[crash_df['year'] < 2018]

def create_dummies(df, col):
    dummies = pd.get_dummies(df[col])
    dummies.columns = [col+"_"+column for column in dummies.columns.values]
    return dummies

cols_for_dummies = ['Crash.Severity',
                    'Number.of.Entering.Roads', 
                    'Traffic.Control.Type',
                    'Roadway.Alignment', 
                    'Roadway.Function',
                    'Roadway.Relation', 
                    'Roadway.Part', 
                    'Roadway.Type', 
                    'Light.Condition',
                    'Crash.Severity_Binary']

for col in cols_for_dummies:
    crash_df = pd.concat([crash_df,
                               create_dummies(limit_df, col)],
                                axis = 1)
    
#Create a column for the number of crashes per intersection
crash_num = crash_df.loc[:,['Intersection.ID',
            'Crash.ID']].groupby(['Intersection.ID']).count().reset_index()
crash_num = crash_num.rename(index = str, columns = {'Crash.ID':'crash_num'})

#Per intersection, number of injured and non-injured crashes
injury_crash_num = crash_df.loc[:,['Intersection.ID',
                                  'Crash.Severity_Binary_INJURED',
                                   'Crash.Severity_Binary_NON-INJURED'
                                  ]].groupby(['Intersection.ID']).sum().reset_index()
injury_crash_num = injury_crash_num.rename(index = str, 
                                           columns = {'Crash.Severity_Binary_INJURED':'crash_injured_num',
                                                     'Crash.Severity_Binary_NON-INJURED':'crash_non-injured_num'})


injury_crash_num_2018 = crash_df_2018.loc[:,['Intersection.ID',
                                             'Crash.Severity_Binary_INJURED',
                                             'Crash.Severity_Binary_NON-INJURED'
                                            ]].groupby(['Intersection.ID']).sum().reset_index()
injury_crash_num_2018 = injury_crash_num_2018.rename(index = str, 
                                                     columns = {'Crash.Severity_Binary_INJURED':'crash_injured_num_18',
                                                                'Crash.Severity_Binary_NON-INJURED':'crash_non-injured_num_18'})

#Create a dataframe for the aggregated crash data by intersection
intersection_df = crash_df.loc[:,['Intersection.ID',
                                 'road_classes',
                                 'street_names',
                                 'n_roads', 
                                  'n_street_names', 
                                  'n_transit_routes_200ft',
                                  'n_transit_routes_400ft', 
                                  'n_transit_routes_closest',
                                  'n_transit_stops_200ft', 
                                  'n_transit_stops_400ft',
                                  'n_transit_stops_closest', 
                                  'n_transit_trips_200ft',
                                  'n_transit_trips_400ft', 
                                  'n_transit_trips_closest']].drop_duplicates()

intersection_df = pd.merge(intersection_df, crash_num, how = "left", on = "Intersection.ID")
intersection_df = pd.merge(intersection_df, injury_crash_num, how = "left", on = "Intersection.ID")
intersection_df = pd.merge(intersection_df, injury_crash_num_2018, how = "left", on = "Intersection.ID")
# may need to fix nulls

#Functions to aggregate the continuous and categorical variable columns
#Continuous - get min, max, mean (not all are relevant to all variables)
#Categorical - get mean (which is essentially a proportion, good for modeling)

def continuous_agg(df, agg_col):
    grouped_df = df.loc[:,['Intersection.ID',
            agg_col]].groupby(['Intersection.ID']).agg([np.min, np.max, np.mean])
    grouped_df.columns = ['_'.join(col).strip() for col in grouped_df.columns.values]
    grouped_df = grouped_df.reset_index()
    return grouped_df

def categorical_agg(df, agg_col):
    grouped_df = df.loc[:,['Intersection.ID',
            agg_col]].groupby(['Intersection.ID']).agg([np.mean])
    grouped_df.columns = ['_'.join(col).strip() for col in grouped_df.columns.values]
    grouped_df = grouped_df.reset_index()
    return grouped_df

base_cols = ['Intersection.ID','road_classes', 'crash_num', 'crash_injured_num', 
             'crash_non-injured_num', 'crash_injured_num_18', 
             'crash_non-injured_num_18',
             'n_roads', 
              'n_street_names', 
              'n_transit_routes_200ft',
              'n_transit_routes_400ft', 
              'n_transit_routes_closest',
              'n_transit_stops_200ft', 
              'n_transit_stops_400ft',
              'n_transit_stops_closest', 
              'n_transit_trips_200ft',
              'n_transit_trips_400ft', 
              'n_transit_trips_closest']
continuous_cols = ['Speed.Limit', 
                    'Roadbed.Width', 
                    'Number.of.Lanes', 
                    'Median.Width']

#Run through the continuous variables and run the continuous aggregator
for col in continuous_cols:
#     print(f"Now doing {col}")
    agged_df = continuous_agg(crash_df, col)
    intersection_df = pd.merge(intersection_df, agged_df, how = "left", on = "Intersection.ID")

#Columns for categorical agg
cat_cols = list(crash_df.columns)
cat_cols = [col for col in cat_cols if col != 'Crash.ID']
cat_cols = [col for col in cat_cols if col not in base_cols]
cat_cols = [col for col in cat_cols if col not in continuous_cols]

#Run through the categorical variables and get the means    
for col in cat_cols:
#     print(f"Now doing {col}")
    agged_df = categorical_agg(crash_df, col)
    intersection_df = pd.merge(intersection_df, agged_df, how = "left", on = "Intersection.ID")
    
#Creating unbiased means
intersection_df['Speed.Limit_unbiased_mean'] = (intersection_df['Speed.Limit_amin']+intersection_df['Speed.Limit_amax'])/2
intersection_df['Number.of.Lanes_unbiased_mean'] = (intersection_df['Number.of.Lanes_amin']+intersection_df['Number.of.Lanes_amax'])/2

#Renumbering the categorical variables to 0 or 1, with a threshold of 0.10
categorical_cols = list(intersection_df.columns)[30:-4]
for col in categorical_cols:
    intersection_df.loc[intersection_df[col]>0.10, col] = 1
    intersection_df.loc[intersection_df[col]<=0.10, col] = 0

#Save the new intersection crash aggregated dataset for modeling
intersection_df.to_csv(wd+'full_aggregated_intersection_dataset.csv', index = False)

intersection_df_to_model = intersection_df.drop(['Roadbed.Width_amin',
                                             'Roadbed.Width_amax',
                                             'Roadbed.Width_mean',
                                             'Number.of.Lanes_amin',
                                             'Number.of.Lanes_amax',
                                             'Number.of.Lanes_mean',
                                             'Median.Width_amin',
                                             'Median.Width_amax',
                                             'Median.Width_mean']
                                             , axis = 1)

#Save the new intersection crash aggregated for modeling
intersection_df_to_model.to_csv(wd+'intersection_dataset_to_model.csv', index = False)
