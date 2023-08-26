import numpy as np
import pandas as pd
import os


def clean_data(df):
    #copy data set to new dataframe
    new_df = df.copy()

    #unique sensor names and depths
    unique_sensors = pd.unique(df.sensor)
    unique_depths = pd.unique(df.depth)

    #empty array to assign nan or zero codes (e.g., all depths have nan or all depths are zeros
    nan_template = np.empty(len(df))
    nan_template[:] = np.nan
    new_df['zero_code'] = nan_template
    new_df['nan_code'] = nan_template

    #check for zeros and nan
    for unique_sensor in unique_sensors:

        #get dataframe values for specific sensor values
        sensor_vals = df[df['sensor'] == unique_sensor]

        #group by days and iterate sensor data for each day
        for day, group in sensor_vals.groupby('day'):

            #get the length of zeros per depth each day
            len_zeros = len(group[group['value'] == 0])

            #get the length of nans per depth each day
            len_nan = len(group[group['value'] != group['value']])

            #check if length of zeros is equal to the length of unique depths
            if len_zeros == len(unique_depths):
                indexes_zeros = group.index.values
                new_df.loc[indexes_zeros, 'value'] = np.nan
                new_df.loc[indexes_zeros, 'zero_code'] = 1

            #check if length of nans is equal to the length of unique depths
            if len_nan == len(unique_depths):
                indexes_nan = group.index.values
                new_df.loc[indexes_nan, 'value'] = np.nan
                new_df.loc[indexes_nan, 'nan_code'] = 1

            #if neither is true (length of nans/zeros == length)
            else:
                pass

    
    return new_df

fp = r'~/Desktop/UVA/RESEARCH/Barrow/Meteorological_Seasons_Data/'
fname = r'Summer_All_Depths_Cleaned.csv'
df = pd.read_csv(os.path.join(fp, fname))
new_df = clean_data(df)
######
print(new_df)


