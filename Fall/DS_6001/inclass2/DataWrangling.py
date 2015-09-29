# Data management
import pandas as pd
from pandas import Series, DataFrame
import numpy as np
import os
#from collections import Counter
#from pympler.tracker import SummaryTracker
#tracker = SummaryTracker()
# Plotting
#import seaborn as sns
#import matplotlib as mpl
#import matplotlib.pyplot as plt

# %matplotlib inline

def print_breaks():
    breaks = ""
    for i in range(81):
        breaks = breaks + "="
    print(breaks)

def read_datafiles(filename):
    return pd.read_csv(filename, encoding="ISO-8859-1",\
    dtype={'user_id':np.float64}, low_memory=False)

def getFiles(directory):
    os.chdir(directory)
    return [f for f in os.listdir('.') if os.path.isfile(f)]

def getDataFrame(files_to_import):
    read_files = map(read_datafiles, files_to_import)
    dataframes = map(DataFrame, read_files)
    return pd.concat(dataframes, ignore_index=False)

def removeDuplicates(df_to_clean):
    return df_to_clean.drop_duplicates\
    (["YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN"]).reset_index()

def remove911(df_to_clean):
    (damage, nine_11_index) = max((v, i) for i, v in enumerate(df_to_clean.ACCDMG))
    return df_to_clean.drop([nine_11_index]).reset_index()

def combineNarratives(df_with_sep_narrs):
    numbers = []
    for i in range(1, 16):
        numbers.append("NARR" + str(i))

    ## concat the narratives together functionally
    df_with_combined_narrs = df_with_sep_narrs
    df_with_combined_narrs.NARR = df_with_sep_narrs[numbers].fillna('').sum(1)
    return df_with_combined_narrs

def fixType(df_without_type):
    map_type = {1:"Derailment", 2:"HeadsOn", 3:"Rearmed", 4:"Side", 5 :"Raking", \
    6:"BrokenTrain", 7:"Hwy-Rail", 8:"GradeX", 9:"Obstruction", 10:"Explosive", \
    11:"Fire", 12:"Other", 13:"SeeNarrative"}
    df_with_type = df_without_type
    df_with_type.TYPE = df_without_type.TYPE.map(map_type)
    return df_with_type

def fixTypeQ(df_without_levels):
    # new levels
    map_typeq = {1:"Freight", 2:"Passenger", 3:"Commuter", 4:"Work",\
                 5:"Single", 6:"CutofCars", 7:"Yard", 8:"Light", 9:"Maint"}
    df_with_levels = df_without_levels
    df_with_levels.TYPEQ = df_without_levels.TYPEQ.map(map_typeq)
    return df_with_levels

def dropNAs(df_with_all_data):
    """ Drops NAs - with stipulations

        I thought this required an explanation. It doesn't drop every single na.
        It drops columns with NAs except for TYPE and TYPEQ

        param:
            df_with_all_data - dataframe with columns that contain NAs
        return:
            df_clean_data    - dataframe that has no columns with NAs
    """
    # Save certain columns
    type_temp = df_with_all_data.TYPE
    typeQ_temp = df_with_all_data.TYPEQ

    # Remove anything with an NA
    df_clean_data = df_with_all_data.dropna(axis=1)

    # Add columns back into the df
    df_clean_data.TYPE = df_with_all_data.TYPE
    df_clean_data.TYPEQ = df_with_all_data.TYPEQ

    # Return the clean df
    return df_clean_data

def fixCauseInPlace(df_dirty_cause):
    df_dirty_cause
    return df_clean_cause

def fixCause(df_without_cause):
    causes = df_without_cause.CAUSE
    mycause = Series([])

    for i, c in enumerate(causes):
        if(i%1000 == 0):
            print("Fixed: ", i)
        if c.startswith("T"):
            mycause = mycause.append(Series(["T"]))
        if c.startswith("H"):
            mycause = mycause.append(Series(["H"]))
        if c.startswith("M"):
            mycause = mycause.append(Series(["M"]))
        if c.startswith("E"):
            mycause = mycause.append(Series(["E"]))
        else:
            mycause = mycause.append(Series(["S"]))
    df_with_cause = df_without_cause
    df_with_cause.CAUSE = causes
    return df_with_cause

def main():
    files = []
    init_df = DataFrame()
    try: # encapsulate unsafe IO into a try/catch
        # get files to read
        files = getFiles("/Users/RustyRosti/MSDS/Fall/DS_6001/inclass2/data")
        # read files into a dataframe
        init_df = getDataFrame(files)
    except:
        #Error reading in files
        print("There's some IO error")
        return -1

    print(init_df[["ACCDMG", "TRNSPD",\
    "TONS", "CARSDMG", "TOTINJ", "TOTKLD"]].describe())
    print_breaks()

    no_dups_df = removeDuplicates(init_df)
    no_911_df = remove911(no_dups_df)
    print("The number of rows without dups: ", no_911_df.shape)
    print("The most expensive accident: ", max(no_911_df.ACCDMG))
    print_breaks()

    print("Concatenating Narratives")
    one_narr_df = combineNarratives(no_911_df)
    print(one_narr_df.NARR)
    print_breaks()

    print("Fixing the types")
    fixed_type_df = fixType(one_narr_df)
    print_breaks()

    print("Fixing TypeQ level")
    fixed_levels_df = fixTypeQ(fixed_type_df)
    print_breaks()

    print("Fixing causes")
    fixed_cause_df = fixCause(fixed_levels_df)
    print_breaks()

    print("Removing NAs")
    no_nas_df = dropNAs(fixed_cause_df)
    print_breaks()


    print("Data Wrangled!")
    return 0

if __name__ == "__main__":
    main()
