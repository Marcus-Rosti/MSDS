# Data
import os
from collections import Counter

import pandas as pd
from pandas.tools.plotting import scatter_matrix


# Statistics

# Plotting
import seaborn as sns
import matplotlib.pyplot as plt
import locale

pd.options.mode.chained_assignment = None

__author__ = 'Marcus Rosti'

sns.set(style="darkgrid", color_codes=True)


locale.setlocale(locale.LC_ALL, '')


########################################################################################################################
#
# cleaning functions
#
########################################################################################################################
def get_all_files(directory, extension=".txt"):
    '''
    Get all files in directory with the specified extension
        and put them into a list.
        The default extension is txt. The input is the path to
        the directory containing the files.
    '''
    file_names = os.listdir(directory)
    all_files = []
    for e in file_names:
        if e.endswith(extension):
            all_files.append(os.path.realpath(e))
    return all_files


def create_list(directory, extension=".txt"):
    '''
    Put all files in the specified directory
    with the chosen extension (txt is the default)
    into a data fame
    '''
    os.chdir(directory)
    files = get_all_files(directory)
    file_list = []
    for index_file, file in enumerate(files):
        file_list.append(pd.read_csv(os.path.realpath(file), low_memory=False, encoding="ISO-8859-1"))
    return file_list


def remove_duplicates(data_frame):
    return data_frame[(data_frame['JOINTCD'] == 1) & (data_frame['TYPE'] != 7)]


def remove_911(data_frame):
    nine11idx = data_frame.ix[(data_frame.ACCDMG > 1.5e7) & (data_frame.YEAR == 1), ["ACCDMG"]].index[0]
    return data_frame.drop(nine11idx)


def concatonate_narratives(data_frame):
    Narratives = []
    for i, _ in enumerate(data_frame["NARR1"]):
        NarrativeList = data_frame.iloc[i]
        Anarrative = ""
        for narr in NarrativeList:
            if pd.isnull(narr):
                break
            else:
                Anarrative += str(narr)
        Narratives.append(Anarrative)
    return 0


def fix_types(data_frame):  # TODO make this more 'functional'
    typeq = data_frame['TYPEQ']
    acctrkcl = data_frame.ACCTRKCL
    ampm = data_frame.AMPM

    data_frame = data_frame.dropna(axis=1)

    data_frame['TYPEQ'] = typeq
    data_frame['ACCTRKCL'] = acctrkcl
    data_frame['AMPM'] = ampm

    map_typeq = {1: "Freight", 2: "Passenger", 3: "Commuter", 4: "Work", 5: "Single",
                 6: "CutofCars", 7: "Yard", 8: "Light", 9: "Maint", 'A': "Maint of Way",
                 '1': "Freight", '2': "Passenger", '3': "Commuter", '4': "Work", '5': "Single",
                 '6': "CutofCars", '7': "Yard", '8': "Light", '9': "Maint", "B": "B", "C": "C",
                 "D": "D", "E": "E"}
    data_frame['TYPEQ'] = data_frame['TYPEQ'].map(map_typeq)

    data_frame['TYPE'] = data_frame['TYPE'].replace([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13],
                                                    ["Derailment", "HeadOn", "Rearend", "Side", "Raking",
                                                     "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction",
                                                     "Explosive", "Fire", "Other", "SeeNarrative"])
    data_frame.CAUSE = data_frame.CAUSE.apply(lambda x: x[0])
    return data_frame


# Define a function to do this for us
def adj_inflation(row):
    """
    Inflation adjustment suggested by D. Rogers
    Add column to adjust ACCDMG for inflation
    Using adjustments from http://data.bls.gov/cgi-bin/cpicalc.pl
    """
    inflation_mult_2000_to_2015_USD = \
        [1.39, 1.35, 1.33, 1.30, 1.26, 1.22, 1.18, 1.15, 1.11, 1.11, 1.09, 1.06, 1.04, 1.02, 1.01, 1]
    cost_year = int(row['YEAR'])
    return row['ACCDMG'] * inflation_mult_2000_to_2015_USD[cost_year]


def adjust_for_inflation(data_frame):
    data_frame.loc[:, 'ACCDMG2015'] = data_frame[['ACCDMG', 'YEAR']].apply(adj_inflation, axis=1)
    return data_frame


def clean_data(directory):  # TODO finish coding this
    '''

    :param directory:
    :return: the clean pandas dataframe
    '''
    # Create the data frame of all accidents
    acts = create_list("/Users/RustyRosti/MSDS/Fall/DS_6001/inclass2/data")
    totacts = pd.concat(acts, ignore_index=True)
    no_dups = remove_duplicates(totacts)
    no_911 = remove_911(no_dups)
    # concated_narrs = concatonate_narratives(no_911)
    clean_types = fix_types(no_911)
    final_frame = adjust_for_inflation(clean_types)
    return final_frame


########################################################################################################################
#
# Stats
#
########################################################################################################################

def print_stats(data_frame):
    print("(a) How many accidents occurred from 2001 to 2014?")
    print(data_frame.shape[0])
    print("(b) What is the total cost of train accidents?")
    print("Raw      $%13.2f" % sum(data_frame['ACCDMG']))
    print("Adjusted $%13.2f" % sum(data_frame['ACCDMG2015']))
    print("(c) What is the average cost of accidents?")
    print("$%14.2f" % data_frame['ACCDMG2015'].mean())
    print("(d) What is the average annual cost?")
    for i in range(1, 15):
        print("for year %2i: $%6.2f" % (i, data_frame['ACCDMG2015'].where(data_frame['YEAR'] == i).mean()))
    print("(e) What is the total number killed in all accidents from 2001 to 2014?")
    print(data_frame["TOTKLD"].sum())
    print("(f) What is the maximum number killed in a single accident?")
    print(data_frame["TOTKLD"].max())
    print("(g) What is the average number of people killed per year?")
    for i in range(1, 15):
        print("for year %2i: %f" % (i, data_frame['TOTKLD'].where(data_frame['YEAR'] == i).mean()))
    print("(h) What is the total number injured in all accidents from 2001 to 2014?")
    print(data_frame["TOTINJ"].sum())
    print("(i) What is the average number of people injured per year?")
    for i in range(1, 15):
        print("for year %2i: %f" % (i, data_frame['TOTINJ'].where(data_frame['YEAR'] == i).mean()))
    print("(j) What is the count of the different types of variables in the total data")
    print(Counter(data_frame.dtypes))


########################################################################################################################
#
# plotting functions
#
########################################################################################################################

def plot_dmg_by_year(data_frame):
    damage = data_frame.groupby(by='YEAR').sum().ACCDMG
    plt.plot(damage)
    plt.scatter(x=list(range(0, 14)), y=list(damage), s=list(data_frame.groupby(by='YEAR').max().ACCDMG / 10000))
    plt.title('Total damage by year')
    plt.ylabel('Total Damage')
    plt.xlabel('Year')
    plt.show()
    return 0


# Plot the yearly costs of accidents as a bar plot
def plot_yearly_costs(data_frame):
    damage = data_frame.groupby(by='YEAR').count().ACCDMG
    plt.bar(left=list(range(2001, 2015)), height=damage, align='center')
    plt.title('Total damage by year')
    plt.ylabel('Number of Accidents')
    plt.xlabel('Year')
    plt.show()
    return 0


def plot_yearly_injuries(data_frame):
    injuries = data_frame.groupby(by='YEAR').sum().TOTINJ
    plt.plot(injuries)
    plt.scatter(x=list(range(0, 14)), y=list(injuries), s=list(data_frame.groupby(by='YEAR').max().TOTINJ * 1.5))
    plt.title('Total Injuries by year')
    plt.ylabel('Number of Injuries')
    plt.xlabel('Year')
    plt.show()
    return 0


# Graph a scatter plot matrix of the variables, ”TRKDMG”, ”EQPDMG”,
# ”ACCDMG”, ”TOTINJ”, ”TOTKLD”, as shown in Figure 4.
def plot_scatter_matrix(data_frame):
    subset_frame = pd.DataFrame()
    subset_frame['TRKDMG'] = data_frame['TRKDMG']
    subset_frame['EQPDMG'] = data_frame['EQPDMG']
    subset_frame['ACCDMG'] = data_frame['ACCDMG']
    subset_frame['TOTINJ'] = data_frame['TOTINJ']
    subset_frame['TOTKLD'] = data_frame['TOTKLD']
    scatter_matrix(subset_frame, alpha=1, figsize=(6, 6), diagonal='bar')
    plt.show()
    return 0


def plot_joint(data_frame):
    sns.jointplot(y=data_frame['ACCDMG'], x=data_frame['TONS'], kind="reg")
    plt.show()


def main():
    os.chdir("/Users/RustyRosti/MSDS/Fall/DS_6001/python_2")
    if os.path.isfile('train_data.pkl'):
        clean_train_data = pd.read_pickle('train_data.pkl')
    else:
        clean_train_data = clean_data("/Users/RustyRosti/MSDS/Fall/DS_6001/inclass2/data")
        os.chdir("/Users/RustyRosti/MSDS/Fall/DS_6001/python_2")
        clean_train_data.read_pickle('train_data.pkl')

    # print_stats(clean_train_data)
    plot_dmg_by_year(clean_train_data)
    plot_yearly_costs(clean_train_data)
    plot_yearly_injuries(clean_train_data)
    print("These two takes a while, don't panic... immediately")
    plot_scatter_matrix(clean_train_data)
    plot_joint(clean_train_data)

    return 0

########################################################################################################################

# Apply inflation adjustment to ACCDMG

if __name__ == "__main__":
    main()
