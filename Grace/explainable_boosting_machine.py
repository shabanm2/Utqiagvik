# import libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, RandomizedSearchCV
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from interpret import set_visualize_provider
from interpret.provider import InlineProvider
set_visualize_provider(InlineProvider())
from interpret.glassbox import ExplainableBoostingRegressor
from interpret import show
from sklearn.metrics import roc_auc_score

# load data
sites = pd.read_csv("C:\\Users\\ellie\\OneDrive\\Desktop\\arctic\\all_sites_daily_gap_filled.csv")
noaa = pd.read_csv("C:\\Users\\ellie\\OneDrive\\Desktop\\arctic\\noaa.csv")

# convert dates to datetime
sites['date'] = pd.to_datetime(sites['day'])
noaa['date'] = pd.to_datetime(noaa['date'])

# drop columns we aren't using and unreliable stations
sites.drop(['Unnamed: 0', 'day'], axis=1, inplace=True)
stations_over_26 = (
    sites
    .groupby('fullname')['airtemp']
    .max()
    .loc[lambda s: s > 26]
)
sites = sites[
    ~(sites['fullname'].isin(stations_over_26.index) &
    (sites['date'] >= '2023-01-01') &
    (sites['date'] <= '2023-08-01'))
]
stations_bad_winddir = (
    sites
    .groupby('fullname')['winddir']
    .min()
    .loc[lambda s: s < 0]
    .index
)
sites = sites[
    ~(sites['fullname'].isin(stations_bad_winddir) &
    (sites['date'] >= '2024-07-15'))
]
sites = sites.dropna()

# drop columns from noaa we aren't using
noaa.drop(['snow', 'wdir', 'wpgt', 'tsun'], axis=1, inplace=True)

# make combined df (not necessary per se but easier to keep track of imo)
merged = pd.merge_asof(sites,
                       noaa,
                       on='date',
                       direction='nearest')

### compute rolling 3 day avgs
def three_day_rolling_avg(df, feature):
    # get mean for each station for each day
    daily = (
        df.groupby(['fullname', 'date'], as_index=False)[feature]
        .mean()
    )
    # sort them 
    daily = daily.sort_values(['fullname', 'date']) 
    # 3 day rolling avg
    daily[f'{feature}_roll3'] = (
        daily.groupby('fullname')[feature]
        .shift(1) # shift so only past days
        .rolling(3, min_periods=1) # set period for calculation (3 days)
        .mean()
        .reset_index(level=0, drop=True)  # keep only the rolling values
    )
    # merge new column to original df
    df = df.merge(
        daily[['fullname', 'date', f'{feature}_roll3']],
        on=['fullname', 'date'],
        how='left'
    )

    return df

# actually compute 3 day rolling avgs for prcp, vwc, airtemp
merged = three_day_rolling_avg(merged, 'prcp')
merged = three_day_rolling_avg(merged, 'vwc')
merged = three_day_rolling_avg(merged, 'airtemp')

### split data for train/test/val
relevant = merged[[ # choose only the columns we care about
    'date',
    'grounddepth',
    'groundtemp',
    'solar',
    'airtemp_roll3',
    'vwc_roll3',
    'prcp_roll3',
    'windspeed',
    'winddir'
]]

# drop nas
relevant = relevant.dropna()

# set up season map
relevant['month'] = pd.to_datetime(relevant['date']).dt.strftime('%b')
season_map = {
    'spring': ['Mar', 'Apr', 'May'],
    'summer': ['Jun', 'Jul', 'Aug'],
    'fall':   ['Sep', 'Oct', 'Nov'],
    'winter': ['Dec', 'Jan', 'Feb']
}

# collect models for each season
ebm_saved_models = {}
ebm_saved_Xtests = {}
ebm_saved_ytests = {}
ebm_results = {}
ebm_feature_importances = {}

for season, months in season_map.items():
    print(f"========= training model on {season} =========")
    season_relevant = relevant[relevant['month'].isin(months)]
    X = season_relevant.select_dtypes(include='number').drop(columns=['groundtemp'])
    y = season_relevant['groundtemp']

    # split data into train/test
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # instantiate model
    ebm = ExplainableBoostingRegressor(
        max_bins=512, # splits feature values into bins
        max_rounds=3000, # max number of training rounds
        interactions=10, # number or variable interactions to include
        learning_rate=0.2, 
        max_leaves=8, # controls complexity of individual trees
        random_state=42 # makes this replicable
    )
    ebm.fit(X_train, y_train)

    # run best combination of parameters on testing data to confirm performance
    preds = ebm.predict(X_test)

    # metrics
    mae = mean_absolute_error(y_test, preds)
    rmse = np.sqrt(mean_squared_error(y_test, preds))
    r2 = r2_score(y_test, preds)
    global_exp = ebm.explain_global()
    data = global_exp.data()

    names = data["names"]
    scores = data["scores"]

    feature_importances = []
    for name, score in zip(names, scores):
        feature_importances.append({
            "season": season,
            "feature": name,
            "importance": np.abs(score).sum()
        })
    ebm_feature_importances[season] = feature_importances
    these_results = {
        'MAE': mae,
        'RMSE': rmse,
        'R2': r2
    }
    print(these_results)
    ebm_results[season] = these_results
    ebm_saved_models[season] = ebm
    ebm_saved_Xtests[season] = X_test
    ebm_saved_ytests[season] = y_test

# print results
for item, importances in ebm_feature_importances.items():
    print(f'====={item}=====')

    df = pd.DataFrame(importances, columns=['feature', 'importance'])

    df = df.sort_values(
        by='importance',
        ascending=False
    )

    print(df)