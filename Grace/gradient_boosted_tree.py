# import libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, RandomizedSearchCV
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import shap
from xgboost import XGBRegressor

# load data
sites = pd.read_csv("all_sites_daily_gap_filled.csv")
noaa = pd.read_csv("noaa.csv")

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

# select variables
X = (relevant
    .drop(columns=['groundtemp', 'date'])
    .reset_index(drop=True)
    .select_dtypes(include='number'))

y = relevant['groundtemp']

# set up season map
relevant['month'] = pd.to_datetime(relevant['date']).dt.strftime('%b')
season_map = {
    'spring': ['Mar', 'Apr', 'May'],
    'summer': ['Jun', 'Jul', 'Aug'],
    'fall':   ['Sep', 'Oct', 'Nov'],
    'winter': ['Dec', 'Jan', 'Feb']
}

# collect models and results for each season
saved_models = {}
saved_Xtests = {}
saved_ytests = {}
results = {}
feature_importances = {}

for season, months in season_map.items():
    print(f"========= training model on {season} =========")
    season_relevant = relevant[relevant['month'].isin(months)]
    X = season_relevant.select_dtypes(include='number').drop(columns=['groundtemp'])
    y = season_relevant['groundtemp']

    # split data into train/test
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    X_test, X_val, y_test, y_val = train_test_split(X_test, y_test, test_size=0.2, random_state=42)

    # instantiate model
    xgb_model = XGBRegressor(n_estimators=1400,
                            learning_rate=0.02,
                            max_depth=8,
                            booster='gbtree',
                            objective='reg:squarederror')
    xgb_model.fit(X_train, y_train,
                eval_set=[(X_val, y_val)],
                verbose=False)

    # run best combination of parameters on testing data to confirm performance
    preds = xgb_model.predict(X_test)

    # metrics
    mae = mean_absolute_error(y_test, preds)
    print(f"mean absolute error: {mae}")
    rmse = np.sqrt(mean_squared_error(y_test, preds))
    print(f"rmse: {rmse}")
    r2 = r2_score(y_test, preds)
    importances = xgb_model.feature_importances_

    feature_importance_df = pd.DataFrame({
        'Feature': X.columns,
        'Importance': importances
    }).sort_values(by='Importance', ascending=True)

    these_results = {
        'MAE': mae,
        'RMSE': rmse,
        'R2': r2
    }
    results[season] = these_results
    feature_importances[season] = feature_importance_df
    saved_models[season] = xgb_model
    saved_Xtests[season] = X_test
    saved_ytests[season] = y_test

# display shap values
for season in saved_models.keys():
  X_sample = saved_Xtests[season].sample(
      n=min(500, len(saved_Xtests[season])),
      random_state=42
  )
  explainer = shap.TreeExplainer(saved_models[season])
  shap_vals = explainer.shap_values(X_sample)
  print(f'====== displaying SHAP summary for {season} ======')
  shap.summary_plot(shap_vals, X_sample)