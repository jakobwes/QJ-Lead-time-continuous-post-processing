# Data

- Lorenz:
  - `nwp_train_mean_sd.csv`: NWP forecasts in Lorenz96 system for 8 variables, collapsed into mean and sd -- Training dataset.
  - `nwp_test_mean_sd.csv`: NWP forecasts in Lorenz96 system for 8 variables, collapsed into mean and sd -- Test dataset.
- T2m
  - `t2m_merged_forecast_with_obs_medium.csv`: Met Office MOGREPS-G forecasts of t2m at 40 stations with corresponding SYNOP observations, foreasts collapsed into mean and sd. Main analysis dataset
  - `t2m_merged_forecast_with_obs_short.csv`: Met Office MOGREPS-G forecasts of t2m at 7 stations with corresponding SYNOP observations, foreasts collapsed into mean and sd. Smaller dataset for shorter read-in times (eg. for analysis on station 20).
- Wind speed:
  - `wind_speed_merged_forecast_with_obs_medium.csv`: Met Office MOGREPS-G forecasts of 10m wind speed at 40 stations with corresponding SYNOP observations, foreasts collapsed into mean and sd.
  - `wind_speed_merged_forecast_with_obs_short.csv`: Met Office MOGREPS-G forecasts of 10m wind speed at 7 stations with corresponding SYNOP observations, foreasts collapsed into mean and sd.  Smaller dataset for shorter read-in times (eg. for analysis on station 20).

