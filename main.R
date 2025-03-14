source("R/setup.R") # Load necessary packages

source("scripts/get_yahoo_data.R") # Obtain stock returns data from Yahoo! Finance

source("scripts/rolling_window_estimator.R") # Apply rolling window estimator for empirical assessment of estimators

source("scripts/simulation_study.R") # Simulate data to evaluate covariance matrix estimators with synthetic data
