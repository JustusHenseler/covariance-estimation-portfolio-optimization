# Covariance Matrix Estimation for Portfolio Selection

## Project Description

This project accompanies the empirical analysis and simulation for a seminar project with the focus on covariance matrix estimation for portfolio selection. It includes scripts for data collection, simulation, and empirical analysis of covariance matrix estimation in high-dimensional portfolio selection problems and aims to reproduce the results of [Agrawal et al (2022)](https://doi.org/10.1093/jjfinec/nbaa018) with NASDAQ stock data obtained from Yahoo! Finance.

## Installation

### Implement MTP2 Estimator

- For the MTP2 estimator of [Slawski & Hein (2015)](https://www.sciencedirect.com/science/article/pii/S0024379514002444) to work, the folder "matlab" has to be pasted into the main project folder from [here](https://github.com/uhlerlab/MTP2-finance/).
- MATLAB license is needed for the MTP2 estimator to work, MATLAB needs to be added to PATH.

## Usage

### 1. Data Acquisition

- The script `get_yahoo_data.R` downloads historical stock price data from Yahoo Finance and saves it in the `data/` directory.

### 2. Simulation Study

- `simulation_study.R` runs a Monte Carlo simulation to compare different covariance matrix estimators.
- Uses DCC-GARCH to construct synthetic return data.

### 3. Rolling-Window Estimator

- `rolling_window_estimator.R` conducts an empirical study based on historical financial data and the GMV portfolio.
- Implements a rolling window approach for genuine out-of-sample performance evaluation of covariance estimators in terms of portfolio optimiziation.

## Future Enhancements
- More flexible benchmarks, alternatives to GMV portfolios, such as the full Markowitz portfolio with adjustable return level
- Include more estimators for comparison
- Allow for more flexible inclusion of different asset return data
