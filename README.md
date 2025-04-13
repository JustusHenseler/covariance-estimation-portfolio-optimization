# Covariance Matrix Estimation for Portfolio Selection

## Project Description

This project accompanies the empirical analysis and simulation for a seminar project with the focus on covariance matrix estimation for portfolio selection. It includes scripts for data collection, simulation, and empirical analysis of covariance matrix estimation in high-dimensional portfolio selection problems and aims to reproduce the results of [Agrawal et al (2022)](https://doi.org/10.1093/jjfinec/nbaa018) with NASDAQ stock data obtained from Yahoo! Finance.

## Installation

- For the MTP2 estimator of [Slawski & Hein (2015)](https://doi.org/10.1016/j.laa.2014.04.020), the folder "matlab" has to be pasted into the main project folder from [here](https://github.com/uhlerlab/MTP2-finance/).
- A MATLAB license is needed for the MTP2 estimator to work & MATLAB needs to be added to PATH.

## Usage
- Data aquisition, the rolling-window estimator and the simulation study are conducted in main.R

## Future Enhancements
- Convert the project into Python & replace MTP2 MATLAB estimator with Python package to make it more accessible and flexible.
- More flexible benchmarks, i.e. alternatives to GMV portfolios, such as the full Markowitz portfolio with adjustable return level
- Include more estimators for comparison
- Allow for more flexible inclusion of different asset return data
