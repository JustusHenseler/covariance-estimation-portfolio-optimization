source("R/setup.R") # Load necessary packages
source("R/get_yahoo_data.R")
source("R/InvestmentUniverseBuilder.R")
source("R/benchmark_estimators.R")

# Load parameters
source("config.R")

# Obtain stock data
## Load NASDAQ ticker symbols using quantmod package
tickers <- stockSymbols(c("NASDAQ"))$"NASDAQ.Symbol"

## Download return data from Yahoo-Finance
yahoo_stock_data <- get_yahoo_data(tickers, start_date, end_date)
save(yahoo_stock_data, file = "data/yahoo_stock_data.RData")
load(file = "data/yahoo_stock_data.RData")

returns <- yahoo_stock_data$returns
volume <- yahoo_stock_data$volume

## Obtain investment dates & investmant universes
inv_univ_builder <- InvestmentUniverseBuilder$new(
  data = yahoo_stock_data,
  max_N = max_N,
  max_T = max_T,
  inv_period_length = inv_period_length
  )
inv_universe_list <- inv_univ_builder$build_universe()
save(inv_universe_list, file = "data/inv_universe_list.RData")

# Run rolling-window estimator for different combinations of N & T

## Define estimators for comparison
estimators <- list(NLS    = list(func = NLS,     params = list(X = NULL)),
                   CLIME  = list(func = CLIME,    params = list(X = NULL, S = NULL)),
                   CLIMEKT  = list(func = CLIMEKT,    params = list(X = NULL, Skt = NULL)),
                   MTP2   = list(func = MTP2,    params = list(S = NULL)),
                   MTP2KT   = list(func = MTP2KT,    params = list(Skt = NULL)))


estimators <- list(MTP2   = list(func = MTP2,    params = list(S = NULL)),
                   MTP2KT   = list(func = MTP2KT,    params = list(Skt = NULL)))

### Run benchmarks for N = 100
results_N100T50 <- benchmark_estimators(returns, inv_universe_list, 100, 50, inv_period_length, NULL, estimators)
results_N100T100 <- benchmark_estimators(returns, inv_universe_list, 100, 100, inv_period_length, NULL, estimators)
results_N100T200 <- benchmark_estimators(returns, inv_universe_list, 100, 200, inv_period_length, NULL, estimators)
results_N100T400 <- benchmark_estimators(returns, inv_universe_list, 100, 400, inv_period_length, NULL, estimators)

### Run benchmarks for N = 200
results_N200T100 <- benchmark_estimators(returns, inv_universe_list, 200, 100, inv_period_length, NULL, estimators)
results_N200T200 <- benchmark_estimators(returns, inv_universe_list, 200, 200, inv_period_length, NULL, estimators)
results_N200T400 <- benchmark_estimators(returns, inv_universe_list, 200, 400, inv_period_length, NULL, estimators)
results_N200T800 <- benchmark_estimators(returns, inv_universe_list, 200, 800, inv_period_length, NULL, estimators)

### Run benchmarks for N = 500
results_N500T250 <- benchmark_estimators(returns, inv_universe_list, 500, 250, inv_period_length, NULL, estimators)
results_N500T500 <- benchmark_estimators(returns, inv_universe_list, 500, 500, inv_period_length, NULL, estimators)

# Simulation Study

source("scripts/simulation_study.R") # Simulate data to evaluate covariance matrix estimators with synthetic data
