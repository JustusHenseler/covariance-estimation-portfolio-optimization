# Load Scripts & Packages ####
source("R/setup.R")

library(R.matlab)
library(nlshrink)
library(uuid)
library(R.utils)

source("R/getPastReturns.R")
source("R/getFutureReturns.R")
source("R/estimators.R")
source("R/portfolio_weights.R")
source("R/portfolio_returns.R")
source("R/benchmark.R")
source("R/calcKT.R")
source("R/calcLoss.R")
source("R/equal.R")

load("data/YahooData.RData")

# Yahoo
for (h in seq_along(YahooData$inv_dates[,1])) {
  
  getPastReturns(h, 100,  50, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 100, 100, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 100, 200, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 100, 400, data = YahooData, save = T, prefix ="Yahoo")
  
  getPastReturns(h, 200, 100, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 200, 200, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 200, 400, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 200, 800, data = YahooData, save = T, prefix ="Yahoo")
  
  getPastReturns(h, 500, 250, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 500, 500, data = YahooData, save = T, prefix ="Yahoo")
  getPastReturns(h, 500, 800, data = YahooData, save = T, prefix ="Yahoo")
}

save( YahooPstRet_N100T50,      YahooPstRet_N100T100,     YahooPstRet_N100T200,     YahooPstRet_N100T400,
      YahooPstRet_N200T100,     YahooPstRet_N200T200,     YahooPstRet_N200T400,     YahooPstRet_N200T800,
      YahooPstRet_N500T250,     YahooPstRet_N500T500,     YahooPstRet_N500T800,
      file = "workspace/YahooPastReturns.RData")

# Save Future Returns in Workspace ####

for (h in seq_along(YahooData$inv_dates[,1])) {
  getFutureReturns(h, P=1, N=100, data = YahooData, save = T, prefix ="Yahoo")
  getFutureReturns(h, P=1, N=200, data = YahooData, save = T, prefix ="Yahoo")
  getFutureReturns(h, P=1, N=500, data = YahooData, save = T, prefix ="Yahoo")
}

save( YahooFtrRet_P1N100,       YahooFtrRet_P1N200,       YahooFtrRet_P1N500,
      file = "workspace/YahooFutureReturns.RData")

# Calculate Kendall's Tau for Past Returns

YahooSkt_N100T50  <- lapply(YahooPstRet_N100T50, calcKT)
YahooSkt_N100T100 <- lapply(YahooPstRet_N100T100, calcKT)
YahooSkt_N100T200 <- lapply(YahooPstRet_N100T200, calcKT)
YahooSkt_N100T400 <- lapply(YahooPstRet_N100T400, calcKT)

YahooSkt_N200T100 <- lapply(YahooPstRet_N200T100, calcKT)
YahooSkt_N200T200 <- lapply(YahooPstRet_N200T200, calcKT)
YahooSkt_N200T400 <- lapply(YahooPstRet_N200T400, calcKT)
YahooSkt_N200T800 <- lapply(YahooPstRet_N200T800, calcKT)

YahooSkt_N500T250 <- lapply(YahooPstRet_N500T250, calcKT)
YahooSkt_N500T500 <- lapply(YahooPstRet_N500T500, calcKT)
YahooSkt_N500T800 <- lapply(YahooPstRet_N500T800, calcKT)

save( YahooSkt_N100T50,      YahooSkt_N100T100,     YahooSkt_N100T200,     YahooSkt_N100T400,
      YahooSkt_N200T100,     YahooSkt_N200T200,     YahooSkt_N200T400,     YahooSkt_N200T800,
      YahooSkt_N500T250,     YahooSkt_N500T500,     YahooSkt_N500T800,
      file = "workspace/YahooKendallsTau.RData")


# Define Selection of Estimators ####

estimators <- list(NLS    = list(func = NLS,     params = list(X = NULL)),
                   CLIME  = list(func = CLIME,    params = list(X = NULL, S = NULL)),
                   CLIMEKT  = list(func = CLIMEKT,    params = list(X = NULL, Skt = NULL)),
                   MTP2   = list(func = MTP2,    params = list(S = NULL)),
                   MTP2KT   = list(func = MTP2KT,    params = list(Skt = NULL)))
# Apply Estimators ####

load("workspace/YahooData.RData")
load("workspace/YahooPastReturns.RData")         
load("workspace/YahooFutureReturns.RData")         
load("workspace/YahooKendallsTau.RData") 

inv_dates <- YahooData$inv_dates[,1]

# N = 100
results_N100T50  <- benchmark(Past = YahooPstRet_N100T50, Future = YahooFtrRet_P1N100, KendallsTau = YahooSkt_N100T50, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N100T50 <- apply(is.na(results_N100T50$returns),2, sum, na.rm = TRUE)

results_N100T100 <- benchmark(Past = YahooPstRet_N100T100,Future = YahooFtrRet_P1N100, KendallsTau = YahooSkt_N100T100, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N100T100 <- apply(is.na(results_N100T100$returns),2, sum, na.rm = TRUE)

results_N100T200 <- benchmark(Past = YahooPstRet_N100T200,Future = YahooFtrRet_P1N100, KendallsTau = YahooSkt_N100T200, estimators = estimators, inv_dates = inv_dates)
# NA_N100T200 <- apply(is.na(results_N100T200$returns),2, sum, na.rm = TRUE)

results_N100T400 <- benchmark(Past = YahooPstRet_N100T400,Future = YahooFtrRet_P1N100, KendallsTau = YahooSkt_N100T400, estimators = estimators, inv_dates = inv_dates)
# NA_N100T400 <- apply(is.na(results_N100T400$returns),2, sum, na.rm = TRUE)

# N = 200
results_N200T100 <- benchmark(Past = YahooPstRet_N200T100,Future = YahooFtrRet_P1N200, KendallsTau = YahooSkt_N200T100, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N200T100 <- apply(is.na(results_N200T100$returns),2, sum, na.rm = TRUE)

results_N200T200 <- benchmark(Past = YahooPstRet_N200T200,Future = YahooFtrRet_P1N200, KendallsTau = YahooSkt_N200T200, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N200T200 <- apply(is.na(results_N200T200$returns),2, sum, na.rm = TRUE)

results_N200T400 <- benchmark(Past = YahooPstRet_N200T400,Future = YahooFtrRet_P1N200, KendallsTau = YahooSkt_N200T400, estimators = estimators, inv_dates = inv_dates)
# NA_N200T400 <- apply(is.na(results_N200T400$returns),2, sum, na.rm = TRUE)

results_N200T800 <- benchmark(Past = YahooPstRet_N200T800,Future = YahooFtrRet_P1N200, KendallsTau = YahooSkt_N200T800, estimators = estimators, inv_dates = inv_dates)
# NA_N200T800 <- apply(is.na(results_N200T800$returns),2, sum, na.rm = TRUE)

# N = 500
results_N500T250 <- benchmark(Past = YahooPstRet_N500T250,Future = YahooFtrRet_P1N500, KendallsTau = YahooSkt_N500T250, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N500T250 <- apply(is.na(results_N500T250$returns),2, sum, na.rm = TRUE)

results_N500T500 <- benchmark(Past = YahooPstRet_N500T500,Future = YahooFtrRet_P1N500, KendallsTau = YahooSkt_N500T500, estimators = estimators[-2], inv_dates = inv_dates)
# NA_N500T500 <- apply(is.na(results_N500T500$returns),2, sum, na.rm = TRUE)