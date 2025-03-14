# Load Scripts & Packages ####
source("R/setup.R")

library(MASS)
library(R.matlab)
library(psych)

source("R/simDCC.R")
source("R/calcLoss.R")
source("R/estimators.R")
source("R/simulate.R")
source("R/calcKT.R")
source("R/selectStocks.R")

# Create Sigma from NASDAQ dataset
load("data/YahooData.RData")

Ret500 <- selectStocks(return_data = YahooData$ret, order_data = YahooData$vol, N = 500, T = 1510, date = "2019-12-31")
Ret500 <- Ret500-colMeans(Ret500)
Sigma500 <- NLS(Ret500)

Ret200 <- selectStocks(return_data = YahooData$ret, order_data = YahooData$vol, N = 200, T = 1510, date = "2019-12-31")
Ret200 <- Ret200-colMeans(Ret200)
Sigma200 <- NLS(Ret200)

Ret100 <- selectStocks(return_data = YahooData$ret, order_data = YahooData$vol, N = 100, T = 1510, date = "2019-12-31")
Ret100 <- Ret100-colMeans(Ret100)
Sigma100 <- NLS(Ret100)

# Define Estimators
estimators_sim <- list( NLS    = list(func = NLS,     params = list(X = NULL)),
                        CLIME  = list(func = CLIME,    params = list(X = NULL, S = NULL)),
                        MTP2   = list(func = MTP2,    params = list(S = NULL)))

simN100 <- simulate(T = 500, Sigma = Sigma100, estimators_sim, iter = 250, KT = FALSE)
simN200 <- simulate(T = 500, Sigma = Sigma200, estimators_sim, iter = 125, KT = FALSE)
simN500 <- simulate(T = 500, Sigma = Sigma500, estimators_sim, iter = 50, KT = FALSE)

colMeans(simN100$avgLoss, na.rm = TRUE)
colMeans(simN200$avgLoss, na.rm = TRUE)
colMeans(simN500$avgLoss, na.rm = TRUE)

colMeans(sqrt(simN100$avgVar), na.rm = TRUE)
colMeans(sqrt(simN200$avgVar), na.rm = TRUE)
colMeans(sqrt(simN500$avgVar), na.rm = TRUE)
