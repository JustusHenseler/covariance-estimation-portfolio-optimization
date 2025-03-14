# Load Scripts & Packages ####
source("R/setup.R")
source("R/convertMatlabDates.R")

library(quantmod)
library(rvest)
library(dplyr)
library(TTR)

# Select start and end date of stock data
start_date <- "1983-01-01"
end_date <- "2023-12-31"

## Get NASDAQ ticker symbols 
tickers <- stockSymbols(c("NASDAQ"))$"NASDAQ.Symbol"

# Get adjusted closing prices & volume of stocks, calculate returns ####
Adj <- data.frame()
Vol <- data.frame()
Ret <- data.frame()
for (i in tickers) {
  
  error_check <- try({current_stock <- as.data.frame(getSymbols(i ,from = start_date, to = end_date, src='yahoo', auto.assign = FALSE))}, silent = TRUE)
  
  if(class(error_check) == "try-error"){
    cat("Skipped",i,"due to error.","\n") 
    next
    }
  
  colnames(current_stock)[5] <- i
  Vol <- merge(Vol, current_stock[i], by = 'row.names', all = TRUE)
  current_stock <- current_stock[-5]
  
  colnames(current_stock)[5] <- i
  Adj <- merge(Adj, current_stock[i], by = 'row.names', all = TRUE)
  
  current_Ret <- (current_stock[i] - lag(current_stock[i]))/lag(current_stock[i])
  Ret <- merge(Ret, current_Ret, by = 'row.names', all = TRUE)
  
  rownames(Ret) <- Ret$"Row.names"
  Ret <- Ret[-1]
  rownames(Adj) <- Adj$"Row.names"
  Adj <- Adj[-1]
  rownames(Vol) <- Vol$"Row.names"
  Vol <- Vol[-1]
}

Ret <- Ret[-1,]
Adj <- Adj[-1,]
Vol <- Vol[-1,]

# save(Ret, Adj, Vol, file = "data/retrievedDataNASDAQ83.RData")

load("data/retrievedDataNASDAQ83.RData") 

# Import investment dates ####
orig_dates <- read.csv('data/mydatestr.txt', header = FALSE)
inv_dates <- read.csv('data/investDateIdx.csv', header = FALSE)

# Replace MATLAB time format ####
inv_dates_string <- orig_dates[as.vector(inv_dates$"V1"),]
inv_dates_string <- sapply(inv_dates_string, convertMatlabDates)
inv_dates_string <- unname(inv_dates_string)
inv_dates_string <- format(as.Date(inv_dates_string,"%d-%m-%Y"), "%Y-%m-%d")

# Shift old investment dates by ~8 years, i.e. 2001 trading days ####
for (h in seq_along(inv_dates_string)){
  inv_dates_string[h] <- rownames(Ret)[which(rownames(Ret) == inv_dates_string[h])+2001]
}

# Remove first 54 h, since they don't allow to estimate based on a history of 500 complete returns
inv_dates_string <- inv_dates_string[-c(1:54)]

#######

dates <- rownames(Ret)
dates_inv_periods <- dates[(dates >= as.Date('1998-06-03')) & (dates <= as.Date(dates[length(dates)-21]))]
inv_dates_string <- dates_inv_periods[seq(1, length(dates_inv_periods), by = 21)]

#######


check_obs <- 800 # check how many stocks have a full history of check_obs/800 periods
check_corr <- 100
stockOrder <- data.frame(matrix(0, nrow = length(inv_dates_string), ncol = dim(Vol)[2]))

for (h in seq_along(inv_dates_string)){
  
  now <- which(rownames(Ret) == inv_dates_string[h])
  start_Vol <- now-21
  start_hist <- now-check_obs # check 
  start_corr <- now-check_corr # check 
  end_forecast <- now+21
  
    mostly_non_na <- colSums(!is.na(Ret[start_hist:now,]),na.rm=TRUE) == 1*(check_obs+1)
    future_non_na <- !is.na(colSums(!is.na(Ret[now:end_forecast,])))
    var_non_zero <- apply(Ret[start_hist:now,], 2, sd) != 0
  
  indices_non_na <- which(mostly_non_na & future_non_na & var_non_zero)
  
  #indices_non_na <- which(!is.na(Ret[start_hist,]) & !is.na(Ret[end_forecast,])) # numeric indices of stocks which don't have NAs from t-800 to t+21 
  stocks_non_na <- colnames(Ret)[indices_non_na] # non NA stocks for h
  number_non_na <- length(stocks_non_na) # number of non NA stocks for h
  
  # how many stocks to consider
  order_Vol_current <- colnames(Ret)[order(colSums(Vol[start_Vol:now,]), decreasing = TRUE)] # sort stocks by Vol
  in_current <- order_Vol_current[order_Vol_current %in% stocks_non_na] # indices of Non-NA stocks sorted by Vol 
  
  first500 <- in_current[1:min(500, length(in_current))] # First 500 Non-NA stocks sorted by Vol 
  
  corr_current <- cor(Ret[start_corr:now,first500], use='pairwise.complete.obs') # Correlation of first 500 
  high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of highly correlated stocks in first 500
  indices_high_corr <- unique(first500[high_corr[,2]]) # indices of highly correlated
  
  while (!dim(high_corr)[1] == 0) { # start loop if there are highly correlated pairs of stocks
    
    first500 <- in_current[!in_current %in% indices_high_corr][1:min(500, length(in_current)-length(indices_high_corr))] # First 500 except correlated stocks
    
    corr_current <- cor(Ret[start_corr:now,first500], use='pairwise.complete.obs') # Check corr again
    high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of new highly correlated stocks
    indices_high_corr_update <- unique(first500[high_corr[,2]])  # indices of new highly correlated
    
    indices_high_corr <- c(indices_high_corr, indices_high_corr_update)
    first500 <- in_current[!in_current %in% indices_high_corr][1:min(500, length(in_current)-length(indices_high_corr))]
  }
  
  number_NonNAHighCorr <- length(in_current[!in_current %in% indices_high_corr])
  
  out_current <- order_Vol_current[!order_Vol_current %in% first500]
  order_current <- c(first500, out_current)
  
  stockOrder[h,] <- match(order_current, colnames(Ret)) # stock names in descending order of volume 
  
  cat("h = ",h,", ",inv_dates_string[h]," - No. Non-NAs: ",number_non_na,", w/o high correlation: ",number_NonNAHighCorr,"\n",sep="")
}

stockOrder <- stockOrder[,1:500]
inv_dates_rows <- which(rownames(Ret) %in% inv_dates_string)

YahooData <- list(ret = Ret, univ = stockOrder, dates = data.frame(rownames(Ret)), inv_dates = data.frame(inv_dates_rows), vol = Vol)
save(YahooData, file = "data/YahooData.RData")


