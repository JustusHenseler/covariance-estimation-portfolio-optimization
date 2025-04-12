# Load Scripts & Packages ####
source("R/setup.R")

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

max_T <- 800
max_N <- 500
non_na_share <- 0.9 
check_corr_obs <- 100
inv_period_length <- 21


get_valid_tickers <- function(period) {
  
  # Convert period to index if it's a date
  if (is.character(period)) {
    period <- which(rownames(Ret) == period)
    if (length(period) == 0) stop("Provided period not found in rownames.")
  }
  
  n_obs <- nrow(Ret)
  if ((period - max_T) < 1 || (period + inv_period_length) > n_obs) {
    return(character(0))  # Not enough history or future data
  }
  
  # Define windows
  hist_idx <- (period - max_T):period
  fcast_idx <- (period + 1):(period + inv_period_length)
  
  # Subset data frames
  Ret_hist <- Ret[hist_idx, , drop = FALSE]
  Vol_hist <- Vol[hist_idx, , drop = FALSE]
  Ret_fcast <- Ret[fcast_idx, , drop = FALSE]
  
  # Criterion 1: sufficient non-NA observations in history
  valid_non_na <- colSums(!is.na(Ret_hist)) >= max_T * non_na_share &
    colSums(!is.na(Vol_hist)) >= max_T * non_na_share
  
  # Criterion 2: non-zero standard deviation in last max_T periods
  valid_sd <- apply(Ret_hist, 2, function(x) sd(x, na.rm = TRUE) > 0)
  
  # Criterion 3: complete returns in future investment period
  valid_future <- colSums(!is.na(Ret_fcast)) == inv_period_length
  
  # Combine all criteria
  valid <- valid_non_na & valid_sd & valid_future
  
  return(names(which(valid)))
}


# 1. Starte bei der ersten Periode, wo Ret UND Vol mindestens max_N non-NAs haben
valid_start <- which(apply(!is.na(Ret), 1, sum) >= max_N &
                       apply(!is.na(Vol), 1, sum) >= max_N)[1]

# 2. Beginne dann bei valid_start + max_T, damit genug Historie vorhanden ist
search_start <- valid_start + max_T

# 3. Initialisiere Variable zum Speichern
first_valid_period <- NA

# 4. Iteriere durch Perioden (ab search_start)
for (i in search_start:nrow(Ret)) {
  period <- rownames(Ret)[i]
  valid_tickers <- get_valid_tickers(period)
  
  message(sprintf("Checking period %s: %d valid tickers", period, length(valid_tickers)))
  
  if (length(valid_tickers) >= max_N) {
    first_valid_period <- period
    message(sprintf("First valid period: %s with %d tickers", period, length(valid_tickers)))
    break
  }
}

first_inv_date <- period
# first_inv_date <- '1998-06-03' # from here there are enough data points for all periods (500+)
dates <- rownames(Ret)
dates_inv_periods <- dates[(dates >= as.Date(first_inv_date)) & (dates <= as.Date(dates[length(dates)-inv_period_length]))]
inv_dates_string <- dates_inv_periods[seq(1, length(dates_inv_periods), by = inv_period_length)]

#######

get_investment_universe <- function(period) {
  
  valid_tickers <- get_valid_tickers(period)
  
  # Convert period to index if it's a date
  if (is.character(period)) {
    period_id <- which(rownames(Ret) == period)
    if (length(period_id) == 0) stop("Provided period not found in rownames.")
  }
  
  # Check if relevant data points are available
  if (period_id - max_T < 1 || period_id + inv_period_length > nrow(Ret)) {
    stop("Insufficient data for this period.")
  }
  
  # Define windows
  hist_idx <- (period_id - max_T):period_id
  vol_idx <- (period_id - inv_period_length):period_id
  fcast_idx <- (period_id + 1):(period_id + inv_period_length)
  
  # Order stocks by volume
  ordered_tickers <- names(sort(colSums(Vol[vol_idx, valid_tickers, drop = FALSE]), decreasing = TRUE))
  
  # Check first max_N stocks for correlated pairs
  selection <- head(ordered_tickers, max_N)# First max_N Non-NA stocks sorted by Vol 
  
  corr_current <- cor(Ret[hist_idx,selection], use='pairwise.complete.obs') # Correlation of first max_N 
  high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of highly correlated stocks
  indices_high_corr <- unique(selection[high_corr[,2]]) # indices of highly correlated
  
  while (nrow(high_corr) > 0) { # start loop if there are highly correlated pairs of stocks
    
    available <- ordered_tickers[!ordered_tickers %in% indices_high_corr]# First max_N Stocks except correlated stocks
    selection <- head(available, max_N)
    
    corr_current <- cor(Ret[hist_idx, selection], use='pairwise.complete.obs') # Check corr again
    high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of new highly correlated stocks
    indices_high_corr_update <- unique(selection[high_corr[,2]])  # indices of new highly correlated
    
    indices_high_corr <- c(indices_high_corr, indices_high_corr_update)
    selection <- ordered_tickers[!ordered_tickers %in% indices_high_corr][1:max_N]
  }
  
  cat(sprintf("Period: %s — %d stocks selected, %d removed due to high correlation.\n", 
              period, length(selection), length(indices_high_corr)))
  return(list(selected = selection, excluded = indices_high_corr))
  
}

inv_universe_list <- list()
for(period in inv_dates_string){
  inv_universe_list[[period]] <- get_investment_universe(period)$selected
} 
save(inv_universe_list, file = "data/inv_universe_list.RData")


