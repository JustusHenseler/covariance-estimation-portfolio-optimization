selectStocks <- function(return_data, order_data, N, T, date = "2023-12-29") {

  
  trading_days <- rownames(return_data)
  now <- which(trading_days == date)
  start <- which(trading_days == date)-T+1
  
  return_data <- YahooData$ret[start:now,]
  order_data <- YahooData$vol[start:now,]
  
  # remove NAs & constants
  mostly_non_na <- colSums(!is.na(return_data),na.rm=TRUE) == T
  var_non_zero <- apply(return_data, 2, sd) != 0
  indices_non_na <- which(mostly_non_na & var_non_zero)
  
  stocks_non_na <- colnames(return_data)[indices_non_na] # non NA stocks for h
  number_non_na <- length(stocks_non_na) # number of non NA stocks for h
  
  # how many stocks to consider
  order_current <- colnames(return_data)[order(colSums(order_data), decreasing = TRUE)] # sort stocks by ordering criterion
  relevant_stocks <- order_current[order_current %in% stocks_non_na] # indices of Non-NA stocks sorted by Vol 
  
  current_selection <- relevant_stocks[1:min(N, length(relevant_stocks))] # First 500 Non-NA stocks sorted by Vol 
  
  corr_current <- cor(return_data[,current_selection], use='pairwise.complete.obs')
  high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of highly correlated stocks in first 500
  indices_high_corr <- unique(current_selection[high_corr[,2]]) # indices of highly correlated
  
  while (!dim(high_corr)[1] == 0) { # start loop if there are highly correlated pairs of stocks
    
    current_selection <- relevant_stocks[!relevant_stocks %in% indices_high_corr][1:min(N, length(relevant_stocks)-length(indices_high_corr))] # First 500 except correlated stocks
    
    corr_current <- cor(return_data[,current_selection], use='pairwise.complete.obs') # Check corr again
    high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | (corr_current == 0)), arr.ind = TRUE) # DF of new highly correlated stocks
    indices_high_corr_update <- unique(current_selection[high_corr[,2]])  # indices of new highly correlated
    
    indices_high_corr <- unique(c(indices_high_corr, indices_high_corr_update))
    current_selection <- relevant_stocks[!(relevant_stocks %in% indices_high_corr)][1:min(N, length(relevant_stocks)-length(indices_high_corr))]
  }
  
return(return_data[,current_selection])
}