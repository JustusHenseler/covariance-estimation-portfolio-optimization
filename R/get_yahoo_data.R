# Load Scripts & Packages ####
source("R/setup.R")

library(quantmod)
library(rvest)
library(dplyr)
library(TTR)

get_yahoo_data <- function(tickers, start_date, end_date){
  
  # Get adjusted closing prices & volume of stocks, calculate returns ####
  adjusted_closing_prices <- data.frame()
  volume <- data.frame()
  returns <- data.frame()
  for (i in tickers) {
    
    error_check <- try({current_stock <- as.data.frame(getSymbols(i ,from = start_date, to = end_date, src='yahoo', auto.assign = FALSE))}, silent = TRUE)
    
    if(class(error_check) == "try-error"){
      cat("Skipped",i,"due to error.","\n") 
      next
    }
    
    colnames(current_stock)[5] <- i
    volume <- merge(volume, current_stock[i], by = 'row.names', all = TRUE)
    current_stock <- current_stock[-5]
    
    colnames(current_stock)[5] <- i
    adjusted_closing_prices <- merge(adjusted_closing_prices, current_stock[i], by = 'row.names', all = TRUE)
    
    current_returns <- (current_stock[i] - lag(current_stock[i]))/lag(current_stock[i])
    returns <- merge(returns, current_returns, by = 'row.names', all = TRUE)
    
    rownames(returns) <- returns$"Row.names"
    returns <- returns[-1]
    rownames(adjusted_closing_prices) <- adjusted_closing_prices$"Row.names"
    adjusted_closing_prices <- adjusted_closing_prices[-1]
    rownames(volume) <- volume$"Row.names"
    volume <- volume[-1]
  }
  
  returns <- returns[-1,]
  adjusted_closing_prices <- adjusted_closing_prices[-1,]
  volume <- volume[-1,]
  
  return(list("adjusted_closing_prices"=adjusted_closing_prices, "returns"=returns, "volume" = volume))
  
}




