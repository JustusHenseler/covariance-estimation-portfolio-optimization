
stockOrder <- data.frame(matrix(0, nrow = length(inv_dates_rows), ncol = dim(Vol)[2]))

for (h in seq_along(inv_dates_string)){

now <- which(rownames(Ret) == inv_dates_string[h])
start_hist <- now-check_obs # check 
end_forecast <- now+21
indices_non_na <- which(!is.na(Ret[start_hist,]) & !is.na(Ret[end_forecast,])) # numeric indices of stocks which don't have NAs from t-800 to t+21 
stocks_non_na <- colnames(Ret)[indices_non_na] # non NA stocks for h
number_non_na[h] <- length(stocks_non_na) # number of non NA stocks for h

# how many stocks to consider
stockOrder[h,] <- order(colSums(Vol[start_hist:now,]), decreasing = TRUE) # stock names in descending order of volume 

cat("h = ",h,", ",inv_dates_string[h],": ",number_non_na,"\n",sep="")
}


# output <- list(message = message, non_na_stocks = non_na_stocks)
# return(non_na_stocks)
# }