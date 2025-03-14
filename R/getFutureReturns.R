getFutureReturns <- function(h, P=1, N, data, save = FALSE, prefix = "") {
  
  universe <- as.numeric(data$univ[h,1:N])
  today <- data$inv_dates[h, 1]
  futurePeriod <- today:(today+21*P-1)
  futureReturns <- data$ret[futurePeriod, universe, drop = FALSE]
  
  if (save){
    
    list_name <- paste(prefix, "FtrRet_P", P, "N", N, sep="")
    
    if (!exists(list_name)) {
      assign(list_name, list(), envir = .GlobalEnv)
    }
    
    updated_list <- get(list_name)
    updated_list[[h]] <- futureReturns
    assign(list_name, updated_list, envir = .GlobalEnv)
    
  }
  return(futureReturns)
}