getPastReturns <- function(h, N, T, data, save = FALSE, prefix = "") {
  
  universe <- as.numeric(data$univ[h,1:N])
  today <- data$inv_dates[h, 1]
  pastPeriod <- (today - T):(today-1)
  pastReturns <- data$ret[pastPeriod, universe, drop = FALSE]
  
  if (save){
    
    list_name <- paste(prefix, "PstRet_N", N, "T", T, sep="")
    
    if (!exists(list_name)) {
      assign(list_name, list(), envir = .GlobalEnv)
    }
    
    updated_list <- get(list_name)
    updated_list[[h]] <- pastReturns
    assign(list_name, updated_list, envir = .GlobalEnv)
    
  }
  return(pastReturns)
}
