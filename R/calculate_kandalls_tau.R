source("R/calcKT.R")

calculate_kandalls_tau <- function(returns, inv_universe_list, N, T){
  
  kendalls_tau_list <- list()
  for (period in names(inv_universe_list)) {
    if (verbose) cat("\nStarting investment period: ", period, "\n", sep = "")
    start_time_period <- Sys.time()
    tickers <- inv_universe_list[[period]]
    tickers <- head(tickers, N)
    
    period_id <- which(rownames(returns) == period)
    if (length(period_id) == 0 || period_id - T < 1 || period_id + inv_period_length > nrow(returns)) {
      warning(paste("   Skipping period", period, "due to insufficient data."))
      next
    }
    
    hist_idx <- (period_id - T):period_id
    X <- returns[hist_idx, tickers, drop = FALSE]
    
    kendalls_tau_list[period] = calcKT(X)
    
    flush.console()
    
    end_time_period <- Sys.time()
    duration_period <- difftime(end_time_period, start_time_period, units = "secs")
    if (verbose) cat("   Overall duration for investment period ",period,": ", duration_period, " sec \n", sep="")
    
  }
  
  return(kendalls_tau_list)
  }