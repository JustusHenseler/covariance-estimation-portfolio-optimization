source("R/estimators.R")
source("R/portfolio_weights.R")
source("R/portfolio_returns.R")

library(R.matlab)
library(nlshrink)
library(uuid)
library(R.utils)

benchmark <- function(returns, inv_universe_list, max_N, max_T, inv_period_length, kendalls_tau_list, estimators, timeout = 300) {
  
  estimator_names <- names(estimators)
  univ_returns <- data.frame(matrix(NA, nrow = length(inv_universe_list), ncol = length(estimator_names)))
  rownames(univ_returns) <- names(inv_universe_list)
  colnames(univ_returns) <- estimator_names
  
  for (period in names(inv_universe_list)) {
    
    start_time_period <- Sys.time()
    tickers <- inv_universe_list[[period]]
    tickers <- head(tickers, max_N)
    
    period_id <- which(rownames(returns) == period)
    if (length(period_id) == 0 || period_id - max_T < 1 || period_id + inv_period_length > nrow(returns)) {
      warning(paste("Skipping period", period, "due to insufficient data."))
      next
    }
    hist_idx <- (period_id - max_T):period_id
    fcast_idx <- (period_id + 1):(period_id + inv_period_length)
    
    X <- returns[hist_idx, tickers, drop = FALSE]
    Y <- returns[fcast_idx, tickers, drop = FALSE]
    Xmeans <- colMeans(X, na.rm = TRUE)
    Skt <- kendalls_tau_list[[period]]
    
    corr <- cor(X, use = 'pairwise.complete.obs')
    high_corr <- which(upper.tri(corr, diag = FALSE) & (abs(corr) > 0.95 | is.na(corr) | (corr == 0)), arr.ind = TRUE)
    indices_high_corr <- unique(high_corr[,2])
    
    if (length(indices_high_corr) > 0) {
      omit_tickers <- colnames(X)[indices_high_corr]
      X <- X[, -indices_high_corr, drop = FALSE]
      Y <- Y[, -indices_high_corr, drop = FALSE]
      Xmeans <- colMeans(X, na.rm = TRUE)
      Skt <- Skt[-indices_high_corr, -indices_high_corr]
      cat("Omitted ",length(indices_high_corr)," stocks due to high correlation ","(N=",N,"):","\n", paste(ticker_symbols_omitted, sep = '', collapse = ' '),"\n", sep="")
    }
    
    X_centered <- sweep(X, 2, Xmeans)
    S <- cov(X_centered, use = "complete.obs")
    
    for (estimator in estimator_names) {
      start_time_estimator <- Sys.time()
      func <- estimators[[estimator]]$func
      params <- estimators[[estimator]]$params
      
      if ("X" %in% names(params)) params$X <- X
      if ("S" %in% names(params)) params$S <- S
      if ("Skt" %in% names(params)) params$Skt <- Skt
      
      cat("   ", estimator,": ", sep="")
      
      error_check <- try({
        cov <- withTimeout(do.call(func, params), timeout = timeout, onTimeout = "error")
        w <- portfolio_weights(cov)
        univ_returns[period, estimator] <- portfolio_returns(Y, w)$sum_returns
      }, silent = TRUE)
      
      if (inherits(error_check, "try-error")) {
        warning(paste("Skipped", estimator, "for period", period, "due to error."))
      }
      end_time_estimator <- Sys.time()
      duration_estimator <- difftime(end_time_estimator, start_time_estimator, units = "secs")
      cat(duration_estimator, " sec \n", sep="")
    }
    
    
    flush.console()
    
    end_time_period <- Sys.time()
    duration_period <- difftime(end_time_period, start_time_period, units = "secs")
    cat("   Overall duration for investment period ",period,": ", duration_period, " sec \n", sep="")
    
  }  
  
  results <- list()
  results$"sd" <- 100*sqrt(12)*sapply(univ_returns, sd, na.rm = TRUE)
  results$"returns" <- univ_returns
  
  return(results)
}