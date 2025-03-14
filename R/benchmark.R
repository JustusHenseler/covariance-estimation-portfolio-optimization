source("R/estimators.R")
source("R/portfolio_weights.R")
source("R/portfolio_returns.R")

library(R.matlab)
library(nlshrink)
library(uuid)
library(R.utils)


benchmark <- function(Past, Future, KendallsTau, inv_dates, estimators, timeout = 300, KT = TRUE) {
  
  estimator_names <- names(estimators) #
  univ_returns <- data.frame(matrix(NA, nrow = length(inv_dates), ncol = length(estimator_names)))
  colnames(univ_returns) <- estimator_names
  
  for (h in seq_along(inv_dates)) {
    
    start_time_h <- Sys.time()
    
    # Get data for investment period from input
    Y <- Future[[h]]
    X <- Past[[h]]
    Xmeans <- colMeans(X)
    Skt <- KendallsTau[[h]]
    
    corr <- cor(X, use='pairwise.complete.obs')
    high_corr <- which(upper.tri(corr, diag = FALSE) & (abs(corr) > 0.95 | is.na(corr) | (corr == 0)), arr.ind = TRUE) # DF of highly correlated stocks
    indices_high_corr <- unique(high_corr[,2])
    ticker_symbols_omitted <- colnames(X)[indices_high_corr]
    number_omitted <- length(indices_high_corr)
    N <- dim(X)[2]
    
    cat("h = ",h,"\n", sep="")
    
    if (number_omitted>0){
      
      # Update data, exclude highly correlated
      X <- X[,-indices_high_corr]
      Xmeans <- colMeans(X)
      Y <- Y[,-indices_high_corr]
      Skt <- Skt[-indices_high_corr,-indices_high_corr]
      N <- dim(X)[2]
      
      cat("Omitted ",number_omitted," stocks due to high correlation ","(N=",N,"):","\n", paste(ticker_symbols_omitted, sep = '', collapse = ' '),"\n", sep="")
    }
    
    
    # Generate S
    S <- cov(X-Xmeans, use = "complete.obs")
    
    
    for (k in estimator_names) {
      
      start_time_k <- Sys.time()
      
      func <- estimators[[k]]$func #
      params <- estimators[[k]]$params
      
      # Assign parameters
      if ("X" %in% names(estimators[[k]]$params)){
        params$X <- X
      }
      if ("S" %in% names(estimators[[k]]$params)){
        params$S <- S
      }
      if ("Skt" %in% names(estimators[[k]]$params)){
        params$Skt <- Skt
      }
      
      
      
      cat("   ", k,": ", sep="")
      
      error_check <- try({
        cov <- withTimeout(do.call(func, params), timeout = timeout, onTimeout = "error")
        w <- portfolio_weights(cov)
        univ_returns[h,k] <- portfolio_returns(Y, w)$sum_returns 
      }, silent = TRUE)
      if(class(error_check)[1] == "try-error"){
        cat("Skipped ",k, " for investment period ",h," due to error. ", sep="") 
      }
      
      end_time_k <- Sys.time()
      duration_k <- difftime(end_time_k, start_time_k, units = "secs")
      cat(duration_k, " sec \n", sep="")
    }
    
    flush.console()
    
    end_time_h <- Sys.time()
    duration_h <- difftime(end_time_h, start_time_h, units = "secs")
    cat("   Overall duration for investment period ",h,": ", duration_h, " sec \n", sep="")
    
  }
  
  results <- list()
  results$"sd" <- 100*sqrt(12)*sapply(univ_returns, sd, na.rm = TRUE)
  results$"returns" <- univ_returns
  
  return(results)
}