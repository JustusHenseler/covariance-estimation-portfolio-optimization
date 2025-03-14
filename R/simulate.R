library(MASS)
library(nlshrink)
library(uuid)
library(R.utils)

source("R/simDCC.R")
source("R/calcKT.R")
source("R/calcLoss.R")

simulate <- function(T, Sigma, estimators, iter, KT = TRUE, timeout = 300){
  
  estimator_names <- names(estimators) #
  K <- length(estimator_names) #
  N <- dim(Sigma)[1] #
  
  loss <- list() #
  
  avgLoss <- data.frame(matrix(NA, nrow = iter, ncol = K)) #
  colnames(avgLoss) <- estimator_names #
  
  avgVar <- data.frame(matrix(NA, nrow = iter, ncol = K)) #
  colnames(avgVar) <- estimator_names #
  
  
  for (j in 1:iter) {
    
    start_time_j <- Sys.time()
    
    loss[[j]] <- data.frame(matrix(NA, nrow = T, ncol = K)) #
    colnames(loss[[j]]) <- estimator_names #
    
    start_time_d <- Sys.time()
    data <- simDCC(T = T, Sigma = Sigma) #
    end_time_d <- Sys.time()
    duration_d <- difftime(end_time_d, start_time_d, units = "secs")
    
    
    X <- data$returns
    Xmeans <- colMeans(X)
    
    # Generate S
    start_time_S <- Sys.time()
    S <- cov(X-Xmeans, use = "complete.obs")
    end_time_S <- Sys.time()
    duration_S <- difftime(end_time_S, start_time_S, units = "secs")
    
    # Generate Skt
    if (KT){
    start_time_Skt <- Sys.time()
    Skt <- calcKT(X-Xmeans)
    end_time_Skt <- Sys.time()
    duration_Skt <- difftime(end_time_Skt, start_time_Skt, units = "secs")}
    
    cat("Iteration ", j,": \n", sep="")
    cat("   Generating returns: ", duration_d, " sec \n", sep="")
    cat("   Calculate S: ", duration_S, " sec \n", sep="")
    if (KT){cat("   Calculate Kendall's Tau: ", duration_Skt, " sec \n", sep="")}
    
    
    for (k in estimator_names) {
      
      start_time_k <- Sys.time()#
      
      func <- estimators[[k]]$func #
      
      # Assign parameters
      if ("X" %in% names(estimators[[k]]$params)){
        estimators[[k]]$params$X <- X
      }
      if ("S" %in% names(estimators[[k]]$params)){
        estimators[[k]]$params$S <- S
      }
      if ("Skt" %in% names(estimators[[k]]$params)){
        estimators[[k]]$params$Skt <- Skt
      }
      
      params <- estimators[[k]]$params
        
      cat("   ", k,": ", sep="")
      
      error_check <- try({
        SigmaHat <- withTimeout(do.call(func, params), timeout = timeout, onTimeout = "error")
      }, silent = TRUE)
      if(class(error_check)[1] == "try-error"){
        cat("Skipped ",k, " for iteration ",j," due to error. ", sep="")
        end_time_k <- Sys.time()
        duration_k <- difftime(end_time_k, start_time_k, units = "secs")
        cat(duration_k, " sec \n", sep="") 
        next
      }
      
      # calculate & save losses for all T cond. cov. of iteration j and estimator k
      loss[[j]][k] <- sapply(data$condCov, function(entry) calcLoss(SigmaHat,entry)) #
      
      # save average loss for iteration j and estimator k
      avgLoss[j,k] <- mean(as.matrix(loss[[j]][k])) #
      avgVar[j,k] <- mean(as.matrix((loss[[j]][k] - avgLoss[j,k])^2)) #
      
      end_time_k <- Sys.time()
      duration_k <- difftime(end_time_k, start_time_k, units = "secs")
      cat(duration_k, " sec \n", sep="")
    }
    
    
  flush.console()
    
  end_time_j <- Sys.time()
  duration_j <- difftime(end_time_j, start_time_j, units = "secs")
  cat("   Overall duration for iteration ", j,": ", duration_j, " sec \n", sep="")
    
  }
  
  overall_avgLoss <- t(as.data.frame(colMeans(avgLoss, na.rm = TRUE)))
  overall_avgVar <- t(as.data.frame(colMeans(avgVar, na.rm = TRUE)))
  
  output <- rbind(overall_avgLoss, overall_avgVar)
  rownames(output) <- c("Avg. Loss", "Avg. Var")
  
  # cat(paste("Results for N = ",N,", T = ",T," (",iter," iterations): \n", sep=""))
  # print(output)
  
  # create table of losses
  return(list(loss = loss, avgLoss = avgLoss, avgVar = avgVar))
}