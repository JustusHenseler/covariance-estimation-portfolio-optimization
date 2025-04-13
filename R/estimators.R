library(reticulate)
library(clime)
library(RcppCNPy)
library(uuid)
library(R.matlab)
library(nlshrink)

source("R/quiet.R")
source("R/calcKT.R")

CLIME <- function(X = NULL, S = NULL){
  
  n <- NROW(X)
  p <- NCOL(X)
  if (is.null(S)){
    X <- X- colMeans(X)
    S <- cov(X)
  }
  
  lambda_opt <- sqrt(log(p)/n)
  
  climeres <- clime(S,
                    lambda=lambda_opt,
                    sigma=TRUE,
                    standardize=FALSE,
                    perturb = FALSE)
  cov <- solve(climeres$Omegalist[[1]])
  
  return(cov)
}

CLIMEKT <- function(X = NULL, Skt = NULL){
  
  n <- NROW(X)
  p <- NCOL(X)
  if (is.null(Skt)){
    X <- X- colMeans(X)
    Skt <- calcKT(X)
  }
  
  lambda_opt <- sqrt(log(p)/n)
  
  climeres <- clime(Skt,
                    lambda=lambda_opt,
                    sigma=TRUE,
                    standardize=FALSE,
                    perturb = FALSE)
  cov <- solve(climeres$Omegalist[[1]])
  
  return(cov)
}

library(uuid)
library(R.matlab)

MTP2 <- function(S){
  
  original_dir <- getwd()
  matlab_dir <- paste(original_dir,"/matlab", sep="")
  
  uuid <- UUIDgenerate()
  input_path <- paste(original_dir,"/workspace/MTP2_input_",uuid,".mat", sep ="")
  output_path <- paste(original_dir,"/workspace/MTP2_output_",uuid,".mat", sep ="")
  
  writeMat(input_path, S = as.matrix(S))
  
  command <- sprintf("matlab -nodesktop -nosplash -wait -r \"computeomega \ '%s' \ '%s'; exit;\"", input_path, output_path)
  setwd(matlab_dir)
  system(command)
  setwd(original_dir)
  cov <- readMat(output_path)$Omega
  cov <- solve(cov)
  
  file.remove(c(input_path, output_path))
  
  return(cov)
}

MTP2KT <- function(Skt){
  
  original_dir <- getwd()
  matlab_dir <- paste(original_dir,"/matlab", sep="")
  
  uuid <- UUIDgenerate()
  input_path <- paste(original_dir,"/workspace/MTP2_input_",uuid,".mat", sep ="")
  output_path <- paste(original_dir,"/workspace/MTP2_output_",uuid,".mat", sep ="")
  
  S <- Skt
  writeMat(input_path, S = as.matrix(S))
  
  command <- sprintf("matlab -nodesktop -nosplash -wait -r \"computeomega \ '%s' \ '%s'; exit;\"", input_path, output_path)
  setwd(matlab_dir)
  system(command)
  setwd(original_dir)
  cov <- readMat(output_path)$Omega
  cov <- solve(cov)
  
  file.remove(c(input_path, output_path))
  
  return(cov)
}

NLS <- function(X = NULL){
  
  X <- X- colMeans(X)
  cov <- quiet(nlshrink_cov(as.matrix(X), k=1))
  
  return(cov)
}
