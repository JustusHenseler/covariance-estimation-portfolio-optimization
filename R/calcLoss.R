library(psych)

calcLoss <- function(SigmaHat, Sigma){
  
  stopifnot(dim(SigmaHat) == dim(Sigma))
  N <- dim(Sigma)[1]
  
  iSH <- solve(SigmaHat)
  num <- tr(iSH %*% Sigma %*% iSH)/N
  denom1 <- (tr(iSH)/N)^2 
  denom2 <- tr(solve(Sigma))/N
  
  loss <- ((num/denom1) - (1/denom2))
  
  return(loss)
}