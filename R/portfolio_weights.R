portfolio_weights <- function (cov, R=NULL, global = FALSE){
  if (global == FALSE){
    
    ones <- rep(1, dim(cov)[1])
    inv <- solve(cov)
    
    weights <- (inv%*%ones)*drop(1/(t(ones)%*%inv%*%ones))
  } else {
  
  }
  return(weights)
}