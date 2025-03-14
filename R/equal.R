equal <- function(X){
  N <- dim(X)[2]
  cov <- diag(rep(1,N))
  return(cov)
}