library(pcaPP)

calcKT <- function(X){
  
  X <- X - colMeans(X)
  cor_KT <- cor.fk(X)
  cor_KT <- sin(0.5 * pi * cor_KT)
  std <- diag(sqrt(diag(cov(X))))
  Skt <- std %*% cor_KT %*% std
  
  return(Skt)
}
