library(psych)

simDCC <- function(T, Sigma, param = list(a = 0.05, b = 0.9, alpha = 0.05, beta = 0.93)){

  # Generate synthetic data
  N <- dim(Sigma)[1]
  mu <- rep(0, N)
  
  a <- param$a
  b <- param$b
  
  alpha <- param$alpha
  beta <- param$beta
  
  ## Initialize process
  
  r <- data.frame(matrix(NA, nrow = T+1, ncol = N))
  r[1,] <- mvrnorm(n = 1, mu = mu, Sigma = Sigma) # initialize returns with draws from uncond. distr.
  # r[1,] <- rep(0, N)
  
  d <- data.frame(matrix(NA, nrow = T+1, ncol = N))
  d[1,] <- sqrt(diag(Sigma)) # initialize d with uncond. s.d.
  
  
  H <- list()
  Q <- list()
  Q[[1]] <- cov2cor(Sigma) # initialize with unconditional covariance matrix
  
  
  ## Generate T data points
  
  for (t in 2:(T+1)){
    # For individual assets i in t
    for (i in 1:N) {
      omega <- (1-a-b)*diag(Sigma)[i] # set omega such that variance of returns in GARCH(1,1) is stationary
      d[t,i] <- sqrt(omega+a*r[t-1,i]^2+b*d[t-1,i]^2)
    }
    
    s_prev <- t(as.matrix(r[t-1,]/d[t-1,]))
    D <- diag(d[t,])
    
    # Generate joint distribution for t
    Q[[t]] <- (1-alpha-beta)*cov2cor(Sigma)+alpha*(s_prev %*% t(s_prev))+beta*Q[[t-1]]
    
    invSqrtDiagQ <- diag(diag(Q[[t]])^(-0.5))
    R <- invSqrtDiagQ %*% Q[[t]] %*% invSqrtDiagQ
    
    H[[t-1]] <- D %*% R %*% D
    
    r[t,] <- mvrnorm(n = 1, mu = mu, Sigma = H[[t-1]])
  }
  
  
  
  r <- r[-1,]
  rownames(r) <- 1:T
  
  return(list(returns = r, condCov = H))
  
}