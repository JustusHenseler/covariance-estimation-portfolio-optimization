portfolio_returns <- function (X, w){
  
  cumulative_returns <- apply(X + 1, 2, cumprod)
  
  nav <- cumulative_returns%*%w
  nav_denom <- c(1,nav[1:length(nav)-1])
  
  returns_vector <- (nav/nav_denom)-1
  sum_returns <- sum(returns_vector)
  return(list(sum_returns = sum_returns, returns_vector = returns_vector))
}