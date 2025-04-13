library(R6)

InvestmentUniverseBuilder <- R6Class("InvestmentUniverseBuilder",
    public = list(
    data = NULL,
    max_N = NULL,
    max_T = NULL,
    non_na_share = NULL,
    check_corr_obs = NULL,
    inv_period_length = NULL,
    inv_universe_list = list(),
                                         
    initialize = function(returns, volume, max_N, max_T, non_na_share = 0.9, check_corr_obs = 100, inv_period_length = 21) {
      self$returns <- data$returns
      self$volume <- data$volume
      self$max_N <- max_N
      self$max_T <- max_T
      self$non_na_share <- non_na_share
      self$check_corr_obs <- check_corr_obs
      self$inv_period_length <- inv_period_length
      },
  
    get_valid_tickers = function(period) {
      if (is.character(period)) {
       period <- which(rownames(self$returns) == period)
       if (length(period) == 0) stop("Provided period not found in rownames.")
      }
      
      n_obs <- nrow(self$returns)
      if ((period - self$max_T) < 1 || (period + self$inv_period_length) > n_obs) {
       return(character(0))
      }
      
      hist_idx <- (period - self$max_T):period
      fcast_idx <- (period + 1):(period + self$inv_period_length)
      
      Ret_hist <- self$returns[hist_idx, , drop = FALSE]
      Vol_hist <- self$volume[hist_idx, , drop = FALSE]
      Ret_fcast <- self$returns[fcast_idx, , drop = FALSE]
      
      valid_non_na <- colSums(!is.na(Ret_hist)) >= self$max_T * self$non_na_share &
       colSums(!is.na(Vol_hist)) >= self$max_T * self$non_na_share
      
      valid_sd <- apply(Ret_hist, 2, function(x) sd(x, na.rm = TRUE) > 0)
      valid_future <- colSums(!is.na(Ret_fcast)) == self$inv_period_length
      
      valid <- valid_non_na & valid_sd & valid_future
      return(names(which(valid)))
      },
  
    find_first_valid_period = function() {
      valid_start <- which(apply(!is.na(self$returns), 1, sum) >= self$max_N &
                            apply(!is.na(self$volume), 1, sum) >= self$max_N)[1]
      
      search_start <- valid_start + self$max_T
      first_valid_period <- NA
      
      for (i in search_start:nrow(self$returns)) {
       period <- rownames(self$returns)[i]
       valid_tickers <- self$get_valid_tickers(period)
       message(sprintf("Checking period %s: %d valid tickers", period, length(valid_tickers)))
       if (length(valid_tickers) >= self$max_N) {
         first_valid_period <- period
         message(sprintf("First valid period: %s with %d tickers", period, length(valid_tickers)))
         break
       }
      }
      
      return(first_valid_period)
      },
                                         
    get_investment_universe = function(period) {
      valid_tickers <- self$get_valid_tickers(period)
      
      if (is.character(period)) {
       period_id <- which(rownames(self$returns) == period)
       if (length(period_id) == 0) stop("Provided period not found in rownames.")
      }
      
      if (period_id - self$max_T < 1 || period_id + self$inv_period_length > nrow(self$returns)) {
       stop("Insufficient data for this period.")
      }
      
      hist_idx <- (period_id - self$max_T):period_id
      vol_idx <- (period_id - self$inv_period_length):period_id
      fcast_idx <- (period_id + 1):(period_id + self$inv_period_length)
      
      ordered_tickers <- names(sort(colSums(self$volume[vol_idx, valid_tickers, drop = FALSE]), decreasing = TRUE))
      selection <- head(ordered_tickers, self$max_N)
      
      corr_current <- cor(self$returns[hist_idx, selection], use = "pairwise.complete.obs")
      high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | corr_current == 0), arr.ind = TRUE)
      indices_high_corr <- unique(selection[high_corr[, 2]])
      
      while (nrow(high_corr) > 0) {
       available <- ordered_tickers[!ordered_tickers %in% indices_high_corr]
       selection <- head(available, self$max_N)
       
       corr_current <- cor(self$returns[hist_idx, selection], use = "pairwise.complete.obs")
       high_corr <- which(upper.tri(corr_current, diag = FALSE) & (abs(corr_current) > 0.95 | is.na(corr_current) | corr_current == 0), arr.ind = TRUE)
       indices_high_corr_update <- unique(selection[high_corr[, 2]])
       indices_high_corr <- c(indices_high_corr, indices_high_corr_update)
      }
      
      cat(sprintf("Period: %s — %d stocks selected, %d removed due to high correlation.\n",
                 period, length(selection), length(indices_high_corr)))
      
      return(list(selected = selection, excluded = indices_high_corr))
      },
                                         
    build_universe = function() {
      first_inv_date <- self$find_first_valid_period()
      dates <- rownames(self$returns)
      dates_inv_periods <- dates[(dates >= as.Date(first_inv_date)) & (dates <= as.Date(dates[length(dates) - self$inv_period_length]))]
      inv_dates_string <- dates_inv_periods[seq(1, length(dates_inv_periods), by = self$inv_period_length)]
      
      for (period in inv_dates_string) {
       self$inv_universe_list[[period]] <- self$get_investment_universe(period)$selected
      }
      
      cat("Done.\n")
      return(self$inv_universe_list)
      },
                                         
    save_universe = function(path = "data/inv_universe_list.RData") {
       save(self$inv_universe_list, file = path)
       cat("Saved investment universe to", path, "\n")
      }
    )
)
