# rsi.R
# Relative Strength Index (RSI) using Wilder's smoothing - base R only

rsi <- function(data, period) {
  n <- length(data)

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  # Not enough data to calculate anything, so return all NA
  if (n <= period) {
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Average gain and average loss for the first 'period' data points
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      # No losses at all, so RSI is 100 (avoids dividing by zero)
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
