# stoch_rsi.R
# Stochastic RSI (StochRSI) - base R only
# Uses rsi() and sma(), so rsi.R and sma.R must be loaded first.

if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (RSI scaled between its min and max)
  if (all(is.na(rsi_values))) {
    # RSI could not be calculated (data too short)
    k_values <- rep(NA_real_, length(data))
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (SMA of the StochRSI values)
  if (length(k_values) >= k_period) {
    k_line <- sma(k_values, k_period)
  } else {
    k_line <- rep(NA_real_, length(k_values))
  }

  # Calculate the %D line (SMA of the %K line)
  if (length(k_line) >= d_period) {
    d_line <- sma(k_line, d_period)
  } else {
    d_line <- rep(NA_real_, length(k_line))
  }

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
