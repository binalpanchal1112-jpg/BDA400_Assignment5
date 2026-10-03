# ema.R
# Exponential Moving Average (EMA) - base R only, no libraries

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty vector to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data vector
  for (i in seq_along(data)) {
    if (i == 1) {
      # EMA for the first data point is just the first value
      ema_values[i] <- data[i]
    } else {
      # EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
