# sma.R
# Simple Moving Average (SMA) - base R only, no libraries

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  num_windows <- length(data) - period + 1
  sma_values <- numeric(num_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:num_windows) {
    # Current window of 'period' data points
    current_window <- data[i:(i + period - 1)]

    # Mean of the current window
    mean_value <- sum(current_window) / period

    # Store the mean value in the sma_values vector
    sma_values[i] <- mean_value
  }

  return(sma_values)
}
