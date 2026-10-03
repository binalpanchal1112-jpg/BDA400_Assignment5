# linreg.R
# Simple Linear Regression - base R only, no libraries

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting and ending index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Need at least 2 points, otherwise the slope can't be calculated
  if (length(source_subset) < 2) {
    stop("Not enough data points selected to run the regression")
  }

  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}
