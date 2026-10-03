# crossover.R
# Crossover signals - base R only
# Default output follows the pseudocode: "Up", "Down" or "None" for each point.
# If as_logical = TRUE it returns TRUE/FALSE instead (TRUE = arr1 crossed over arr2).

crossover <- function(arr1, arr2, as_logical = FALSE) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)

  # Initialize a vector to store the crossover signals (first point is always "None")
  crossover_signals <- rep("None", n)

  # Check for crossovers at each data point
  if (n >= 2) {
    for (i in 2:n) {
      # isTRUE() makes sure NA values just count as "no signal"
      if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
        crossover_signals[i] <- "Up"
      } else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  if (as_logical) {
    return(crossover_signals == "Up")
  }

  return(crossover_signals)
}
