# crossunder.R
# Crossunder signals - base R only
# Default output follows the pseudocode: "None" for the first point, then "True"/"False".
# If as_logical = TRUE it returns real TRUE/FALSE values instead
# (TRUE = arr1 crossed under arr2).

crossunder <- function(arr1, arr2, as_logical = FALSE) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)

  # Initialize a vector to store the crossunder signals (first point is "None")
  crossunder_signals <- rep("None", n)

  # Check for crossunder signals at each data point
  if (n >= 2) {
    for (i in 2:n) {
      if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  if (as_logical) {
    return(crossunder_signals == "True")
  }

  return(crossunder_signals)
}
