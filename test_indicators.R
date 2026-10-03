# test_indicators.R
# Runs the examples from the assignment for every indicator.
# Run from the folder that contains all the .R files:  source("test_indicators.R")

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

cat("\n--- SMA (period = 3) ---\n")
print(sma(data, period = 3))

cat("\n--- EMA (period = 3) ---\n")
print(ema(data, period = 3))

cat("\n--- MACD (3, 5, 2) ---\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130),
           short_period = 3, long_period = 5, signal_period = 2))

cat("\n--- Standard deviation ---\n")
print(stdev(data))

cat("\n--- Linear regression (length = 5, offset = 0) ---\n")
print(linreg(data, regressionLength = 5, regressionOffset = 0))

cat("\n--- RSI (period = 5) ---\n")
print(rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), period = 5))

cat("\n--- StochRSI (period = 14, k = 3, d = 3) on the 10-point example ---\n")
print(stoch_rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62),
                period = 14, k_period = 3, d_period = 3))

cat("\n--- StochRSI (period = 5, k = 3, d = 3) on a longer series ---\n")
long_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 59, 63, 67, 64, 61, 66, 70, 68)
print(stoch_rsi(long_data, period = 5, k_period = 3, d_period = 3))

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

cat("\n--- Crossover ---\n")
print(crossover(arr1, arr2))

cat("\n--- Crossunder ---\n")
print(crossunder(arr1, arr2))

cat("\n--- Crossover / Crossunder on a series that really crosses ---\n")
a <- c(1, 2, 3, 4, 3, 2, 1)
b <- c(2, 2, 2, 2, 2, 2, 2)
print(crossover(a, b))
print(crossunder(a, b))

# ---------- automatic checks (base R functions used ONLY for checking) ----------
cat("\n--- Automatic checks ---\n")
check <- function(name, ok) cat(sprintf("%-45s %s\n", name, if (isTRUE(ok)) "PASS" else "FAIL"))

check("sma matches manual calculation",
      isTRUE(all.equal(sma(data, 3)[1:2], c(37/3, 47/3))))
check("sma length = n - period + 1", length(sma(data, 3)) == 7)
check("sma errors when data is too short",
      inherits(try(sma(c(1, 2), 3), silent = TRUE), "try-error"))

check("ema first value equals first data point", ema(data, 3)[1] == data[1])
check("ema second value (manual)", isTRUE(all.equal(ema(data, 3)[2], 11)))

m <- macd(data, 3, 5, 2)
check("macd histogram = macd line - signal line",
      isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))

check("stdev matches population sd from sd()",
      isTRUE(all.equal(stdev(data), sd(data) * sqrt((length(data) - 1) / length(data)))))

lr <- linreg(data, length(data), 0)
fit <- lm(data ~ seq_along(data))
check("linreg slope matches lm()", isTRUE(all.equal(lr$slope, unname(coef(fit)[2]))))
check("linreg intercept matches lm()", isTRUE(all.equal(lr$intercept, unname(coef(fit)[1]))))
check("linreg errors if length too big",
      inherits(try(linreg(data, 20, 0), silent = TRUE), "try-error"))
check("linreg errors if offset >= length",
      inherits(try(linreg(data, 5, 5), silent = TRUE), "try-error"))

r <- rsi(long_data, 5)
check("rsi values stay between 0 and 100", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
check("rsi first 'period' values are NA", all(is.na(r[1:5])))

s <- stoch_rsi(long_data, 5, 3, 3)
check("stoch_rsi %K between 0 and 1",
      all(s$k_line[!is.na(s$k_line)] >= 0 & s$k_line[!is.na(s$k_line)] <= 1))

check("crossover finds the up cross at point 3", crossover(a, b)[3] == "Up")
check("crossunder finds the down cross at point 7", crossunder(a, b)[7] == "True")
check("crossover errors if lengths differ",
      inherits(try(crossover(1:3, 1:4), silent = TRUE), "try-error"))
