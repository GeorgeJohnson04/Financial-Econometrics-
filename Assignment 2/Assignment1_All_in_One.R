# Assignment 1 - Financial Econometrics

install.packages(c("quantmod", "tseries"))


library(quantmod)   # downloads prices from Yahoo Finance
library(tseries)    # contains adf.test()



out_dir <- "Assignment1_outputs"
dir.create(out_dir, showWarnings = FALSE)

# Draws a chart twice: once into a PNG file (for the report) and once in
# the Plots pane. If the Plots pane is too small, the PNG is still saved.
make_chart <- function(file_name, draw, height = 6) {
  png(file.path(out_dir, file_name), width = 10, height = height, units = "in", res = 150)
  draw()
  dev.off()
  try(draw(), silent = TRUE)
}

# Puts a tick on January 1 of each year on the x-axis of a date chart
year_axis <- function(dates) {
  first_jan <- as.Date(paste0(format(min(dates), "%Y"), "-01-01"))
  axis.Date(1, at = seq(first_jan, max(dates), by = "year"), format = "%Y")
}

# Basic summary statistics for a series
describe <- function(x) {
  round(c(Mean = mean(x), SD = sd(x), Min = min(x), Median = median(x),
          Max = max(x), N = length(x)), 3)
}


# ---------------------------------------------------------------------
# Data: download prices and calculate daily returns
# ---------------------------------------------------------------------

getSymbols(c("XLF", "KRE"), src = "yahoo", from = "2007-01-01", to = Sys.Date())

xlf_prices <- Cl(XLF)   # daily closing prices
kre_prices <- Cl(KRE)

# Daily log returns in %, kept only on days where both ETFs traded
returns      <- na.omit(merge(diff(log(xlf_prices)), diff(log(kre_prices))) * 100)
return_dates <- index(returns)
xlf_returns  <- as.numeric(returns[, 1])
kre_returns  <- as.numeric(returns[, 2])

cat("\n===== PRICE LEVELS ($) =====\n")
print(rbind(XLF = describe(as.numeric(xlf_prices)),
            KRE = describe(as.numeric(kre_prices))))

cat("\n===== DAILY LOG RETURNS (%) =====\n")
print(rbind(XLF = describe(xlf_returns),
            KRE = describe(kre_returns)))


# ---------------------------------------------------------------------
# Part 1: Price level charts
# ---------------------------------------------------------------------

make_chart("01_XLF_price.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  plot(index(xlf_prices), as.numeric(xlf_prices), type = "l", col = "blue",
       main = "XLF Daily Closing Price", xlab = "", ylab = "Price ($)", xaxt = "n")
  year_axis(index(xlf_prices))
  grid()
})

make_chart("02_KRE_price.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  plot(index(kre_prices), as.numeric(kre_prices), type = "l", col = "brown",
       main = "KRE Daily Closing Price", xlab = "", ylab = "Price ($)", xaxt = "n")
  year_axis(index(kre_prices))
  grid()
})


# ---------------------------------------------------------------------
# Part 2: Decomposition (four-panel chart for each ETF)
# ---------------------------------------------------------------------

# Decomposes a return series and draws:
#   linear trend | cyclical | seasonal | noise
plot_decomposition <- function(x, dates, name) {

  # R's additive decomposition, treating 252 trading days as one year
  dec <- decompose(ts(x, start = c(2007, 1), frequency = 252), type = "additive")

  # decompose() leaves blanks (NA) at both ends - drop those days
  keep     <- complete.cases(dec$trend, dec$seasonal, dec$random)
  dates    <- dates[keep]
  trend    <- as.numeric(dec$trend)[keep]
  seasonal <- as.numeric(dec$seasonal)[keep]
  noise    <- as.numeric(dec$random)[keep]

  # R's "trend" is really trend + cycle. Fit a straight line through it:
  #   the straight line  = linear trend
  #   what's left over   = cyclical component
  time         <- seq_along(trend)
  line_fit     <- lm(trend ~ time)
  linear_trend <- fitted(line_fit)
  cyclical     <- resid(line_fit)

  # Same y-axis for the bottom three panels so their sizes can be compared
  lim <- max(abs(c(cyclical, seasonal, noise)))

  par(mfrow = c(4, 1), mar = c(3, 5, 2, 2))

  plot(dates, linear_trend, type = "l", xaxt = "n", xlab = "", ylab = "Linear Trend (%)",
       main = paste(name, "Returns - Additive Decomposition Components"))
  year_axis(dates)
  abline(h = 0, lty = 2, col = "gray")

  plot(dates, cyclical, type = "l", xaxt = "n", xlab = "", ylab = "Cyclical (%)",
       ylim = c(-lim, lim))
  year_axis(dates)
  abline(h = 0, lty = 2, col = "gray")

  plot(dates, seasonal, type = "l", xaxt = "n", xlab = "", ylab = "Seasonal (%)",
       ylim = c(-lim, lim))
  year_axis(dates)
  abline(h = 0, lty = 2, col = "gray")

  plot(dates, noise, type = "l", xaxt = "n", xlab = "Time", ylab = "Noise (%)",
       ylim = c(-lim, lim))
  year_axis(dates)
  abline(h = 0, lty = 2, col = "gray")
}

make_chart("03_XLF_decomposition.png", height = 9, function() {
  plot_decomposition(xlf_returns, return_dates, "XLF")
})

make_chart("04_KRE_decomposition.png", height = 9, function() {
  plot_decomposition(kre_returns, return_dates, "KRE")
})


# ---------------------------------------------------------------------
# Part 3: Daily return charts
# ---------------------------------------------------------------------

# Both charts use the same y-axis so their volatility can be compared by eye
return_lim <- range(c(xlf_returns, kre_returns))

make_chart("05_XLF_returns.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  plot(return_dates, xlf_returns, type = "l", col = "blue", ylim = return_lim,
       main = "XLF Daily Log Returns", xlab = "", ylab = "Return (%)", xaxt = "n")
  year_axis(return_dates)
  abline(h = 0, lty = 2, col = "gray")
})

make_chart("06_KRE_returns.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  plot(return_dates, kre_returns, type = "l", col = "brown", ylim = return_lim,
       main = "KRE Daily Log Returns", xlab = "", ylab = "Return (%)", xaxt = "n")
  year_axis(return_dates)
  abline(h = 0, lty = 2, col = "gray")
})


# ---------------------------------------------------------------------
# Part 4: ADF stationarity test
#   H0: the series has a unit root (NOT stationary)
#   H1: the series is stationary
#   p-value < 0.05 -> reject H0 -> stationary
# Note: adf.test() can't show p-values below 0.01. The warning
# "p-value smaller than printed p-value" means the real p-value is under 0.01.
# ---------------------------------------------------------------------

adf_xlf <- adf.test(xlf_returns, alternative = "stationary")
adf_kre <- adf.test(kre_returns, alternative = "stationary")

adf_table <- data.frame(
  Series   = c("XLF returns", "KRE returns"),
  ADF_stat = round(c(adf_xlf$statistic, adf_kre$statistic), 3),
  Lags     = c(adf_xlf$parameter, adf_kre$parameter),
  p_value  = c(adf_xlf$p.value, adf_kre$p.value)
)
adf_table$Stationary <- ifelse(adf_table$p_value < 0.05, "Yes", "No")

cat("\n===== ADF TEST: XLF RETURNS =====\n")
print(adf_xlf)
cat("\n===== ADF TEST: KRE RETURNS =====\n")
print(adf_kre)
cat("\n===== ADF SUMMARY =====\n")
print(adf_table)


# ---------------------------------------------------------------------
# Part 5: Ljung-Box white noise test (30 lags for daily data)
#   H0: the series is white noise (no autocorrelation)
#   H1: the series has autocorrelation
#   p-value < 0.05 -> reject H0 -> NOT white noise
# ---------------------------------------------------------------------

lb_xlf <- Box.test(xlf_returns, lag = 30, type = "Ljung-Box")
lb_kre <- Box.test(kre_returns, lag = 30, type = "Ljung-Box")

lb_p <- c(lb_xlf$p.value, lb_kre$p.value)

lb_table <- data.frame(
  Series      = c("XLF returns", "KRE returns"),
  Chi_squared = round(c(lb_xlf$statistic, lb_kre$statistic), 3),
  DF          = c(lb_xlf$parameter, lb_kre$parameter),
  p_value     = format.pval(lb_p, digits = 4),   # very small p-values show as "< 2.2e-16"
  White_Noise = ifelse(lb_p >= 0.05, "Yes", "No")
)

cat("\n===== LJUNG-BOX TEST: XLF RETURNS =====\n")
print(lb_xlf)
cat("\n===== LJUNG-BOX TEST: KRE RETURNS =====\n")
print(lb_kre)
cat("\n===== LJUNG-BOX SUMMARY =====\n")
print(lb_table)


# ---------------------------------------------------------------------
# Part 6: ACF charts (40 lags)
# Blue dashed lines = 95% confidence band. Spikes outside the band are
# significant autocorrelation. White noise has no spikes past lag 0.
# ---------------------------------------------------------------------

make_chart("07_XLF_ACF.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  acf(xlf_returns, lag.max = 40, col = "blue",
      main = "XLF Returns - Autocorrelation Function")
})

make_chart("08_KRE_ACF.png", function() {
  par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))
  acf(kre_returns, lag.max = 40, col = "brown",
      main = "KRE Returns - Autocorrelation Function")
})


# ---------------------------------------------------------------------
# Save the test results to a text file and show where everything went
# ---------------------------------------------------------------------

capture.output(
  cat("ADF TEST - XLF RETURNS\n"),      print(adf_xlf),
  cat("\nADF TEST - KRE RETURNS\n"),    print(adf_kre),
  cat("\nADF SUMMARY\n"),               print(adf_table),
  cat("\n\nLJUNG-BOX TEST - XLF RETURNS\n"), print(lb_xlf),
  cat("\nLJUNG-BOX TEST - KRE RETURNS\n"),   print(lb_kre),
  cat("\nLJUNG-BOX SUMMARY\n"),         print(lb_table),
  file = file.path(out_dir, "test_results.txt")
)

cat("\nDone. Charts and test results saved in:\n", normalizePath(out_dir), "\n")
