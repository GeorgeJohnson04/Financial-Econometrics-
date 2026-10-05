## TIME SERIES DECOMPOSITION USING R
## This code performs additive decomposition of financial return series,
## separating the trend into pure linear and cyclical components.
## Analysis compares Financial Select Sector SPDR (XLF) and SPDR S&P Regional Banking ETF (KRE) returns.

## Load required packages for data download
# Install if needed: install.packages(c("quantmod", "zoo"))
library(quantmod)
library(zoo)

## Download Financial Select Sector SPDR Fund (XLF) and SPDR S&P Regional Banking ETF (KRE) data
# XLF tracks the largest US financial-sector companies (banks, insurers, asset managers)
# KRE tracks small/mid-cap US regional banks, a historically cyclical, crisis-sensitive segment
# Adjust start and end dates as needed
start_date <- "2007-01-01"
end_date <- Sys.Date()  # Current date

# Download data - getSymbols downloads to environment
getSymbols("XLF", src = "yahoo", from = start_date, to = end_date)
getSymbols("KRE", src = "yahoo", from = start_date, to = end_date)

########################################
## PLOT ORIGINAL LEVEL SERIES         ##
########################################

## Extract closing prices and dates
xlf_prices <- Cl(XLF)
kre_prices <- Cl(KRE)
xlf_price_dates <- index(xlf_prices)
kre_price_dates <- index(kre_prices)

## Plot XLF and KRE price levels side-by-side
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2))

plot(xlf_price_dates, as.numeric(xlf_prices), type = "l", col = "blue",
     main = "XLF Price Level Series",
     ylab = "XLF Price ($)", xlab = "", xaxt = "n")
axis.Date(1, at = seq(min(xlf_price_dates), max(xlf_price_dates), by = "year"), format = "%Y")
grid()

plot(kre_price_dates, as.numeric(kre_prices), type = "l", col = "brown",
     main = "KRE Price Level Series",
     ylab = "KRE Price ($)", xlab = "Time", xaxt = "n")
axis.Date(1, at = seq(min(kre_price_dates), max(kre_price_dates), by = "year"), format = "%Y")
grid()

## Reset graphics parameters
par(mfrow = c(1, 1))

## Calculate daily log returns (more appropriate for time series analysis)
xlf_returns <- diff(log(Cl(XLF))) * 100  # Convert to percentage
kre_returns <- diff(log(Cl(KRE))) * 100

# Remove the first NA value created by differencing
xlf_returns <- na.omit(xlf_returns)
kre_returns <- na.omit(kre_returns)

## Align both series to common dates (in case of different trading days)
common_dates <- index(xlf_returns)[index(xlf_returns) %in% index(kre_returns)]
xlf_returns_aligned <- xlf_returns[common_dates]
kre_returns_aligned <- kre_returns[common_dates]

## Convert to numeric vectors for decomposition
xlf_data <- as.numeric(xlf_returns_aligned)
kre_data <- as.numeric(kre_returns_aligned)

## Store dates for plotting
all_dates <- as.Date(common_dates)

## Display summary statistics
cat("\n=== XLF (FINANCIALS) RETURNS SUMMARY ===\n")
summary(xlf_data)
cat("\n=== KRE (REGIONAL BANKS) RETURNS SUMMARY ===\n")
summary(kre_data)

#################################
## XLF RETURNS DECOMPOSITION ##
#################################

## Convert to time series object
# Using freq=252 for daily trading data (approximate trading days per year)
xlf.ts <- ts(xlf_data, start=c(2007, 1), frequency=252)

## Perform additive decomposition
# Separates series into trend, seasonal, and random components
dec.xlf <- decompose(xlf.ts, type="additive")

## Display R's default decomposition plot (combines trend + cycle)
plot(dec.xlf)

## Convert decomposition results to data frame for further analysis
dec.xlf.df <- data.frame(
  observed = dec.xlf$x,
  trend = dec.xlf$trend,
  seasonal = dec.xlf$seasonal,
  random = dec.xlf$random
)

## Remove rows with NA values (from edges of decomposition)
dec.xlf.clean <- dec.xlf.df[complete.cases(dec.xlf.df), ]

## Create time index variable (t = 1, 2, 3, ...)
dec.xlf.clean$time <- seq_len(nrow(dec.xlf.clean))

## Extract corresponding dates for the cleaned decomposition data
# Find which rows had complete cases
complete_rows <- which(complete.cases(dec.xlf.df))
xlf_dates <- all_dates[complete_rows]
dec.xlf.clean$date <- xlf_dates

## Separate trend into LINEAR TREND and CYCLICAL components
# Regress the trend-cycle component on time to extract pure linear trend
# Residuals represent cyclical deviations from the linear trend
xlf.lm <- lm(trend ~ time, data = dec.xlf.clean)

# Extract components
xlf.linear_trend <- predict(xlf.lm)
xlf.cyclical <- resid(xlf.lm)

## Plot XLF linear trend
plot(
  xlf_dates,
  xlf.linear_trend,
  type = "l",
  lty = 1,
  xlab = "Time",
  ylab = "Linear Trend (%)",
  main = "XLF Returns - Pure Linear Trend Component",
  col = "blue",
  xaxt = "n"
)
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

## Plot XLF cyclical component
plot(
  xlf_dates,
  xlf.cyclical,
  type = "l",
  lty = 1,
  xlab = "Time",
  ylab = "Cyclical Component (%)",
  main = "XLF Returns - Cyclical Component",
  col = "darkgreen",
  xaxt = "n"
)
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

## Create comprehensive 4-panel decomposition plot for XLF
options(scipen = 999)  # Disable scientific notation

## Calculate common y-axis limits for bottom three components
# Find the maximum absolute value across cyclical, seasonal, and random components
xlf.max_range <- max(
  abs(range(xlf.cyclical, na.rm = TRUE)),
  abs(range(dec.xlf.clean$seasonal, na.rm = TRUE)),
  abs(range(dec.xlf.clean$random, na.rm = TRUE))
)
# Set symmetric limits for better visual comparison
xlf.common_ylim <- c(-xlf.max_range, xlf.max_range)

par(mar = c(3, 5, 2, 2), mfrow = c(4, 1))

plot(xlf_dates, xlf.linear_trend, type = "l",
     main = "XLF Returns - Additive Decomposition Components",
     xlab = "", ylab = "Linear Trend (%)", xaxt = "n")
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(xlf_dates, xlf.cyclical, type = "l",
     main = "", xlab = "", ylab = "Cyclical (%)",
     ylim = xlf.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(xlf_dates, dec.xlf.clean$seasonal, type = "l",
     main = "", xlab = "", ylab = "Seasonal (%)",
     ylim = xlf.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(xlf_dates, dec.xlf.clean$random, type = "l",
     main = "", xlab = "Time", ylab = "Random (%)",
     ylim = xlf.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

############################################
## KRE RETURNS DECOMPOSITION ##
############################################

## Convert to time series object
kre.ts <- ts(kre_data, start=c(2007, 1), frequency=252)

## Perform additive decomposition
dec.kre <- decompose(kre.ts, type="additive")

## Display R's default decomposition plot
par(mfrow = c(1, 1))
plot(dec.kre)

## Convert decomposition results to data frame
dec.kre.df <- data.frame(
  observed = dec.kre$x,
  trend = dec.kre$trend,
  seasonal = dec.kre$seasonal,
  random = dec.kre$random
)

## Remove rows with NA values
dec.kre.clean <- dec.kre.df[complete.cases(dec.kre.df), ]

## Create time index variable
dec.kre.clean$time <- seq_len(nrow(dec.kre.clean))

## Extract corresponding dates for the cleaned decomposition data
complete_rows_kre <- which(complete.cases(dec.kre.df))
kre_dates <- all_dates[complete_rows_kre]
dec.kre.clean$date <- kre_dates

## Separate trend into LINEAR TREND and CYCLICAL components
kre.lm <- lm(trend ~ time, data = dec.kre.clean)

# Extract components
kre.linear_trend <- predict(kre.lm)
kre.cyclical <- resid(kre.lm)

## Plot KRE linear trend
plot(
  kre_dates,
  kre.linear_trend,
  type = "l",
  lty = 1,
  xlab = "Time",
  ylab = "Linear Trend (%)",
  main = "KRE Returns - Pure Linear Trend Component",
  col = "blue",
  xaxt = "n"
)
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

## Plot KRE cyclical component
plot(
  kre_dates,
  kre.cyclical,
  type = "l",
  lty = 1,
  xlab = "Time",
  ylab = "Cyclical Component (%)",
  main = "KRE Returns - Cyclical Component",
  col = "darkgreen",
  xaxt = "n"
)
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

## Create comprehensive 4-panel decomposition plot for KRE
## Calculate common y-axis limits for bottom three components
kre.max_range <- max(
  abs(range(kre.cyclical, na.rm = TRUE)),
  abs(range(dec.kre.clean$seasonal, na.rm = TRUE)),
  abs(range(dec.kre.clean$random, na.rm = TRUE))
)
# Set symmetric limits for better visual comparison
kre.common_ylim <- c(-kre.max_range, kre.max_range)

par(mar = c(3, 5, 2, 2), mfrow = c(4, 1))

plot(kre_dates, kre.linear_trend, type = "l",
     main = "KRE Returns - Additive Decomposition Components",
     xlab = "", ylab = "Linear Trend (%)", xaxt = "n")
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(kre_dates, kre.cyclical, type = "l",
     main = "", xlab = "", ylab = "Cyclical (%)",
     ylim = kre.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(kre_dates, dec.kre.clean$seasonal, type = "l",
     main = "", xlab = "", ylab = "Seasonal (%)",
     ylim = kre.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

plot(kre_dates, dec.kre.clean$random, type = "l",
     main = "", xlab = "Time", ylab = "Random (%)",
     ylim = kre.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2, col = "gray")

#################################################
## COMPARATIVE NOISE COMPONENT ANALYSIS        ##
#################################################

## Compare XLF and KRE returns side-by-side
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2))

plot(all_dates, xlf_data, type = "l", col = "blue",
     main = "Daily Returns Comparison: Financials vs Regional Bankss",
     ylab = "XLF Returns (%)", xlab = "", xaxt = "n")
axis.Date(1, at = seq(min(all_dates), max(all_dates), by = "year"), format = "%Y")

plot(all_dates, kre_data, type = "l", col = "brown",
     ylab = "KRE Returns (%)", xlab = "Time", xaxt = "n")
axis.Date(1, at = seq(min(all_dates), max(all_dates), by = "year"), format = "%Y")

## Compare noise (random) components with common y-axis scale
## Calculate common y-axis limits for noise components
noise.max_range <- max(
  abs(range(dec.xlf.clean$random, na.rm = TRUE)),
  abs(range(dec.kre.clean$random, na.rm = TRUE))
)
# Set symmetric limits for better visual comparison
noise.common_ylim <- c(-noise.max_range, noise.max_range)

par(mfrow = c(2, 1), mar = c(3, 5, 2, 2))

plot(xlf_dates, dec.xlf.clean$random, type = "l", col = "blue",
     main = "Noise Components Comparison: Financials vs Regional Bankss",
     ylab = "XLF Noise (%)", xlab = "", 
     ylim = noise.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(xlf_dates), max(xlf_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2)

plot(kre_dates, dec.kre.clean$random, type = "l", col = "brown",
     ylab = "KRE Noise (%)", xlab = "Time", 
     ylim = noise.common_ylim, xaxt = "n")
axis.Date(1, at = seq(min(kre_dates), max(kre_dates), by = "year"), format = "%Y")
abline(h = 0, lty = 2)

## Reset graphics parameters
par(mfrow = c(1, 1))

## END