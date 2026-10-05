## LJUNG-BOX WHITE NOISE TEST
## This program performs the Ljung-Box test to determine whether a time series
## exhibits white noise behavior (no autocorrelation) or contains predictable patterns.
## Analysis compares Financial Select Sector SPDR (XLF) and SPDR S&P Regional Banking ETF (KRE) returns.

## Load required packages
# Install if needed: install.packages(c("tseries", "quantmod", "zoo"))
library(tseries)   # For Ljung-Box test
library(quantmod)  # For downloading financial data
library(zoo)       # For time series manipulation

## Download Financial Select Sector SPDR Fund (XLF) and SPDR S&P Regional Banking ETF (KRE) data
# XLF tracks the largest US financial-sector companies (banks, insurers, asset managers)
# KRE tracks small/mid-cap US regional banks, a historically cyclical, crisis-sensitive segment
start_date <- "2007-01-01"
end_date <- Sys.Date()  # Current date

# Download data - getSymbols downloads to environment
cat("\nDownloading data...\n")
getSymbols("XLF", src = "yahoo", from = start_date, to = end_date)
getSymbols("KRE", src = "yahoo", from = start_date, to = end_date)

## Calculate daily log returns (more appropriate for time series analysis)
xlf_returns <- diff(log(Cl(XLF))) * 100  # Convert to percentage
kre_returns <- diff(log(Cl(KRE))) * 100

# Remove the first NA value created by differencing
xlf_returns <- na.omit(xlf_returns)
kre_returns <- na.omit(kre_returns)

## Convert to numeric vectors for white noise testing
xlf_data <- as.numeric(xlf_returns)
kre_data <- as.numeric(kre_returns)

## Display basic descriptive statistics
cat("\n=== XLF (FINANCIALS) RETURNS SUMMARY ===\n")
print(summary(xlf_data))
cat("\nNumber of observations:", length(xlf_data), "\n")

cat("\n=== KRE (REGIONAL BANKS) RETURNS SUMMARY ===\n")
print(summary(kre_data))
cat("\nNumber of observations:", length(kre_data), "\n")

########################################
## AUTOCORRELATION FUNCTION PLOTS     ##
########################################

## Plot ACF to visually inspect autocorrelation structure
# White noise should show no significant autocorrelation beyond lag 0
par(mfrow = c(2, 1), mar = c(4, 5, 3, 2))

acf(xlf_data, main = "XLF Returns - Autocorrelation Function", 
    col = "blue", lag.max = 40)

acf(kre_data, main = "KRE Returns - Autocorrelation Function", 
    col = "brown", lag.max = 40)

## Reset graphics parameters
par(mfrow = c(1, 1))

cat("\nBlue dashed lines show 95% confidence intervals.\n")
cat("Spikes outside these lines indicate significant autocorrelation at those lags.\n")
cat("White noise should have no significant spikes beyond lag 0.\n")

## END