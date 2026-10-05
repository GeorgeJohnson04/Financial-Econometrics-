## AUGMENTED DICKEY-FULLER (ADF) TEST FOR STATIONARITY
## This program performs the ADF unit root test to assess whether a time series
## is stationary or contains a unit root (non-stationary).
## Analysis compares Financial Select Sector SPDR (XLF) and SPDR S&P Regional Banking ETF (KRE) returns.

## Load required packages
# Install if needed: install.packages(c("tseries", "quantmod", "zoo"))
library(tseries)   # For ADF test
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
# Returns are typically stationary, while price levels are usually non-stationary
xlf_returns <- diff(log(Cl(XLF))) * 100  # Convert to percentage
kre_returns <- diff(log(Cl(KRE))) * 100

# Remove the first NA value created by differencing
xlf_returns <- na.omit(xlf_returns)
kre_returns <- na.omit(kre_returns)

## Convert to numeric vectors for ADF testing
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
## ADF TEST FOR XLF RETURNS           ##
########################################

cat("\n" , rep("=", 60), "\n", sep="")
cat("AUGMENTED DICKEY-FULLER TEST: XLF RETURNS (k=0)\n")
cat(rep("=", 60), "\n", sep="")

## Perform ADF test with k=0 (no lagged difference terms)
# Null Hypothesis (H0): Series has a unit root (non-stationary)
# Alternative Hypothesis (H1): Series is stationary
# If p-value < 0.05, reject H0 and conclude series is stationary
adf_xlf <- adf.test(xlf_data, k=0)
print(adf_xlf)


########################################
## ADF TEST FOR KRE RETURNS           ##
########################################

cat("\n" , rep("=", 60), "\n", sep="")
cat("AUGMENTED DICKEY-FULLER TEST: KRE RETURNS (k=0)\n")
cat(rep("=", 60), "\n", sep="")

## Perform ADF test with k=0 (no lagged difference terms)
adf_kre <- adf.test(kre_data, k=0)
print(adf_kre)


