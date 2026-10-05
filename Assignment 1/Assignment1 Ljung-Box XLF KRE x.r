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
## LJUNG-BOX TEST FOR XLF RETURNS     ##
########################################

cat("\n" , rep("=", 60), "\n", sep="")
cat("LJUNG-BOX WHITE NOISE TEST: XLF RETURNS\n")
cat(rep("=", 60), "\n", sep="")

## Perform Ljung-Box test with lag=30 (appropriate for daily data)
# Null Hypothesis (H0): The data are white noise (no autocorrelation)
# Alternative Hypothesis (H1): The data exhibit autocorrelation (not white noise)
# If p-value < 0.05, reject H0 and conclude series is NOT white noise
# Use lag=30 for daily series, lag=6 for monthly series
lb_xlf <- Box.test(xlf_data, type="Ljung-Box", lag=30)
print(lb_xlf)

## Interpret results
if(lb_xlf$p.value < 0.01) {
  cat("\nInterpretation: Strong evidence that series is NOT white noise (p < 0.01)\n")
  cat("The series exhibits significant autocorrelation and may be predictable\n")
} else if(lb_xlf$p.value < 0.05) {
  cat("\nInterpretation: Evidence that series is NOT white noise (p < 0.05)\n")
  cat("The series exhibits autocorrelation and may be predictable\n")
} else if(lb_xlf$p.value < 0.10) {
  cat("\nInterpretation: Weak evidence that series is NOT white noise (p < 0.10)\n")
  cat("The series may exhibit some autocorrelation\n")
} else {
  cat("\nInterpretation: Insufficient evidence to reject white noise hypothesis\n")
  cat("The series appears to be white noise (unpredictable/random)\n")
}

########################################
## LJUNG-BOX TEST FOR KRE RETURNS     ##
########################################

cat("\n" , rep("=", 60), "\n", sep="")
cat("LJUNG-BOX WHITE NOISE TEST: KRE RETURNS\n")
cat(rep("=", 60), "\n", sep="")

## Perform Ljung-Box test with lag=30 (appropriate for daily data)
lb_kre <- Box.test(kre_data, type="Ljung-Box", lag=30)
print(lb_kre)

## Interpret results
if(lb_kre$p.value < 0.01) {
  cat("\nInterpretation: Strong evidence that series is NOT white noise (p < 0.01)\n")
  cat("The series exhibits significant autocorrelation and may be predictable\n")
} else if(lb_kre$p.value < 0.05) {
  cat("\nInterpretation: Evidence that series is NOT white noise (p < 0.05)\n")
  cat("The series exhibits autocorrelation and may be predictable\n")
} else if(lb_kre$p.value < 0.10) {
  cat("\nInterpretation: Weak evidence that series is NOT white noise (p < 0.10)\n")
  cat("The series may exhibit some autocorrelation\n")
} else {
  cat("\nInterpretation: Insufficient evidence to reject white noise hypothesis\n")
  cat("The series appears to be white noise (unpredictable/random)\n")
}

########################################
## COMPARATIVE SUMMARY                ##
########################################

cat("\n" , rep("=", 60), "\n", sep="")
cat("COMPARATIVE SUMMARY\n")
cat(rep("=", 60), "\n", sep="")

## Create summary table
summary_table <- data.frame(
  Series = c("XLF Returns", "KRE Returns"),
  Chi_Squared = c(lb_xlf$statistic, lb_kre$statistic),
  DF = c(lb_xlf$parameter, lb_kre$parameter),
  P_Value = c(lb_xlf$p.value, lb_kre$p.value),
  White_Noise = c(
    ifelse(lb_xlf$p.value >= 0.05, "Yes", "No"),
    ifelse(lb_kre$p.value >= 0.05, "Yes", "No")
  )
)

print(summary_table)

cat("\nNote: 'White_Noise = No' indicates p-value < 0.05 (reject white noise hypothesis)\n")
cat("'White_Noise = No' means the series exhibits autocorrelation and may be predictable\n")
cat("All tests performed with lag=30 (appropriate for daily data)\n")

