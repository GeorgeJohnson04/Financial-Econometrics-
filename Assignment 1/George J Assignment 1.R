# Assignment 1 George Johnson
# Claude code utilized for debugging

library(quantmod)
library(tseries)


# Download Daily Prices 
getSymbols(c("XLF", "KRE"), src = "yahoo", from = "2007-01-01", to = Sys.Date())


# Convert Prices to Daily Log
xlf_returns <- as.numeric(na.omit(diff(log(Cl(XLF))) * 100))
kre_returns <- as.numeric(na.omit(diff(log(Cl(KRE))) * 100))

cat("\nXLF daily returns (%):\n")
print(summary(xlf_returns))

cat("\nKRE daily returns (%):\n")
print(summary(kre_returns))


# Run ADF Test if test p val <.01 it returns a warning
adf_xlf <- adf.test(xlf_returns, alternative = "stationary")
adf_kre <- adf.test(kre_returns, alternative = "stationary")

cat("\n----- ADF test: XLF returns -----\n")
print(adf_xlf)

cat("\n----- ADF test: KRE returns -----\n")
print(adf_kre)


# Compares results (side by side view)
results <- data.frame(
  Series     = c("XLF returns", "KRE returns"),
  ADF_stat   = round(c(adf_xlf$statistic, adf_kre$statistic), 3),
  Lags       = c(adf_xlf$parameter, adf_kre$parameter),
  p_value    = c(adf_xlf$p.value, adf_kre$p.value)
)
results$Stationary <- ifelse(results$p_value < 0.05, "Yes", "No")

cat("\n----- Summary -----\n")
print(results)
