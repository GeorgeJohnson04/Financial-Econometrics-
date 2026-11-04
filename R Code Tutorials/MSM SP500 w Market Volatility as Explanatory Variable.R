library(quantmod)
library(MSwM)
library(ggplot2)

# Get S&P 500 returns (monthly)
getSymbols("^GSPC", from = "2000-01-01", to = "2023-01-01")
GSPC_monthly <- to.monthly(GSPC)[, "GSPC.Close"]
sp500.ret <- diff(log(GSPC_monthly))

# Calculate realized volatility (monthly)
realized_volatility <- volatility(GSPC_monthly, calc = "close")

# Convert to data frames
sp500.df <- data.frame(date = index(sp500.ret), coredata(sp500.ret))
realized_volatility.df <- data.frame(date = index(realized_volatility), coredata(realized_volatility))

# Merge the data based on dates
data <- merge(sp500.df, realized_volatility.df, by = "date")

# Remove the "date" column
data$date <- NULL

# Remove rows with missing values
data <- na.omit(data)

# Fit the regime-switching model
mod <- msmFit(sp500.ret ~ realized_volatility, data = data, k = 2, sw = c(TRUE, TRUE, TRUE), p = 0)
plot(sp500.ret)
plot(realized_volatility)

# OLS regression for sp500.ret on the explanatory variable
lm_model <- lm(sp500.ret ~ realized_volatility, data = data)
summary(lm_model)

# Fit the regime-switching model
mod <- msmFit(lm_model, k = 2, sw = rep(TRUE, 3))

# Plot the model
plot(mod)

# Plot smoothed probabilities
plot(mod, type = "smoothed")

# Plot probabilities
par(mar = c(3, 3, 3, 3))
plotProb(mod, which = 1)
plotProb(mod, which = 2)

# Plot diagnostics
par(mar = c(3, 3, 3, 3))
plotDiag(mod, regime = 1, which = 1)
plotDiag(mod, regime = 1, which = 2)
plotDiag(mod, regime = 1, which = 3)

# Summary of the model
summary(mod)

