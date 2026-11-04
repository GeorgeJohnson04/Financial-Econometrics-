# Load required packages
library(vars)
library(quantmod)

# Get stock price data for S&P500 and Nasdaq
getSymbols("^GSPC")
getSymbols("^IXIC")

# Extract the adjusted close prices
sp500_prices <- Ad(GSPC)
nasdaq_prices <- Ad(IXIC)

# Calculate monthly returns for S&P500 and Nasdaq
sp500_returns <- diff(log(sp500_prices))
nasdaq_returns <- diff(log(nasdaq_prices))

# Select the last 60 monthly returns for S&P500 and Nasdaq
series1 <- tail(sp500_returns, 60)
series2 <- tail(nasdaq_returns, 60)

# Create a data frame with the time series variables
data <- data.frame(series1, series2)

colnames(data) <- c('series1', 'series2')

# Fit a VAR model with one lag of each variable
var_model <- VAR(data, p = 1, type = "const")

# Display the VAR model results
summary(var_model)

# Extract the residuals and fitted values
fitted_values <- fitted(var_model)
residuals_series1 <- residuals(var_model)[, 1, drop = FALSE]

# Adjust the lengths of series1 and residuals_series1
n <- min(length(series1), length(residuals_series1))
series1 <- series1[length(series1) - n + 1:length(residuals_series1)]
series2 <- series2[length(series2) - n + 1:length(residuals_series1)]

# Calculate the fitted values
fitted_series1 <- as.numeric(series1) - residuals_series1[1:n, 1]
fitted_series2 <- as.numeric(series2) - residuals_series1[1:n, 1]

# Perform 6-period forecasts for each series
forecast_series1 <- predict(var_model, n.ahead = 6)$fcst$series1[, "fcst"]
lower_ci_series1 <- predict(var_model, n.ahead = 6)$fcst$series1[, "lower"]
upper_ci_series1 <- predict(var_model, n.ahead = 6)$fcst$series1[, "upper"]

forecast_series2 <- predict(var_model, n.ahead = 6)$fcst$series2[, "fcst"]
lower_ci_series2 <- predict(var_model, n.ahead = 6)$fcst$series2[, "lower"]
upper_ci_series2 <- predict(var_model, n.ahead = 6)$fcst$series2[, "upper"]

# Plot actual vs fitted for series1
plot(as.numeric(series1), type = "l", col = "blue", ylim = range(c(as.numeric(series1), fitted_series1)),
     main = "Actual vs Fitted: series1")
lines(fitted_series1, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1)

# Plot actual vs fitted for series2
plot(as.numeric(series2), type = "l", col = "blue", ylim = range(c(as.numeric(series2), fitted_series2)),
     main = "Actual vs Fitted: series2")
lines(fitted_series2, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1)

var_model

# Plot IRF for series1 on series2
irf_series1_on_series2 <- irf(var_model, impulse = "series2", response = "series1", ortho = FALSE, n.ahead = 6)
plot(irf_series1_on_series2)
irf_series2_on_series1 <- irf(var_model, impulse = "series1", response = "series2", ortho = FALSE, n.ahead = 6)
plot(irf_series2_on_series1)

# Plot actual and 6-period forecast with confidence interval for series1
time <- 61:66
plot(as.numeric(series1), type = "l", col = "blue", ylim = range(c(as.numeric(series1), forecast_series1, lower_ci_series1, upper_ci_series1)),
     main = "Actual and Forecast: series1", xlab = "Time", ylab = "Series1", xlim = c(1, max(time)))
lines(time, forecast_series1, col = "green")
lines(time, lower_ci_series1, col = "red", lty = 2)
lines(time, upper_ci_series1, col = "red", lty = 2)
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("blue", "green", "red"), lty = c(1, 1, 2))

# Plot actual and 6-period forecast with confidence interval for series2
plot(as.numeric(series2), type = "l", col = "blue", ylim = range(c(as.numeric(series2), forecast_series2, lower_ci_series2, upper_ci_series2)),
     main = "Actual and Forecast: series2", xlab = "Time", ylab = "Series2", xlim = c(1, max(time)))
lines(time, forecast_series2, col = "green")
lines(time, lower_ci_series2, col = "red", lty = 2)
lines(time, upper_ci_series2, col = "red", lty = 2)
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("blue", "green", "red"), lty = c(1, 1, 2))

# Diagnostic plots
par(mfrow = c(2, 2))

# Plot the autocorrelation function (ACF) of residuals
acf(residuals_series1, main = "ACF of Residuals: series1")
acf(residuals(var_model)[, 2], main = "ACF of Residuals: series2")

# Plot the partial autocorrelation function (PACF) of residuals
pacf(residuals_series1, main = "PACF of Residuals: series1")
pacf(residuals(var_model)[, 2], main = "PACF of Residuals: series2")

# Plot the histogram of residuals
par(mfrow = c(1, 2))
hist(residuals_series1, main = "Histogram of Residuals: series1", xlab = "Residuals")
hist(residuals(var_model)[, 2], main = "Histogram of Residuals: series2", xlab = "Residuals")

# Plot the QQ plot of residuals
par(mfrow = c(1, 2))
qqnorm(residuals_series1, main = "Normal QQ Plot: series1")
qqline(residuals_series1)
qqnorm(residuals(var_model)[, 2], main = "Normal QQ Plot: series2")
qqline(residuals(var_model)[, 2])

# Variance decomposition
var_decomp <- fevd(var_model, n.ahead = 6)
plot(var_decomp)

# Return the modified code
list(fitted_values, forecast_series1, lower_ci_series1, upper_ci_series1, forecast_series2, lower_ci_series2, upper_ci_series2)
