# Load required libraries
library(urca)
library(vars)
library(tseries)
library(dynlm)
library(quantmod)

# Retrieve 10-Year Treasury Yield data from FRED
getSymbols("GS10", src = "FRED", from = "1990-01-01", to = Sys.Date())

# Retrieve 30-Year Mortgage Rate data from FRED
getSymbols("MORTGAGE30US", src = "FRED", from = "1990-01-01", to = Sys.Date())

# Extract the 10-Year Treasury Yield values
treasury10_values <- GS10[, "GS10"]

# Extract the 30-Year Mortgage Rate values
mortgage30_values <- MORTGAGE30US[, "MORTGAGE30US"]

# Remove any missing or invalid values
treasury10_values <- na.omit(treasury10_values)
mortgage30_values <- na.omit(mortgage30_values)

# Convert the xts objects to data frames
treasury10_df <- data.frame(Date = index(treasury10_values), Treasury10 = coredata(treasury10_values))
mortgage30_df <- data.frame(Date = index(mortgage30_values), Mortgage30 = coredata(mortgage30_values))

# Merge the data frames based on the dates
merged_data <- merge(treasury10_df, mortgage30_df, by = "Date")

# Rename the columns
colnames(merged_data) <- c("Date", "series1", "series2")

# Perform cointegration test
coint_test <- ca.jo(merged_data[, c("series1", "series2")], type = "trace", ecdet = "const", K = 2)

# Check for cointegration
summary(coint_test)

# Estimate VECM (Vector Error Correction Model)
vecm <- vec2var(coint_test, r = 1)

# Get the estimated parameters
beta <- coef(vecm)

# Extract the error correction term
ect <- beta[3, 1]

# Compute the forecast for the next 12 months
n_months <- 12
forecast <- predict(vecm, n.ahead = n_months)

# Print the forecasted values
cat("Forecasted values for the next", n_months, "months:\n")
print(forecast$fcst$series1)

# Perform ECM diagnostics
ecm_diagnostics <- summary(vecm)

# Print ECM diagnostics
cat("\nECM Diagnostics:\n")
print(ecm_diagnostics)

# Regression of 30-Year Mortgage on 10-Year Rate
regression <- lm(series2 ~ series1, data = merged_data)
residuals <- residuals(regression)
summary(regression)
merged_data
sd(merged_data$series1)
sd
# ADF test on residuals
adf_test <- adf.test(residuals, alternative = "stationary",k=0)
cat("\nADF Test on Residuals:\n")
print(adf_test)

# Set the figure size and margins to fit the plot window
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Calculate the x-axis values for the forecasted data
forecast_start <- as.Date(tail(index(merged_data$series1), 1))
n_months <- length(forecast$fcst$series1)
forecast_dates <- seq(forecast_start + 1, by = "1 month", length.out = n_months)

# Plot the two rates and their forecasts
plot(merged_data$series1, type = "l", col = "blue", xlab = "Time", ylab = "Yield",
     main = "10-Year and 30-Yr Mortgage Rate", ylim = range(merged_data$series1, merged_data$series2, forecast$fcst$series1))
lines(merged_data$series2, col = "red")
lines(forecast_dates, forecast$fcst$series1, col = "blue", lty = "dashed")
lines(forecast_dates, forecast$fcst$series2, col = "red", lty = "dashed")
legend("topright", legend = c("10-Year Yield", "30-Yr Mortgage Forecast", "10-Year Forecast", "30-Yr Mortgage Forecast"),
       col = c("blue", "red", "blue", "red"), lty = c("solid", "solid", "dashed", "dashed"), cex = 0.8)

# Load required libraries
library(zoo)

# Convert the variables to zoo objects
series1_zoo <- zoo(merged_data$series1, order.by = merged_data$Date)
series2_zoo <- zoo(merged_data$series2, order.by = merged_data$Date)
residuals_zoo <- zoo(residuals, order.by = merged_data$Date[-1])

# Estimate the ECM (Error Correction Model)
ecm <- dynlm(diff(series2_zoo) ~ diff(series1_zoo) + L(residuals_zoo, 1) - 1)

# Print the entire output of the model
summary(ecm)

# Calculate the residuals of the VECM
residuals_vecm <- residuals(vecm)

# Compute the variance-covariance matrix of the residuals
residuals_cov <- cov(residuals_vecm)

# Calculate variance decomposition using impulse response function
irf_obj <- irf(vecm, impulse = "series1", response = c("series1", "series2"), n.ahead = n_months)
var_decomp <- irf_obj$irf

# Print variance decomposition results
cat("Variance Decomposition:\n")
print(var_decomp)

# Calculate the residuals of the VECM
residuals_vecm <- residuals(vecm)

# Compute the variance-covariance matrix of the residuals
residuals_cov <- cov(residuals_vecm)

# Calculate variance decomposition using fevd
var_decomp <- fevd(vecm)

# Print variance decomposition results
cat("Variance Decomposition:\n")
print(var_decomp)

# Plot graphical representation of variance decomposition
plot(var_decomp, main = "Variance Decomposition")