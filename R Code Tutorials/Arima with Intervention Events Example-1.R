library(tseries)
library(forecast)
library(ggplot2)

set.seed(0)

# Generate the first data series with two upward spike outliers
series1 <- rnorm(50)
series1[41] <- series1[41] + 7.5  # Add an upward spike outlier at position 41 (1.5 times larger)
series1[43] <- series1[43] + 7.5  # Add another upward spike outlier at position 43 (1.5 times larger)

# Generate the second data series with two downward spike outliers
series2 <- rnorm(50)
series2[41] <- series2[41] - 7.5  # Add a downward spike outlier at position 41 (1.5 times larger)
series2[43] <- series2[43] - 7.5  # Add another downward spike outlier at position 43 (1.5 times larger)

# Create an exogenous dummy variable for the spike periods
dummy <- rep(0, 50)
dummy[41:42] <- 1  # Set the dummy variable to 1 for the first spike
dummy[43:44] <- 1  # Set the dummy variable to 1 for the second spike
plot(dummy, type="l")

# Adjust the magnitude of the spikes in series2 while retaining the autocorrelation
series2[41] <- series2[41] - 3.75
series2[43] <- series2[43] - 3.75

# Print the series1 and series2
cat("Series 1:\n")
plot(series1, type = "l")
print(series1)
cat("\n")

cat("Series 2:\n")
plot(series2, type = "l")
print(series2)
cat("\n")

# Perform Augmented Dickey-Fuller (ADF) tests
cat("ADF Test for Series 1:\n")
adf_test1 <- adf.test(series1)
print(adf_test1)
cat("\n")

cat("ADF Test for Series 2:\n")
adf_test2 <- adf.test(series2)
print(adf_test2)
cat("\n")

# Plot ACF and PACF for series1
acf_series1 <- acf(series1, main = "ACF for Series 1")
print(acf_series1)
pacf_series1 <- pacf(series1, main = "PACF for Series 1")
print(pacf_series1)

# Plot ACF and PACF for series2
acf_series2 <- acf(series2, main = "ACF for Series 2")
print(acf_series2)
pacf_series2 <- pacf(series2, main = "PACF for Series 2")
print(pacf_series2)

# Fit ARIMA models to the two series with the dummy variable
model1 <- Arima(series1, order = c(2, 0, 0), xreg = dummy)
forecast::checkresiduals(model1)
model2 <- Arima(series2, order = c(4, 0, 0), xreg = dummy)
forecast::checkresiduals(model2)


# Fit ARIMA models to the two series without the dummy variable
model1_reg <- Arima(series1, order = c(2, 0, 0))
forecast::checkresiduals(model1_reg)
model2_reg <- Arima(series2, order = c(4, 0, 0))
forecast::checkresiduals(model2_reg)

# Generate forecasts assuming a dummy value of 1 and 0 respectively
forecast1_dummy1 <- forecast(model1, xreg = matrix(1, nrow = 12, ncol = 1))
forecast1_dummy0 <- forecast(model1, xreg = matrix(0, nrow = 12, ncol = 1))
forecast2_dummy1 <- forecast(model2, xreg = matrix(1, nrow = 12, ncol = 1))
forecast2_dummy0 <- forecast(model2, xreg = matrix(0, nrow = 12, ncol = 1))

# Generate forecasts without the dummy variable
forecast1_reg <- forecast(model1_reg, h = 12)
forecast2_reg <- forecast(model2_reg, h = 12)

# Prepare data for plotting
series1_data <- data.frame(Period = 1:50, Value = series1, Label = rep("Actual", 50))
forecast1_dummy1_data <- data.frame(Period = 51:62, Value = forecast1_dummy1$mean, Label = rep("w/dummy=1 in Forecast", 12))
forecast1_dummy0_data <- data.frame(Period = 51:62, Value = forecast1_dummy0$mean, Label = rep("w/dummy=0 in Forecast", 12))
forecast1_reg_data <- data.frame(Period = 51:62, Value = forecast1_reg$mean, Label = rep("No Dummy in Forecast", 12))

series2_data <- data.frame(Period = 1:50, Value = series2, Label = rep("Actual", 50))
forecast2_dummy1_data <- data.frame(Period = 51:62, Value = forecast2_dummy1$mean, Label = rep("w/dummy=1 in Forecast", 12))
forecast2_dummy0_data <- data.frame(Period = 51:62, Value = forecast2_dummy0$mean, Label = rep("w/dummy=0 in Forecast", 12))
forecast2_reg_data <- data.frame(Period = 51:62, Value = forecast2_reg$mean, Label = rep("No Dummy in Forecast", 12))

# Plot the actual series with forecasts for series 1
series1_plot <- ggplot() +
  geom_line(data = series1_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast1_dummy1_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast1_dummy0_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast1_reg_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  labs(title = "Series 1 with Forecasts") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_minimal()

# Plot the actual series with forecasts for series 2
series2_plot <- ggplot() +
  geom_line(data = series2_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast2_dummy1_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast2_dummy0_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  geom_line(data = forecast2_reg_data, aes(x = Period, y = Value, linetype = Label, color = Label), size = 1) +
  labs(title = "Series 2 with Forecasts") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_minimal()

# Display the plots
print(series1_plot)
print(series2_plot)

