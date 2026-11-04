# Libraries
library(forecast)
library(tseries)

## Modify the setwd line to match your folder location
setwd("D:/TEACHING/Summer 2023/Financial Econometrics/Data/Lecture 6")

## Read files with monthly euro and monthly change in the monthly euro
data <- read.csv("CW.csv", header = TRUE)

# Extract the cw column from the data frame
data_column <- data$CW
head(data_column)

# Make it a time series, specify appropriate frequency
ts_data <- ts(data_column, frequency =12)

#Plot the series
plot(ts_data)

#Ensure the data is a time series object
ts_data <- ts(ts_data)

# Check the stationarity of the series to see if needs to be differenced
adf_test <- adf.test(ts_data)
print(adf_test)

# Check the ACF and PACF
acf(ts_data)
pacf(ts_data)

#Apply a one-lag difference to the series as usual
differenced_ts_data <- diff(ts_data, differences = 1)
plot(differenced_ts_data)

# Apply ADF to see what it shows, although the series will need seasonal
# differencing regardless of the ADF result
adf_test <- adf.test(differenced_ts_data)
print(adf_test)

# Check the ACF and PACF
acf(differenced_ts_data)
pacf(differenced_ts_data)

# Since the series is seasonal, a seasonal difference is also needed at the 
# appropriate frequency, even if the ADF rejects non-stationarity. The ADF does
# not capture the lack of "seasonal stationarity"
seasonal_differenced_ts_data <- diff(differenced_ts_data, differences = 1, lag = 12)
plot(seasonal_differenced_ts_data)

# Check stationarity on the series after regular and seasonal differencing
adf_test <- adf.test(seasonal_differenced_ts_data)
print(adf_test)

# Check the ACF and PACF to decide on AR and MA orders
acf_plot <- acf(seasonal_differenced_ts_data)
pacf_plot <- pacf(seasonal_differenced_ts_data)

# Fit the SARIMA model on the original series, including the appropriate differencing
# parameters. The middle parameter "d" in the c vector defines the differencing order
arima_model = forecast::Arima(ts_data, order = c(4,1,0))
summary(arima_model)
acf_plot<-acf(arima_model$residuals)
pacf_plot<-pacf(arima_model$residuals)

sarima_model = forecast::Arima(ts_data, order = c(4,1,0), seasonal=list(order=c(0,1,1), period=12))
summary(sarima_model)

# Plot the fitted vs the residuals
plot.default(fitted(sarima_model), sarima_model$residuals)
plot(sarima_model$residuals)

# Additional residual diagnostics
forecast::checkresiduals(sarima_model)

#Plot the actual and fitted series
plot(sarima_model$x, col='red')
lines(fitted(sarima_model), col='blue')

# Produce forecast
print(data.frame(predict(sarima_model, n.ahead=12)))

