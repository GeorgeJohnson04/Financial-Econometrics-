# This program estimates a VAR set of equations between two interest rate
# series to test the presence of Granger Causality.

# Load required packages
library(vars)
library(quantmod)

# Note that no data file is needed. R gets the interest rate file directly
# from FRED using the "getSymbols" script. Very helpful.

# Retrieve 10-Year and 3-Month Treasury Yield data from FRED
getSymbols("GS10", src = "FRED", from = "2010-01-01", to = Sys.Date())
getSymbols("DTB3", src = "FRED", from = "2010-01-01", to = Sys.Date())

# Extract the 10-Year Treasury Yield values
treasury10_values <- GS10[, "GS10"]

plot(treasury10_values, type="l",col = "blue", xlab = "Time", ylab = "Treasury Rate", 
     main = "10-Year Treasury Rate")

# Extract the 3-Month Treasury Yield values
treasury3_values <- DTB3[, "DTB3"]

plot(treasury3_values, type="l",col = "red", xlab = "Time", ylab = "Treasury Rate", 
     main = "3-Month Treasury Rate")


# Remove any missing or invalid values
treasury10_values <- na.omit(treasury10_values)
treasury3_values <- na.omit(treasury3_values)

# Exclude zero or negative values
treasury10_values <- treasury10_values[treasury10_values > 0]
treasury3_values <- treasury3_values[treasury3_values > 0]

# Calculate daily returns for 10-Year and 3-Month Treasury Yields
treasury10_returns <- diff(log(treasury10_values))
treasury3_returns <- diff(log(treasury3_values))

# Check if the first observation is missing in both series
if (is.na(treasury10_returns[1]) || is.na(treasury3_returns[1])) {
  # Remove the first observation from both series
  treasury10_returns <- treasury10_returns[-1]
  treasury3_returns <- treasury3_returns[-1]
}

# Select the last 160 daily returns for 10-Year and 3-Month Treasury Yields
treasury10_returns <- tail(treasury10_returns, 160)
treasury3_returns <- tail(treasury3_returns, 160)

# Create a data frame with the time series variables
data <- data.frame(series1 = treasury10_returns, series2 = treasury3_returns)
data

colnames(data) <- c('series1', 'series2')

# Fit a VAR model with one lag of each variable
var_model <- VAR(data, p = 1, type = "const")

# Display the VAR model results to check whether there is Granger
# Causality between the variables, in both or either direction.
# Granger causality implies a relationship or correspondence, but
# does not necessarily (although it could involve) a strict causal
# effect.
summary(var_model)

# The null hypothesis is no Granger Causality. A significant relationship
# between a given series and the lagged values of the other series implies
# the presence of Granger Causality.
