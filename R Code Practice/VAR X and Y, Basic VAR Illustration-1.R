# This program simulates two series and estimates a VAR
# system to illustrate how the relationships are
# estimated and how the lagged values
# of one series could related to current values of
# the other variable

# Start simulation here. Set the desired correlation coefficient between 0.5 and 0.7
correlation <- runif(1, 0.5, 0.7)

# Generate random data for the first time series
set.seed(0)
series1 <- rnorm(60)

# Generate the second time series correlated with the first
series2 <- 0.8 * series1 + correlation * c(series1[2:60], series1[1]) + sqrt(1 - correlation^2) * rnorm(60)

# Create a dataframe based on the simulated time series.
# Create a data frame with the time series variables
data <- data.frame(series1, series2)

# Plot for series1
plot(series1, type = "l", col = "blue", main = "Plot of series1")

# Plot for series2
plot(series2, type = "l", col = "blue", main = "Plot of series2")

# Load the 'vars' package
library(vars)

# Fit a VAR model with one lag of each variable
var_model <- VAR(data, p = 1, type = "const")

# Display the VAR model results
summary(var_model)