# Load necessary packages
library(vars)

# This program simulates two series X and Y to show how the cross-correlation
# function (CCF) works.

# Set seed for reproducibility
set.seed(123)

# Generate correlated variables x and y
n <- 50  # Number of observations
epsilon_x <- rnorm(n)  # Error term for variable x
epsilon_y <- rnorm(n)  # Error term for variable y
x <- 0.8 * epsilon_x + sqrt(1 - 0.8^2) * epsilon_y  # Variable x with correlation
y <- 0.8 * epsilon_y + sqrt(1 - 0.8^2) * epsilon_x  # Variable y with correlation

# Create a data frame with variables x and y
data <- data.frame(x, y)

# Plot the x and y series
plot(x, type = "l", col = "blue", xlab = "Time", ylab = "x", main = "Time Series of x and y")
lines(y, col = "red")

# Calculate cross-correlation of original x and y variables
ccf_original <- ccf(x, y)

# Plot cross-correlation function of original x and y variables
plot(ccf_original, main = "Cross-Correlation Function of Original x and y", xlab = "Lag", ylab = "Cross-Correlation")

# Estimate VAR model
var_model <- VAR(data, p = 5)  # Set the desired lag order (p)

# Extract residuals
var_residuals <- residuals(var_model)

# Calculate cross-correlation of residuals
ccf_result <- ccf(var_residuals[, "x"], var_residuals[, "y"], plot = FALSE)

# Calculate the y-axis limits
ylim <- max(abs(ccf_result$acf), na.rm = TRUE) * 1.1

# Plot cross-correlation function with significance threshold
plot(ccf_result, main = "Cross-Correlation Function of Residuals", xlab = "Lag", ylab = "Cross-Correlation", ylim = c(-ylim, ylim))
abline(h = critical_value, lty = "dotted", col = "red")
abline(h = -critical_value, lty = "dotted", col = "red")

# Assess significance of cross-correlation
conf_level <- 0.95  # Set the desired confidence level
critical_value <- qnorm((1 + conf_level) / 2) / sqrt(n)
significant_lags <- ccf_result$lag[abs(ccf_result$acf) > critical_value]

# Print significant lags
cat("Significant lags:")
if (length(significant_lags) > 0) {
  cat(paste(significant_lags, collapse = ", "))
} else {
  cat("None")
}
