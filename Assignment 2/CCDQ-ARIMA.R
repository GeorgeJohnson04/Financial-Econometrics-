# Libraries
# If you don't have these packages installed in R, you need to run
# the install.packages function.
#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)

###############################################################################
## DATA: Delinquency rate on credit card loans, all commercial banks (quarterly)
## Source: Federal Reserve Board, Charge-off and Delinquency Rates (CHGDEL),
##         downloaded live from the Board's Data Download Program.
## No API key, no CSV file, and no setwd() needed.
##
## The Board publishes this series both seasonally adjusted and not seasonally
## adjusted. Set SEASONALLY_ADJUSTED to match the series you want.
##   TRUE  = seasonally adjusted     (same data as FRED series DRCCLACBS)
##   FALSE = not seasonally adjusted (same data as FRED series DRCCLACBN)
###############################################################################
SEASONALLY_ADJUSTED <- TRUE

## Preformatted Board package "Delinquency rates, all commercial banks"
## (20 series, seasonally adjusted and not seasonally adjusted, CSV format)
DDP_URL <- paste0("https://www.federalreserve.gov/datadownload/Output.aspx?",
                  "rel=CHGDEL&series=9c215a53d083d9416a3f3625bcb1223d",
                  "&lastobs=&from=&to=&filetype=csv&label=include",
                  "&layout=seriescolumn&type=package")

## Board series identifiers for the credit card delinquency rate
SERIES_ID <- if (SEASONALLY_ADJUSTED) {
  "STFBQDCC%STFBAILCC_XEOP_MA.Q"
} else {
  "STFBQDCC%STFBAILCC_XEOP_MA_N.Q"
}

## The file has 5 metadata rows (description, unit, multiplier, currency,
## identifier), then a header row starting with "Time Period", then the data.
## Dates look like 1991Q1.
raw <- read.csv(DDP_URL, skip = 5, header = TRUE, check.names = FALSE,
                stringsAsFactors = FALSE, na.strings = c("", "NA", "ND"))

if (!SERIES_ID %in% names(raw)) {
  stop("Credit card delinquency series not found in the Board download. ",
       "The package layout may have changed.", call. = FALSE)
}

periods <- raw[["Time Period"]]
values  <- as.numeric(raw[[SERIES_ID]])

## The package starts in 1985, but the credit card series starts in 1991Q1,
## so drop the empty rows before the first and after the last observation
first_obs <- which(!is.na(values))[1]
last_obs  <- max(which(!is.na(values)))
periods   <- periods[first_obs:last_obs]
values    <- values[first_obs:last_obs]
if (anyNA(values)) stop("Missing values inside the sample period.", call. = FALSE)

start_year    <- as.integer(substr(periods[1], 1, 4))
start_quarter <- as.integer(substr(periods[1], 6, 6))

## Build the same two series the CSV used to contain:
## CCDQ    = level of the delinquency rate (percent)
## CHGCCDQ = one-period (quarter-over-quarter) change in the rate
data <- list()
data$CCDQ    <- ts(values, start = c(start_year, start_quarter), frequency = 4)
data$CHGCCDQ <- diff(data$CCDQ, differences = 1)

cat("Credit card delinquency rate,",
    if (SEASONALLY_ADJUSTED) "seasonally adjusted" else "not seasonally adjusted", "\n")
cat("Sample:", periods[1], "to", periods[length(periods)],
    "(", length(values), "quarters )\n\n")

## Plot the original series
plot(data$CCDQ, type="l",
     main="Original Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Year")             # Adding a label to the X axis

## Plot the one-period difference series
plot(data$CHGCCDQ, type="l",
     main="One-Period Difference Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Year")             # Adding a label to the X axis

## Perform stationarity test on level series included in the dataset
adf_test <- adf.test(data$CCDQ, k=0)

# Print the ADF test result with a custom title
cat("ADF Test for the Original Series\n")
print(adf_test)

# Check the ACF and PACF
# as.numeric() keeps the lags labeled in quarters (1, 2, 3, ...) rather than years
acf(as.numeric(data$CCDQ), main="ACF of the Original Series")
pacf(as.numeric(data$CCDQ), main="PACF of the Original Series")

# Perform stationarity tests on differenced series included in the dataset
adf_test <- adf.test(data$CHGCCDQ, k=0)

# Print the ADF test result with a custom title
cat("ADF Test for the Differenced Series\n")
print(adf_test)

# Check the ACF and PACF
acf(as.numeric(data$CHGCCDQ), main="ACF of the Differenced Series")
pacf(as.numeric(data$CHGCCDQ), main="PACF of the Differenced Series")

##### MODEL #####
## Fit Arima model on the differenced series, with 1 AR term
(arima_model_simpler <- Arima(data$CHGCCDQ, c(1, 0, 0)))

## Check residuals
checkresiduals(arima_model_simpler)

# Plot the actual and fitted series
plot(arima_model_simpler$x, col='red', type="l",
     main="Actual vs. Fitted",  # Adding a title to the chart
     ylab="Series Values",      # Adding a label to the Y axis
     xlab="Year")               # Adding a label to the X axis

lines(fitted(arima_model_simpler), col='blue')

# Adding legends for the blue and red lines
legend("topleft", legend=c("Actual", "Fitted"), col=c("red", "blue"), lty=1)

## Produce forecast, in quarters. Note this is the forecast of the change.
## To see how they affect the original level series, add
## the forecasted change to the series
futureval<-forecast(arima_model_simpler,h=12)
futureval

## The level forecast built from this model is in the next section



# =============================================================================
# LEVEL FORECAST CHART
# =============================================================================
# The model above forecasts the CHANGE in the delinquency rate. Here it is
# re-estimated on the LEVEL series with d = 1, so R undoes the differencing
# itself. The forecast and its 80% / 95% bands come out in levels (percent),
# with no Excel step needed.
#
# ARIMA(1,0,0) with a mean, fitted on the differenced series, is the same model
# as ARIMA(1,1,0) with drift fitted on the level series. The drift term plays
# the role of the mean, so the AR coefficient printed below should match the
# one estimated above.

FC_HORIZON   <- 12   # quarters ahead (3 years), same horizon as above
FC_HIST_SHOW <- 56   # quarters of history shown on the chart (14 years)

# --- Shared plot theme -------------------------------------------------------
set_theme <- function() {
  par(
    bg        = "white",
    col.axis  = "#333333",
    col.lab   = "#333333",
    col.main  = "#1a1a2e",
    font.main = 2,
    cex.main  = 1.15,
    cex.lab   = 0.95,
    cex.axis  = 0.85,
    mgp       = c(2.2, 0.6, 0),
    las       = 1
  )
}

# --- Fit on levels, print the level forecast table, and draw the chart -------
level_forecast_chart <- function(ar_order, label) {

  level_model <- Arima(data$CCDQ,
                       order         = c(ar_order, 1, 0),
                       include.drift = TRUE)
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("  ", label, ": ARIMA(", ar_order, ",1,0) with drift on the level series\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  print(level_model)

  level_fc <- forecast(level_model, h = FC_HORIZON, level = c(80, 95))

  # Forecast table in levels
  fc_time <- time(level_fc$mean)
  fc_df <- data.frame(
    Quarter    = paste0(floor(fc_time + 1e-6), "Q", cycle(level_fc$mean)),
    Forecast   = round(as.numeric(level_fc$mean),          2),
    Lower_80CI = round(as.numeric(level_fc$lower[, "80%"]), 2),
    Upper_80CI = round(as.numeric(level_fc$upper[, "80%"]), 2),
    Lower_95CI = round(as.numeric(level_fc$lower[, "95%"]), 2),
    Upper_95CI = round(as.numeric(level_fc$upper[, "95%"]), 2)
  )
  cat("\n", FC_HORIZON, "-Quarter Level Forecast: Credit Card Delinquency Rate (%)\n", sep = "")
  print(fc_df, row.names = FALSE)

  # Chart
  set_theme()
  plot(level_fc,
       include   = FC_HIST_SHOW,
       main      = paste0("ARIMA(", ar_order, ",1,0): ", FC_HORIZON,
                          "-Quarter Forecast — Level Series"),
       ylab      = "Credit Card Delinquency Rate (%)",
       xlab      = "Year",
       col       = "#d6604d",                  # actual series
       fcol      = "#2166ac",                  # forecast line
       shadecols = c("#c6dbef", "#deebf7"),    # 80% band, 95% band
       lwd       = 1.6)

  # Gridlines, then redraw the series so they sit on top of the grid
  grid(nx = NULL, ny = NULL, col = "#e0e0e0", lty = 1, lwd = 0.8)
  hist_start <- time(level_fc$x)[length(level_fc$x) - FC_HIST_SHOW + 1]
  lines(window(level_fc$x, start = hist_start), col = "#d6604d", lwd = 1.6)
  lines(level_fc$mean, col = "#2166ac", lwd = 2)
  box(col = "#aaaaaa")

  invisible(list(model = level_model, forecast = level_fc, table = fc_df))
}

# --- AR(1) model -------------------------------------------------------------
fc_simpler <- level_forecast_chart(1, "AR(1) MODEL")

# Uncomment to export the level forecast table for Excel
# write.csv(fc_simpler$table, "ccdq_forecast_ar1.csv", row.names = FALSE)
