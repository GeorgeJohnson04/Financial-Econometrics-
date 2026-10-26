# =============================================================================
# SARIMA Analysis: Large Bank Consumer Credit Card Balances — Total
# FRED Series: RCCCBBALTOT (same data, republished by FRED)
# Source: Federal Reserve Bank of Philadelphia, Large Bank Credit Card and
#         Mortgage Data (FR Y-14M), downloaded directly from the Philadelphia Fed
# Units: billions of dollars, quarterly, not seasonally adjusted
# =============================================================================
# Required packages — uncomment to install if needed:
# install.packages("forecast")
# install.packages("tseries")
library(forecast)
library(tseries)


# =============================================================================
# SECTION 1: DATA RETRIEVAL FROM THE PHILADELPHIA FED
# =============================================================================
# FRED only republishes this series; the Philadelphia Fed is the original
# publisher and posts a CSV with each quarterly release. No API key needed.
#
# The CSV's address changes every quarter (it contains the year and quarter),
# so the script first reads the Philadelphia Fed data page and finds the
# current link. If that fails, it falls back to the last known address.

PHILLY_PAGE  <- "https://www.philadelphiafed.org/surveys-and-data/large-bank-credit-card-and-mortgage-data"
FALLBACK_CSV <- "https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/Y14/2026/Q2/26Q2-CreditCardBalances.csv"

find_balances_csv <- function() {
  page  <- tryCatch(readLines(PHILLY_PAGE, warn = FALSE), error = function(e) NULL)
  if (is.null(page)) return(FALLBACK_CSV)
  links <- unlist(regmatches(page,
             gregexpr("[^\"' ]*CreditCardBalances\\.csv", page, ignore.case = TRUE)))
  if (length(links) == 0) return(FALLBACK_CSV)
  link <- links[1]
  if (!grepl("^https?://", link)) link <- paste0("https://www.philadelphiafed.org", link)
  link
}

csv_url <- find_balances_csv()
cat("Downloading:", csv_url, "\n")

cc_raw <- read.csv(csv_url, check.names = FALSE, stringsAsFactors = FALSE)

# Column 1 is the quarter (e.g. "2012Q3"); total balances is the column named
# "Total Balances ($Billions)". Values are text like "$1,234.56", so strip
# the dollar signs and commas before converting to numbers.
bal_col <- grep("^Total Balances", names(cc_raw), value = TRUE)[1]
if (is.na(bal_col)) stop("Total Balances column not found; the file layout may have changed.",
                         call. = FALSE)

cc_raw      <- cc_raw[grepl("^[0-9]{4}Q[1-4]$", cc_raw[[1]]), ]
ccbal_raw   <- as.numeric(gsub("[$,]", "", cc_raw[[bal_col]]))
ccbal_qtrs  <- cc_raw[[1]]

# Quarter-start dates (used for the year labels on the plots)
start_year  <- as.integer(substr(ccbal_qtrs, 1, 4))
start_qtr   <- as.integer(substr(ccbal_qtrs, 6, 6))
ccbal_dates <- as.Date(sprintf("%d-%02d-01", start_year, 3 * start_qtr - 2))

# Order by date in case the file is not sorted
ord         <- order(ccbal_dates)
ccbal_raw   <- ccbal_raw[ord]
ccbal_dates <- ccbal_dates[ord]
ccbal_qtrs  <- ccbal_qtrs[ord]

cat("Sample:", ccbal_qtrs[1], "to", ccbal_qtrs[length(ccbal_qtrs)],
    "(", length(ccbal_raw), "quarters )\n")

# Determine quarterly start for ts objects
start_year <- as.integer(format(ccbal_dates[1], "%Y"))
start_qtr  <- ceiling(as.integer(format(ccbal_dates[1], "%m")) / 3)

# Level series as a proper quarterly ts object (equivalent to data$CCBAL)
Original_CCBAL_Series <- ts(ccbal_raw,
                            start     = c(start_year, start_qtr),
                            frequency = 4)

# One-period difference of the level series
Differenced_CCBAL_Series <- diff(Original_CCBAL_Series, differences = 1)

# Seasonal difference applied on top of the one-period difference (lag = 4)
Seasonally_Differenced_CCBAL_Series <- diff(Differenced_CCBAL_Series,
                                            differences = 1, lag = 4)

# Date vectors for the differenced series (for axis labeling)
diff_dates     <- ccbal_dates[-1]                        # after one-period diff
seas_diff_dates <- ccbal_dates[-(1:5)]                   # after seasonal diff (loses 5 obs)


# =============================================================================
# SECTION 2: HELPER — SHARED PLOT THEME
# =============================================================================

set_theme <- function() {
  par(
    bg        = "white",
    col.axis  = "#333333",
    col.lab   = "#333333",
    col.main  = "#1a1a2e",
    col.sub   = "#555555",
    font.main = 2,
    font.lab  = 1,
    cex.main  = 1.15,
    cex.lab   = 0.95,
    cex.axis  = 0.85,
    mgp       = c(2.2, 0.6, 0),
    las       = 1
  )
}

add_grid <- function() {
  grid(nx = NULL, ny = NULL,
       col = "#e0e0e0", lty = 1, lwd = 0.8)
}

# Helper: build a year-labeled x-axis for any ts object + matching date vector
year_axis <- function(ts_obj, dates_vec, by = 8) {
  idx <- seq(1, length(ts_obj), by = by)
  axis(1,
       at     = time(ts_obj)[idx],
       labels = format(dates_vec[idx], "%Y"))
}


# =============================================================================
# SECTION 3: EXPLORATORY PLOTS — LEVEL SERIES
# =============================================================================

# --- 3a. Level Series --------------------------------------------------------
set_theme()
plot(Original_CCBAL_Series,
     type = "l",
     col  = "#2166ac",
     lwd  = 1.8,
     main = "Large Bank Consumer Credit Card Balances \u2014 Level Series\n(RCCCBBALTOT)",
     ylab = "Total Balances ($ billions)",
     xlab = "Year",
     xaxt = "n")
add_grid()
year_axis(Original_CCBAL_Series, ccbal_dates)
lines(Original_CCBAL_Series, col = "#2166ac", lwd = 1.8)
box(col = "#aaaaaa")


# =============================================================================
# SECTION 4: STATIONARITY TESTING — LEVEL SERIES
# =============================================================================

cat("\n", strrep("=", 60), "\n")
cat("  ADF TEST \u2014 LEVEL SERIES (RCCCBBALTOT)\n")
cat(strrep("=", 60), "\n")
adf_level <- adf.test(Original_CCBAL_Series, k = 0)
print(adf_level)

# ACF / PACF — Level Series
# as.numeric() strips frequency=4 so lags are labeled 1, 2, 3, ... (quarters),
# not 1, 2, 3, ... (years)
set_theme()
acf(as.numeric(Original_CCBAL_Series),
    main   = "ACF \u2014 Level Series (RCCCBBALTOT)",
    xlab   = "Lag (quarters)",
    col    = "#2166ac",
    lwd    = 2,
    ci.col = "#aaaaaa")

set_theme()
pacf(as.numeric(Original_CCBAL_Series),
     main   = "PACF \u2014 Level Series (RCCCBBALTOT)",
     xlab   = "Lag (quarters)",
     col    = "#2166ac",
     lwd    = 2,
     ci.col = "#aaaaaa")


# =============================================================================
# SECTION 5: ONE-PERIOD DIFFERENCED SERIES
# =============================================================================

# --- 5a. Plot ----------------------------------------------------------------
set_theme()
plot(Differenced_CCBAL_Series,
     type = "l",
     col  = "#d6604d",
     lwd  = 1.8,
     main = "Credit Card Balances \u2014 One-Period Difference\n(\u0394RCCCBBALTOT)",
     ylab = "\u0394 Total Balances ($ billions)",
     xlab = "Year",
     xaxt = "n")
add_grid()
year_axis(Differenced_CCBAL_Series, diff_dates)
abline(h = 0, col = "#555555", lty = 2, lwd = 1)
lines(Differenced_CCBAL_Series, col = "#d6604d", lwd = 1.8)
box(col = "#aaaaaa")

# --- 5b. ADF Test ------------------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("  ADF TEST \u2014 ONE-PERIOD DIFFERENCED SERIES\n")
cat(strrep("=", 60), "\n")
cat("  Note: A seasonal difference is also required regardless of this result.\n")
cat("  The ADF does not detect the absence of seasonal stationarity.\n")
cat(strrep("=", 60), "\n")
adf_diff <- adf.test(Differenced_CCBAL_Series)
print(adf_diff)

# --- 5c. ACF / PACF ----------------------------------------------------------
set_theme()
acf(as.numeric(Differenced_CCBAL_Series),
    main   = "ACF \u2014 One-Period Differenced Series",
    xlab   = "Lag (quarters)",
    col    = "#d6604d",
    lwd    = 2,
    ci.col = "#aaaaaa")

set_theme()
pacf(as.numeric(Differenced_CCBAL_Series),
     main   = "PACF \u2014 One-Period Differenced Series",
     xlab   = "Lag (quarters)",
     col    = "#d6604d",
     lwd    = 2,
     ci.col = "#aaaaaa")


# =============================================================================
# SECTION 6: SEASONALLY DIFFERENCED SERIES (one-period diff + seasonal diff)
# =============================================================================

# --- 6a. Plot ----------------------------------------------------------------
set_theme()
plot(Seasonally_Differenced_CCBAL_Series,
     type = "l",
     col  = "#4dac26",
     lwd  = 1.8,
     main = "Credit Card Balances \u2014 One-Period + Seasonal Difference\n(\u0394\u0394\u2084RCCCBBALTOT)",
     ylab = "Seasonally Differenced Balances ($ billions)",
     xlab = "Year",
     xaxt = "n")
add_grid()
year_axis(Seasonally_Differenced_CCBAL_Series, seas_diff_dates)
abline(h = 0, col = "#555555", lty = 2, lwd = 1)
lines(Seasonally_Differenced_CCBAL_Series, col = "#4dac26", lwd = 1.8)
box(col = "#aaaaaa")

# --- 6b. ADF Test ------------------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("  ADF TEST \u2014 SEASONALLY DIFFERENCED SERIES\n")
cat(strrep("=", 60), "\n")
adf_seas <- adf.test(Seasonally_Differenced_CCBAL_Series)
print(adf_seas)

# --- 6c. ACF / PACF ----------------------------------------------------------
# These are the key correlograms for identifying the SARIMA(p,d,q)(P,D,Q)[4] orders.
# Lags are labeled 1, 2, 3, ... via as.numeric().
set_theme()
acf(as.numeric(Seasonally_Differenced_CCBAL_Series),
    main   = "ACF \u2014 Seasonally Differenced Series (guides MA / SMA orders)",
    xlab   = "Lag (quarters)",
    col    = "#4dac26",
    lwd    = 2,
    ci.col = "#aaaaaa")

set_theme()
pacf(as.numeric(Seasonally_Differenced_CCBAL_Series),
     main   = "PACF \u2014 Seasonally Differenced Series (guides AR / SAR orders)",
     xlab   = "Lag (quarters)",
     col    = "#4dac26",
     lwd    = 2,
     ci.col = "#aaaaaa")


# =============================================================================
# SECTION 7: SARIMA(1,1,0)(0,1,1)[4] MODEL
# =============================================================================
# The model is fit on the ORIGINAL (undifferenced) level series.
# The d=1 and D=1 parameters instruct R to apply the one-period and
# seasonal differences internally before estimation.
# =============================================================================

cat("\n", strrep("=", 60), "\n")
cat("  SARIMA(1,1,0)(0,1,1)[4] \u2014 MODEL SUMMARY\n")
cat(strrep("=", 60), "\n\n")

sarima_model <- forecast::Arima(Original_CCBAL_Series,
                                order    = c(1, 1, 0),
                                seasonal = list(order = c(0, 1, 1), period = 4))
summary(sarima_model)

# --- Residual Diagnostics ----------------------------------------------------
set_theme()
forecast::checkresiduals(sarima_model)

# --- Actual vs. Fitted -------------------------------------------------------
set_theme()
plot(sarima_model$x,
     col  = "#d6604d",
     lwd  = 1.6,
     type = "l",
     main = "SARIMA(1,1,0)(0,1,1)[4]: Actual vs. Fitted \u2014 Level Series",
     ylab = "Total Balances ($ billions)",
     xlab = "Year",
     xaxt = "n")
add_grid()
year_axis(Original_CCBAL_Series, ccbal_dates)
lines(sarima_model$x,       col = "#d6604d", lwd = 1.6)
lines(fitted(sarima_model), col = "#2166ac", lwd = 1.6)
legend("topleft",
       legend = c("Actual", "Fitted"),
       col    = c("#d6604d", "#2166ac"),
       lty    = 1, lwd = 2, bty = "n", cex = 0.9)
box(col = "#aaaaaa")


# =============================================================================
# SECTION 8: 12-QUARTER FORECAST
# =============================================================================

fc_raw <- predict(sarima_model, n.ahead = 12)
fc_df  <- data.frame(
  Quarter    = paste0("Q", seq_len(12)),
  Forecast   = round(fc_raw$pred,  2),
  SE         = round(fc_raw$se,    2),
  Lower_95CI = round(fc_raw$pred - 1.96 * fc_raw$se, 2),
  Upper_95CI = round(fc_raw$pred + 1.96 * fc_raw$se, 2)
)

cat("\n", strrep("=", 60), "\n")
cat("  SARIMA(1,1,0)(0,1,1)[4]: 12-Quarter Level Forecast\n")
cat(strrep("=", 60), "\n\n")
print(fc_df, row.names = FALSE)

# --- Forecast Plot -----------------------------------------------------------
# Build a forecast object via forecast() for the shaded-band plot
futureval <- forecast::forecast(sarima_model, h = 12)

set_theme()
plot(futureval,
     main      = "SARIMA(1,1,0)(0,1,1)[4]: 12-Quarter Forecast \u2014 Level Series",
     ylab      = "Total Balances ($ billions)",
     xlab      = "Year",
     col       = "#d6604d",
     fcol      = "#2166ac",
     shadecols = c("#c6dbef", "#deebf7"),
     lwd       = 1.6)
add_grid()
box(col = "#aaaaaa")

# =============================================================================
# NOTE: Unlike a plain ARIMA model fitted on a differenced series, the SARIMA
# forecast produced here is already expressed in LEVELS (the original series
# units). The SE column can be used to construct 95% confidence intervals:
#   Upper CI = Forecast + 1.96 * SE
#   Lower CI = Forecast - 1.96 * SE
# These are included in the fc_df table above and can be pasted into Excel
# to extend an actual-vs-forecast chart with confidence bands.
# =============================================================================