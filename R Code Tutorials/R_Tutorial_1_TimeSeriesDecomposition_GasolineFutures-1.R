# ==============================================================================
# TIME SERIES DECOMPOSITION IN R
# Financial Econometrics — Tutorial 1
# ==============================================================================
#
# This tutorial demonstrates classical additive time series decomposition
# applied to RBOB Unleaded Gasoline Futures, a monthly financial price series
# downloaded directly from Yahoo Finance — no CSV file is required.
#
# SERIES    : RB=F — RBOB Unleaded Gasoline Futures (front-month contract)
# SOURCE    : Yahoo Finance via the quantmod package
# EXCHANGE  : CME / NYMEX
# UNITS     : U.S. Dollars per Gallon
# FREQUENCY : Daily (aggregated to monthly closing prices in this script)
# COVERAGE  : ~October 2005 to present (~20 years of monthly data)
#
# WHY THIS SERIES?
#   Gasoline futures are a purely financial series with no ambiguity about
#   whether they belong in a finance or macroeconomics curriculum — they are
#   actively traded financial contracts whose price is set by supply, demand,
#   and speculative positioning in derivatives markets.
#
#   The series exhibits all four classical decomposition components clearly:
#
#     TREND     — long-run price movements driven by global crude oil supply,
#                 refinery capacity, and structural energy demand trends
#     CYCLICAL  — medium-term price cycles tied to the broader business cycle,
#                 inventory build/draw cycles, and commodity market dynamics
#     SEASONAL  — a strong, consistent intra-year price pattern:
#                   • Jan–Mar : prices rise as refineries switch to the more
#                     expensive summer-blend formulation required by the EPA
#                   • Apr–Aug : peak prices during the summer driving season
#                               (Memorial Day through Labor Day)
#                   • Sep–Dec : prices fall as winter-blend (cheaper to produce)
#                               replaces summer-blend and driving activity drops
#     IRREGULAR — sudden price shocks from geopolitical events, hurricanes
#                 (Gulf Coast refining capacity), unexpected inventory data,
#                 and pandemic-era demand collapses
#
# DECOMPOSITION APPROACH
#   Step 1 — decompose() applies a classical additive model via a centered
#             12-month moving average, extracting a blended Trend-Cycle
#             component along with Seasonal and Irregular components.
#   Step 2 — We regress the Trend-Cycle on a linear time index via OLS.
#             The fitted line is the Pure Linear Trend; the residuals are
#             the Cyclical Component.  This yields all four components.
# ==============================================================================


# ==============================================================================
# SECTION 0 — PACKAGE SETUP
# ==============================================================================

# 'quantmod' provides:
#   getSymbols()  — downloads financial data from Yahoo Finance
#   to.monthly()  — aggregates daily prices to monthly
#   Cl()          — extracts the closing price column from OHLCV data
#
# Install once if needed:  install.packages("quantmod")

library(quantmod)


# ==============================================================================
# SECTION 1 — DOWNLOAD AND PREPARE DATA FROM YAHOO FINANCE
# ==============================================================================

# Download RBOB Gasoline Futures daily price data.
# We use auto.assign = FALSE because the "=" in the ticker symbol "RB=F"
# is not a valid R variable name character, which causes errors when
# quantmod tries to create a variable automatically.

gasoline_xts <- getSymbols("RB=F",
                            src         = "yahoo",
                            auto.assign = FALSE)

# Report the download result
cat("─────────────────────────────────────────────────────────\n")
cat("Series         : RB=F (RBOB Unleaded Gasoline Futures)\n")
cat("Daily obs.     :", nrow(gasoline_xts), "trading days downloaded\n")
cat("Period covered :", format(start(gasoline_xts)),
    "to", format(end(gasoline_xts)), "\n")
cat("─────────────────────────────────────────────────────────\n\n")

# Inspect the first few rows of the daily OHLCV data
head(gasoline_xts)

# --- Aggregate daily closing prices to monthly ----------------------------

# We take the last closing price of each calendar month.
# Cl() extracts the closing price column; to.monthly() handles aggregation.
# indexAt = "lastof" places the monthly observation at the last calendar
# day of each month (e.g., Jan 31, Feb 28, etc.).

gasoline_monthly <- to.monthly(Cl(gasoline_xts),
                               indexAt = "lastof",
                               OHLC    = FALSE)

cat("Monthly observations:", nrow(gasoline_monthly), "\n")
cat("Monthly period      :", format(start(gasoline_monthly)),
    "to", format(end(gasoline_monthly)), "\n\n")

head(gasoline_monthly)
summary(gasoline_monthly)

# Store the series start date; this makes the script robust to any future
# change in how far back Yahoo Finance's history extends
start_yr <- as.numeric(format(start(gasoline_monthly), "%Y"))
start_mo <- as.numeric(format(start(gasoline_monthly), "%m"))


# ==============================================================================
# SECTION 2 — CONVERT TO A REGULAR TIME SERIES OBJECT
# ==============================================================================

# R's decompose() function requires a ts (time series) object.
# We convert from xts format, specifying monthly frequency (freq = 12).

gasoline.ts <- ts(as.numeric(gasoline_monthly),
                  start     = c(start_yr, start_mo),
                  frequency = 12)

# Plot the raw monthly series before decomposition
plot(gasoline.ts,
     main = "RBOB Unleaded Gasoline Futures — Monthly Closing Price",
     ylab = "Price (USD per Gallon)",
     xlab = "Year",
     col  = "steelblue",
     lwd  = 1.5)


# ==============================================================================
# SECTION 3 — ADDITIVE DECOMPOSITION USING decompose()
# ==============================================================================

# decompose() applies classical additive decomposition via a centered
# 12-month moving average.  The assumed model is:
#
#   Observed(t)  =  TrendCycle(t)  +  Seasonal(t)  +  Irregular(t)
#
# NOTE ON MODEL CHOICE: For price series, a multiplicative model
# (Observed = Trend × Seasonal × Irregular) may be preferred if the
# amplitude of seasonal swings grows proportionally with the price level.
# We use the additive model here for consistency with the tutorial.  In
# practice, the seasonal amplitude of gasoline prices is reasonably stable
# in dollar-per-gallon terms, so the additive specification is defensible.
#
# KEY LIMITATION: The "trend" produced by decompose() is actually the
# TREND-CYCLE — it blends the long-run linear trend with medium-term
# cyclical fluctuations.  Section 4 separates these two components.

dec.gasoline <- decompose(gasoline.ts, type = "additive")

# Default decomposition plot (the "trend" panel here is trend + cycle blended).
# Note: plot() for decomposed.ts objects does not accept xlab or ylab
# arguments — passing them causes a graphics rendering error in RStudio.
plot(dec.gasoline)


# ==============================================================================
# SECTION 4 — SEPARATING TREND FROM CYCLE VIA OLS REGRESSION
# ==============================================================================

# Strategy: regress the trend-cycle component on a linear time index t.
#
#   TrendCycle(t)  =  α  +  β·t  +  ε(t)
#
#   Fitted values (α̂ + β̂·t)  →  Pure Linear Trend  (deterministic drift)
#   Residuals     (ε̂(t))      →  Cyclical Component  (medium-term deviations
#                                                      around that trend line)

# --- 4a. Assemble a data frame from the decomposition output ---------------

dec.df <- with(dec.gasoline,
               data.frame(
                 observed    = as.numeric(x),
                 trend_cycle = as.numeric(trend),
                 seasonal    = as.numeric(seasonal),
                 irregular   = as.numeric(random)
               ))

# Inspect: NAs appear at both ends because the centered 12-month moving
# average requires 6 observations on either side of every data point
head(dec.df)
tail(dec.df)

# --- 4b. Remove rows containing NAs ----------------------------------------

dec.df.clean <- dec.df[complete.cases(dec.df), ]

# --- 4c. Create an integer time index  t = 1, 2, 3, ..., T ----------------

dec.df.clean$time <- seq_len(nrow(dec.df.clean))

head(dec.df.clean, 10)

# --- 4d. Estimate the OLS regression  TrendCycle(t) = α + β·t + ε(t) ------

trend.lm <- lm(trend_cycle ~ time, data = dec.df.clean)

cat("\n── OLS Regression: Trend-Cycle Regressed on Linear Time Index ────\n")
print(summary(trend.lm))

# --- 4e. Extract the two separated components ------------------------------

dec.df.clean$trend    <- predict(trend.lm)  # fitted line  = Pure Linear Trend
dec.df.clean$cyclical <- resid(trend.lm)    # residuals    = Cyclical Component


# ==============================================================================
# SECTION 5 — DIAGNOSTIC PLOTS: TREND AND CYCLICAL COMPONENTS
# ==============================================================================

# These stand-alone plots verify the regression correctly separated the two
# components before we assemble the final four-panel chart.

# --- Pure Linear Trend ---
# ylim runs from 0 to at least 3.5, keeping the trend line in the middle
# of the chart space rather than crowded at the top.  The ceiling is
# data-driven (rounded up to the nearest 0.5 above the series max plus 1.0,
# with a floor of 3.5) so the axis adjusts if the sample period changes.
trend_top <- max(ceiling(max(dec.df.clean$trend, na.rm = TRUE) / 0.5) * 0.5
                 + 1.0, 3.5)

plot(dec.df.clean$time, dec.df.clean$trend,
     type = "l", lwd = 1.8, col = "steelblue",
     main = "Pure Linear Trend Component",
     xlab = "Time (months from series start)",
     ylab = "Gasoline Price (USD per Gallon)",
     ylim = c(0, trend_top))

# --- Cyclical Component ---
plot(dec.df.clean$time, dec.df.clean$cyclical,
     type = "l", lwd = 1.5, col = "darkorange",
     main = "Cyclical Component",
     sub  = "Deviations of the Trend-Cycle around the Pure Linear Trend",
     xlab = "Time (months from series start)",
     ylab = "Price Deviation (USD per Gallon)")
abline(h = 0, lty = 2, col = "gray50", lwd = 0.9)


# ==============================================================================
# SECTION 6 — FOUR-PANEL DECOMPOSITION CHART
# ==============================================================================

# The chart shows all four decomposition components in a single figure.
#
# Y-AXIS SCALING — KEY PEDAGOGICAL POINT
#   The TREND panel uses USD per gallon (the natural price unit) since it
#   represents the actual price level of the series.
#
#   The CYCLICAL, SEASONAL, and IRREGULAR panels all represent *deviations*
#   from that price level.  They are placed on a single shared, symmetric
#   y-axis scale so that their magnitudes are directly comparable.  This is
#   important: if the three deviation panels used different scales, a
#   visually large seasonal swing might be mistaken for something larger or
#   smaller than the cyclical component when the truth is the opposite.

# --- Compute a single symmetric y-axis range for the deviation panels -----

dev_max_raw <- max(
  abs(range(dec.df.clean$cyclical,  na.rm = TRUE)),
  abs(range(dec.df.clean$seasonal,  na.rm = TRUE)),
  abs(range(dec.df.clean$irregular, na.rm = TRUE))
)

# Round up to the nearest 0.5 so tick labels always fall on clean symmetric
# values (e.g. -1.0, -0.5, 0, 0.5, 1.0) rather than the uneven positions
# R selects automatically when axis limits are not round numbers.
dev_max  <- ceiling(dev_max_raw / 0.5) * 0.5
dev_ylim <- c(-dev_max, dev_max)

# Symmetric tick positions: just three labels per deviation panel
# (-dev_max, 0, +dev_max).  Showing only the outer bounds and the zero
# line keeps the axis readable at the compressed panel height while still
# making the scale and symmetry immediately clear to students.
dev_ticks <- c(-dev_max, 0, dev_max)

# --- Build calendar-year tick marks for the x-axis -----------------------

# The centered 12-month moving average removes 6 months of NAs from each
# end of the series.  We reconstruct the calendar dates of the observations
# in dec.df.clean by extracting the non-NA positions from the original ts.

all_dates <- seq(as.Date(paste0(start_yr, "-",
                                sprintf("%02d", start_mo), "-01")),
                 by         = "month",
                 length.out = length(gasoline.ts))

obs_dates <- all_dates[which(!is.na(as.numeric(dec.gasoline$trend)))]

# Place tick marks at January of every third year within the sample window
tick_years  <- seq(2007, 2030, by = 3)
tick_pos    <- sapply(tick_years, function(yr) {
  idx <- which(format(obs_dates, "%Y-%m") == sprintf("%d-01", yr))
  if (length(idx) == 0L) NA_integer_ else idx[1L]
})
valid_ticks <- !is.na(tick_pos)
tick_pos    <- tick_pos[valid_ticks]
tick_labels <- tick_years[valid_ticks]

# --- Set the four-panel layout and margin sizes ---------------------------

par(mfrow = c(4, 1),
    mar   = c(1.5, 6.5, 1.5, 1.5),   # bottom, left, top, right (in lines)
    oma   = c(3.5, 0.5, 5.0, 0.5),   # outer margins for shared labels
    mgp   = c(4.0, 0.7, 0))           # axis title, tick label, tick positions

# --- Panel 1: Pure Linear Trend (in original USD/gallon units) ------------
# ylim starts at 0 and uses the same trend_top ceiling computed in Section 5
# so the line sits toward the middle of the panel rather than the top.

plot(dec.df.clean$trend,
     type = "l", lwd = 1.8, col = "steelblue",
     main = "", xlab = "",
     ylab = "Trend ($/gal)",
     ylim = c(0, trend_top),
     xaxt = "n")
axis(1, at = tick_pos, labels = FALSE, tck = -0.04)

# --- Panel 2: Cyclical Component (shared deviation scale) -----------------

plot(dec.df.clean$cyclical,
     type = "l", lwd = 1.4, col = "darkorange",
     main = "", xlab = "",
     ylab = "Cyclical ($/gal)",
     ylim = dev_ylim,
     xaxt = "n", yaxt = "n")
axis(1, at = tick_pos, labels = FALSE, tck = -0.04)
# gap.axis = -1 disables R's automatic label-gap suppression, which otherwise
# drops intermediate ticks in small panels and produces asymmetric labels.
axis(2, at = dev_ticks, labels = format(dev_ticks, nsmall = 1), gap.axis = -1)
abline(h = 0, lty = 2, col = "gray55", lwd = 0.9)

# --- Panel 3: Seasonal Component (shared deviation scale) -----------------

plot(dec.df.clean$seasonal,
     type = "l", lwd = 1.4, col = "forestgreen",
     main = "", xlab = "",
     ylab = "Seasonal ($/gal)",
     ylim = dev_ylim,
     xaxt = "n", yaxt = "n")
axis(1, at = tick_pos, labels = FALSE, tck = -0.04)
axis(2, at = dev_ticks, labels = format(dev_ticks, nsmall = 1), gap.axis = -1)
abline(h = 0, lty = 2, col = "gray55", lwd = 0.9)

# --- Panel 4: Irregular Component (shared deviation scale; x-axis labels) -

plot(dec.df.clean$irregular,
     type = "l", lwd = 1.0, col = "firebrick",
     main = "", xlab = "",
     ylab = "Irregular ($/gal)",
     ylim = dev_ylim,
     xaxt = "n", yaxt = "n")
axis(1, at = tick_pos, labels = tick_labels, cex.axis = 0.85)
axis(2, at = dev_ticks, labels = format(dev_ticks, nsmall = 1), gap.axis = -1)
abline(h = 0, lty = 2, col = "gray55", lwd = 0.9)

# --- Shared outer labels and overall chart title --------------------------

mtext("Year",
      side = 1, outer = TRUE, line = 2.3, cex = 0.90)

mtext("Additive Decomposition — RBOB Unleaded Gasoline Futures (RB=F)",
      side = 3, outer = TRUE, line = 3.5, cex = 1.05, font = 2)

mtext("Source: Yahoo Finance / CME-NYMEX  |  Units: USD per Gallon  |  Monthly Closing Price",
      side = 3, outer = TRUE, line = 2.3, cex = 0.78)

mtext("Trend expressed in USD/gal;  Cyclical, Seasonal, and Irregular share the same deviation scale",
      side = 3, outer = TRUE, line = 1.1, cex = 0.75, font = 3)

# --- Reset graphics parameters to R defaults ------------------------------

par(mfrow = c(1, 1),
    mar   = c(5, 4, 4, 2) + 0.1,
    oma   = c(0, 0, 0, 0),
    mgp   = c(3, 1, 0))


# ==============================================================================
# END OF TUTORIAL SCRIPT
# ==============================================================================
# Additional resources:
#   Yahoo Finance (RB=F)   :  https://finance.yahoo.com/quote/RB%3DF/
#   quantmod documentation :  https://www.quantmod.com/
#   R plot() reference     :  https://r-coder.com/plot-r/
# ==============================================================================
