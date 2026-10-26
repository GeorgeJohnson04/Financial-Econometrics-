# ==============================================================================
# LOG-LINEAR REGRESSION IN R
# Financial Econometrics — Lecture 2
# ==============================================================================
#
# This tutorial demonstrates how to run a log-linear regression of a financial
# time series on a time trend, interpret the results, and visualize the fit.
# The data are downloaded automatically from Yahoo Finance — no CSV file is
# required.
#
# SERIES    : ^GSPC — S&P 500 Index (end-of-quarter closing price)
# SOURCE    : Yahoo Finance via the quantmod package
# UNITS     : Index points (price level, not returns)
# FREQUENCY : Daily (aggregated to quarterly end-of-period closes here)
# COVERAGE  : Full history available from 1928; we analyze 1968–2017 to match
#             the sample period used in the companion CSV-based version of this
#             tutorial
#
# WHY THE S&P 500?
#   The S&P 500 is a purely financial series — it reflects the market-clearing
#   price of a diversified portfolio of equity claims, set in real time by
#   buyers and sellers in financial markets.  Unlike GDP or employment, it is
#   not a macroeconomic aggregate; it is a financial asset price.
#
#   Over long horizons, equity prices grow roughly exponentially, meaning a
#   log transformation of the index linearizes the trend.  This makes the
#   S&P 500 an ideal teaching example for a log-linear regression of the form:
#
#       log(Price_t)  =  α  +  β · Year_t  +  ε_t
#
#   where β approximates the average annual continuously-compounded return
#   over the sample period.
#
# DATA STRUCTURE
#   The original version of this tutorial used a CSV file organized as a
#   wide table: one row per year, four columns for the four quarterly
#   observations (q1, q2, q3, q4).  We replicate that structure here by:
#     1. Downloading daily S&P 500 data from Yahoo Finance
#     2. Extracting the last closing price of each calendar quarter
#     3. Reshaping the result into the same year / q1 / q2 / q3 / q4 layout
#   From that point forward, all code steps mirror the original tutorial.
# ==============================================================================


# ==============================================================================
# SECTION 0 — PACKAGE SETUP
# ==============================================================================

# quantmod  : downloads financial data from Yahoo Finance and provides
#             helper functions for time series manipulation
# ggplot2   : produces publication-quality plots with a concise syntax
#
# Install once if needed:
#   install.packages(c("quantmod", "ggplot2"))

library(quantmod)
library(ggplot2)


# ==============================================================================
# SECTION 1 — DOWNLOAD S&P 500 DATA FROM YAHOO FINANCE
# ==============================================================================

# Download the full daily S&P 500 history.
# auto.assign = FALSE avoids the automatic naming step (the "^" in "^GSPC"
# is not a legal R variable name character, which can cause errors).

sp500_xts <- getSymbols("^GSPC",
                         src         = "yahoo",
                         auto.assign = FALSE)

cat("─────────────────────────────────────────────────────────\n")
cat("Series         : ^GSPC  (S&P 500 Index)\n")
cat("Daily obs.     :", nrow(sp500_xts), "trading days downloaded\n")
cat("Full period    :", format(start(sp500_xts)),
    "to", format(end(sp500_xts)), "\n")
cat("─────────────────────────────────────────────────────────\n\n")

# Restrict to the 1968–2017 study window to replicate the companion
# CSV-based tutorial's sample period
sp500_xts <- window(sp500_xts,
                    start = "1968-01-01",
                    end   = "2017-12-31")

cat("Study window   : 1968-01-01  to  2017-12-31\n")
cat("Obs. in window :", nrow(sp500_xts), "trading days\n\n")


# ==============================================================================
# SECTION 2 — AGGREGATE TO QUARTERLY END-OF-PERIOD CLOSING PRICES
# ==============================================================================

# to.quarterly() collapses daily OHLCV data to a quarterly frequency.
# Cl() extracts only the closing price column.
# indexAt = "lastof" places each observation at the last calendar day of
# the quarter (e.g., March 31, June 30, September 30, December 31).
# OHLC = FALSE returns a single-column series (close only) rather than
# a four-column OHLC bar.

sp500_qtr <- to.quarterly(Cl(sp500_xts),
                           indexAt = "lastof",
                           OHLC    = FALSE)

cat("Quarterly observations:", nrow(sp500_qtr), "\n")
head(sp500_qtr)
tail(sp500_qtr)


# ==============================================================================
# SECTION 3 — RESHAPE INTO WIDE FORMAT  (year / q1 / q2 / q3 / q4)
# ==============================================================================

# The original tutorial uses a CSV arranged as a wide table — one row per
# year with one column for each of the four quarterly price observations.
# We replicate that layout here so all subsequent code steps are identical.

# --- 3a. Build a plain data frame with year and quarter columns -----------

sp500_df <- data.frame(
  date  = as.Date(index(sp500_qtr)),         # calendar date of each quarter end
  price = as.numeric(coredata(sp500_qtr)),    # closing index level
  stringsAsFactors = FALSE
)

# Derive year and quarter numerically from the date.
# quarters() returns character strings ("Q1", "Q2", ...) not integers, so
# we instead extract the month number and divide by 3 to get 1, 2, 3, or 4.
sp500_df$year    <- as.integer(format(sp500_df$date, "%Y"))
sp500_df$quarter <- ceiling(as.integer(format(sp500_df$date, "%m")) / 3)

# Drop any rows where year or quarter is NA before reshaping
sp500_df <- sp500_df[complete.cases(sp500_df[ , c("year", "quarter")]), ]

# Verify the structure before reshaping
head(sp500_df, 8)

# --- 3b. Pivot from long to wide format ------------------------------------

# reshape() converts a long data frame (one row per year-quarter) into a
# wide data frame (one row per year, one column per quarter).
mydata <- reshape(sp500_df[ , c("year", "quarter", "price")],
                  idvar     = "year",
                  timevar   = "quarter",
                  direction = "wide")

# Rename the auto-generated column names to match the original CSV layout
colnames(mydata) <- c("year", "q1", "q2", "q3", "q4")

# Sort by year (reshape does not guarantee row order)
mydata <- mydata[order(mydata$year), ]
rownames(mydata) <- NULL    # reset row indices after sorting

# Inspect the final wide table — should look identical in structure to the
# original CSV, with one row per year and columns year, q1, q2, q3, q4
cat("\n── Wide-format quarterly S&P 500 closing prices ──────────────\n")
head(mydata, 10)
cat("...\n")
tail(mydata, 10)

cat("\nDimensions:", nrow(mydata), "years ×", ncol(mydata), "columns\n")
summary(mydata)


# ==============================================================================
# SECTION 4 — LOG TRANSFORMATION AND INITIAL PLOT
# ==============================================================================

# Because equity prices grow roughly exponentially over long horizons, taking
# the natural log linearizes the series, making a linear time trend regression
# appropriate.  The slope coefficient β will approximate the average annual
# continuously-compounded return of the index.

mydata$logq1 <- log(mydata$q1)

# Plot log(Q1 closing price) against year — should look close to linear
plot(mydata$year, mydata$logq1,
     main = "Log of S&P 500 Q1 Closing Price vs. Year  (1968–2017)",
     xlab = "Year",
     ylab = "log(S&P 500 Q1 Close)",
     pch  = 16,
     col  = "steelblue",
     cex  = 0.85)


# ==============================================================================
# SECTION 5 — LOG-LINEAR REGRESSION OF Q1 PRICE ON TIME
# ==============================================================================

# Model:  log(Q1_price_t)  =  α  +  β · Year_t  +  ε_t
#
# Interpretation of β:
#   A one-year increase in the time index is associated with a β-unit increase
#   in the log price, i.e., approximately a 100·β percent increase in the
#   index level per year on average — the estimated long-run annual return.

reg.mydata <- lm(logq1 ~ year, data = mydata)

cat("\n── OLS Regression: log(S&P 500 Q1 Close) ~ Year ─────────────\n")
summary(reg.mydata)


# ==============================================================================
# SECTION 6 — EXTRACT AND INSPECT REGRESSION OUTPUTS
# ==============================================================================

# Save the regression coefficients, residuals, and fitted values to separate
# data frames so each component can be examined individually.

coefout  <- data.frame(coef(reg.mydata))
residout <- data.frame(resid(reg.mydata))
predout  <- data.frame(predict(reg.mydata))

# Display each component
cat("\n── Regression Coefficients ───────────────────────────────────\n")
coefout

cat("\n── Residuals (first 10 rows) ─────────────────────────────────\n")
head(residout, 10)

cat("\n── Fitted Values (first 10 rows) ─────────────────────────────\n")
head(predout, 10)


# ==============================================================================
# SECTION 7 — COMBINE AND EXPORT REGRESSION RESULTS
# ==============================================================================

# Merge residuals and fitted values into a single data frame for export
exportout <- data.frame(residout, predout)

cat("\n── Combined Export File (first 10 rows) ──────────────────────\n")
head(exportout, 10)

# Export to CSV.
# Update the path below to match the directory where you want to save the file.
# Example (Windows) : "C:/Users/YourName/Documents/regout.csv"
# Example (Mac/Linux): "~/Documents/regout.csv"
write.csv(exportout, file = "regout_sp500.csv", row.names = TRUE)
cat("\nRegression output saved to: regout_sp500.csv\n")

# Add the year column so the exported file is self-contained and can be
# opened in Excel or merged with other datasets without ambiguity
year <- data.frame(mydata["year"])

exportout.wyear <- data.frame(year, residout, predout)

cat("\n── Export File with Year Column (first 10 rows) ──────────────\n")
head(exportout.wyear, 10)

# Combine everything: original data plus residuals and fitted values
# in a single master data frame for analysis and plotting
mydata.woutputs <- data.frame(mydata, residout, predout)

cat("\n── Full Combined Data Frame (first 10 rows) ───────────────────\n")
head(mydata.woutputs, 10)


# ==============================================================================
# SECTION 8 — VISUALIZE ACTUAL VS. FITTED VALUES
# ==============================================================================

# --- 8a. Base R plot() -------------------------------------------------------

# The first call plots the actual log(Q1 price) as points.
# The second call overlays the fitted regression line in red.
# Viewing both together shows how well the log-linear model tracks the
# long-run trajectory of the index.

plot(mydata$year, mydata$logq1,
     main = "S&P 500 Q1 Close — Actual vs. Fitted Log-Linear Trend",
     xlab = "Year",
     ylab = "log(S&P 500 Q1 Close)  —  Actual (blue) and Fitted (red)",
     pch  = 16,
     col  = "blue",
     cex  = 0.85)

lines(mydata$year, mydata.woutputs$predict.reg.mydata.,
      col = "red",
      lwd = 2)

legend("topleft",
       legend = c("Actual log(Q1 close)", "Fitted trend line"),
       col    = c("blue", "red"),
       pch    = c(16, NA),
       lty    = c(NA, 1),
       lwd    = c(NA, 2),
       bty    = "n",
       cex    = 0.85)

# --- 8b. ggplot2 plot --------------------------------------------------------

# ggplot2 produces the same information with a cleaner default appearance
# and a more concise syntax for adding multiple layers.

# Rename the fitted-values column to something shorter for readability
# inside the ggplot call (the auto-generated name is verbose)
mydata.woutputs$fitted <- mydata.woutputs$predict.reg.mydata.

ggplot(data = mydata.woutputs, aes(x = year)) +

  geom_line(aes(y = logq1,  color = "Actual"),
            linewidth = 0.9) +

  geom_line(aes(y = fitted, color = "Fitted trend"),
            linewidth = 0.9, linetype = "dashed") +

  scale_color_manual(
    name   = NULL,
    values = c("Actual" = "steelblue", "Fitted trend" = "firebrick")
  ) +

  labs(
    title    = "S&P 500 Q1 Closing Price — Log-Linear Trend Regression",
    subtitle = paste0("log(Price) = α + β · Year  |  β ≈ ",
                      round(coef(reg.mydata)["year"], 4),
                      "  (~", round(coef(reg.mydata)["year"] * 100, 2),
                      "% avg. annual return)"),
    x        = "Year",
    y        = "log(S&P 500 Q1 Close)"
  ) +

  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray35"),
    legend.position = "bottom"
  )


# ==============================================================================
# END OF TUTORIAL SCRIPT
# ==============================================================================
# Additional resources:
#   S&P 500 (Yahoo Finance)  :  https://finance.yahoo.com/quote/%5EGSPC/
#   quantmod documentation   :  https://www.quantmod.com/
#   ggplot2 documentation    :  https://ggplot2.tidyverse.org/
#   R reshape() reference    :  https://stat.ethz.ch/R-manual/R-devel/library/stats/html/reshape.html
# ==============================================================================
