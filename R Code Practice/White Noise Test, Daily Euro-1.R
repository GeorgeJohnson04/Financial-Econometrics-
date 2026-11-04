## WHITE NOISE TEST

## This program performs the Ljung-Box white noise test on a financial time series

## Modify the setwd line to match your folder location
setwd("D:/TEACHING/Summer 2023/Financial Econometrics/Data/Lecture 2/Euro Daily")

## Read files with daily euro and the change in the daily euro
dailyeuro<- read.csv("dailyeuro.csv", header=TRUE)
ddailyeuro<-read.csv("ddailyeuro.csv", header=TRUE)

## Now we plot the time series
plot.ts(dailyeuro$euro)
plot.ts(ddailyeuro$deuro)

## This section converts the original table into a time series
dailyeuro.ts<-ts(dailyeuro$euro)

## Ljung test (or Box-Ljung test) on the level and difference series
Box.test(dailyeuro.ts, type="Ljung", lag=6)

## This section converts the original table into a time series
ddailyeuro.ts<-ts(ddailyeuro$deuro)

## Ljung test (or Box-Ljung test) on the level and difference series
Box.test(ddailyeuro.ts, type="Ljung", lag=6)
