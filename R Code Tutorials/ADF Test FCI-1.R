## ADF TEST

## This program performs the ADF unit root test on a financial time series
setwd("D:/TEACHING/Summer 2023/Financial Econometrics/Data/Lecture 3/Financial Conditions Index")
Tdata<- read.csv("STLFSI4.csv", header=TRUE)

## Install the required package
#install.packages("tseries")
library(tseries)

## This section converts the original table into a time series
## to get it ready for the decomposition
Tdata.ts<-ts(Tdata, start=1994, freq=52)## ACF of euro data
Tdata.ts=as.data.frame(Tdata.ts)

## Plot
plot(Tdata$Tvar, type="l")

## ADF on the level series
adf.test(k=0, Tdata.ts$Tvar)

#ACF and PACF
acf(Tdata.ts$Tvar)
pacf(Tdata.ts$Tvar)
