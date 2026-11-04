## ADF TEST

## This program performs the ADF unit root test on a financial time series
setwd("D:/TEACHING/Fall 2023/Financial Econometrics/Data/Lecture 4/Apple")
Tdata<- read.csv("AAPLMonthlyPrice.csv", header=TRUE)

## Install the required package
#install.packages("tseries")
library(tseries)

## This section converts the original table into a time series
## to get it ready for the decomposition
Tdata.ts<-ts(Tdata, start=c(2018,6), freq=12)
## This extra line ensures the time series object is in a data frame
## for the purpose of running the ADF test and ACF/PACF
Tdata.ts=as.data.frame(Tdata.ts)

## Create new panel of time series components with separate linear
## trend and cyclical components
## Remove scientific notation
options(scipen=999)

## Divide frame to allow for several graphs within a single graph panel
par(mar=c(3, 5, 2, 2), mfrow= c(1,1) )

## Plot
plot(Tdata$Tvar, type="l")

## ADF on the level series
adf.test(k=0, Tdata.ts$Tvar)

#ACF and PACF
acf(Tdata.ts$Tvar)
pacf(Tdata.ts$Tvar)

