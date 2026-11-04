## ADF TEST

## This program performs the ADF unit root test on a financial time series

## Modify the setwd line to match your folder location
setwd("D:/TEACHING/Summer 2023/Financial Econometrics/Data/Lecture 2/Euro Monthly")

## Read files with monthly euro and the change in the monthly euro
monthlyeuro<-read.csv("monthlyeuro.csv", header=TRUE)
dmonthlyeuro<-read.csv("dmonthlyeuro.csv", header=TRUE)

## Install the required package
#install.packages("tseries")
library(tseries)

## Now we plot the time series
plot.ts(monthlyeuro$meuro)
plot.ts(dmonthlyeuro$dmeuro)

## This section converts the original euro level table into a time series
monthlyeuro.ts<-ts(monthlyeuro$meuro)

## ADF on the level series
adf.test(k=0, monthlyeuro.ts)

## This section converts the original euro delta table into a time series
dmonthlyeuro.ts<-ts(dmonthlyeuro$dmeuro)

## ADF on the difference series
adf.test(k=0, dmonthlyeuro.ts)

