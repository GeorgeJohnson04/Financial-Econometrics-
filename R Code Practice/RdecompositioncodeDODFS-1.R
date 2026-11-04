## TIME SERIES DECOMPOSITION USING R

## This code shows how to do the time series decomposition discussed
## in Lecture 1.

## Get data
setwd("D:/TEACHING/Summer 2023/Financial Econometrics/Data/Lecture 3/Domestic Debt Securities and Loans")
Tdata<- read.csv("DODFS.csv", header=TRUE)

## The head statement shows first six rows of data
head(Tdata)

## Here we ask for summary statistics
summary(Tdata)

## Now we plot the time series
plot.ts(Tdata$Tvar)

## This section converts the original table into a time series
## to get it ready for the decomposition
Tdata.ts<-ts(Tdata$Tvar, start=1976, freq=4)
head(Tdata.ts,20)

## The series is decomposed through the 'decompose' function
dec.Tdata.ts<-decompose(Tdata.ts)

## Here we produce the plot of the components
plot(dec.Tdata.ts)

## And finally here we ask R to show the decomposed data,
## component by component
dec.Tdata.ts

## Identify existing cyclical component
## Start by plotting trend component produced through decomposition function
plot(dec.Tdata.ts$trend)

## Now convert decomposition output to data frame
dec.data=with(dec.Tdata.ts, data.frame(observed=x, trend=trend,
                                                seasonal=seasonal, random=random))

## Check if there are 'NA' missing values
head(dec.data)

## Exclude lines with 'NA' missing values
dec.datanomiss<-dec.data[complete.cases(dec.data), ]

## Create a time variable starting at t=1, just a  time indicator
dec.datanomiss$time <- seq.int(nrow(dec.datanomiss))
head(dec.datanomiss, 10)
                
## Estimate regression to derive pure linear trend and cyclical
## components
cyc.lm <-lm(dec.datanomiss$trend ~ dec.datanomiss$time)
cyc.cyclical <- resid(cyc.lm)
cyc.puretrend <- predict(cyc.lm)
              
## Plot Pure Linear Trend Component
plot(
   dec.datanomiss$time,
    cyc.puretrend,
    ylab = "S&P500",
    xlab = "Time",
    main = "Pure Linear Trend Component",
    type = "l",
    lty = 1
    )
    abline(0, 0)
                
## Plot Cyclical Component
    plot(
    dec.datanomiss$time,
    cyc.cyclical,
    ylab = "Fluctuations Around Pure Linear Trend, i.e. Cyclical",
    xlab = "Time",
    main = "Cyclical Component",
    type = "l",
    lty = 1
    )
    abline(0, 0)

## Create new panel of time series components with separate linear
## trend and cyclical components
    ## Remove scientific notation
    options(scipen=999)
                
    ## Divide frame to allow for several graphs within a single graph panel
    par(mar=c(3, 5, 2, 2), mfrow= c(4,1) )
                
    ## Draw 4 plots
    plot( cyc.puretrend, type = "l",                  
      main = "Additive Decomposition Components",
      xlab = "",
      ylab = "Trend",
      ylim=c(-5000,20000))
    plot( cyc.cyclical, type = "l",                  
      main = "",
      xlab = "",
      ylab = "Cyclical",
      ylim=c(-5000,7000))
    plot( dec.datanomiss$seasonal, type = "l",                  
      main = "",
      xlab = "",
      ylab = "Seasonal",
      ylim=c(-5000,7000))
    plot( dec.datanomiss$random, type = "l",                  
      main = "",
      xlab = "",
      ylab = "Random",
      ylim=c(-5000,7000))

## Important to have the components (at least the seasonal, cyclical, and noise
## components) in the same scale. The trend component is usually in the scale
## of the original series.   