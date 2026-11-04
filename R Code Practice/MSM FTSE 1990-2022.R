library(tseries)
library(MSwM)

#Prices
ftse<-get.hist.quote(instrument="^FTSE", start="1990-01-03", end="2023-01-01", quote="AdjClose",     compression="m")

#Log-returns
ftse.ret<-diff(log(ftse))
plot(ftse.ret)

#Model with only intercept
mod<-lm(ftse.ret ~ 1)

#Fit regime-switching model
mod.mswm=msmFit(mod, k=2, sw=c(T,T), p=0)
plot(mod.mswm)

#Plot probs model1
par(mar=c(3,3,3,3))
plotProb(mod.mswm, which=1)
plotProb(mod.mswm, which=2)

#PlotDiag
par(mar=c(3,3,3,3))
plotDiag(mod.mswm, regime=1, which=1)
plotDiag(mod.mswm, regime=1, which=2)
plotDiag(mod.mswm, regime=1, which=3)

