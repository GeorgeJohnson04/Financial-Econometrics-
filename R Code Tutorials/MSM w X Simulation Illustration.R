#install.packages("knitr")
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("dplyr")
library(dplyr)

theta_v <- data.frame(t(c(-2.00,2.00,2.00,1.00,0.85,0.95)))
names(theta_v) <- c("$\\mu_1$","$\\mu_2$","$\\sigma_1$","$\\sigma_2$","$p_{11}$","$p_{22}$")
kable(theta_v, "html", booktabs = F,escape = F) %>%
  kable_styling(position = "center")

p11 <- theta_v[1,5] 
p22 <- theta_v[1,6]
P <- matrix(c(p11,1-p22,1-p11,p22),2,2)
P[1,]

set.seed(13)
T_end <- 10^2

s0 <- 1 
st <- function(i) sample(1:2,1,prob = P[i,])

s <- st(s0)
for(t in 2:T_end) {
  s <- c(s,st(s[t-1]))
}
plot(s, pch = 20,cex = 0.5, ylim=c(1,2))

s
P_stat <- Reduce(function(M1,M2) M1%*%M2 ,lapply(1:100,function(x) P ))
P_stat[1,]

mean(s==1)

set.seed(11)
x1 <- rnorm(T_end,theta_v[1,1],theta_v[1,3])
set.seed(17)
x2 <- rnorm(T_end,theta_v[1,2],theta_v[1,4])

t_index <- 1:T_end
x <- rep(0,T_end)
x[s==1] <- x1[s==1]
x[s==2] <- x2[s==2]
plot(x~t_index, pch = 20)
points(x[s == 2]~t_index[s==2],col = 2)


library(MSwM)

#Model with only intercept
mod<-lm(x ~ 1)
mod

#Fit regime-switching model
mod.mswm=msmFit(mod, k=2, sw=c(T,T), p=0)
plot(mod.mswm)

#Plot probs model1
par(mar=c(3,3,3,3))
plotProb(mod.mswm, which=1)
plotProb(mod.mswm, which=2)

