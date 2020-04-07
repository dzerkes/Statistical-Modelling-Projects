
file1 <- read.table("/home/zerkes/Desktop/DSML - MASTER/1st semester/STATISTICAL MODELLING/solution/mcars1.txt", header=TRUE)
attach(file1)
mod1 <- lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb)
summary(mod1)

plot(file1)

#which selects which plot to be displayed:

#1 = A plot of residuals against fitted values
#2 = A normal Q-Q plot
#3 = A Scale-Location plot of sqrt(| residuals |) against fitted values
#4 = A plot of Cook's distances versus row labels
#5 = A plot of residuals against leverages
#6 = A plot of Cook's distances against leverage/(1-leverage)

#erwtima 1
plot(mod1,which=1,pch=15)
plot(mod1,which=2,pch=15)
plot(cooks.distance(mod1))
plot(hatvalues(mod1))
dfbetas(mod1)
dffits(mod1)

par(mfrow=c(2,2))
plot(mod1)

library(car)
qqPlot(mod1,labels=row.names(file1),id.n =5)
vif(mod1)

mod2 <- lm(mpg~cyl+hp+drat+wt+qsec+vs+am+gear+carb)
vif(mod2)

mod3 <- lm(mpg~hp+drat+wt+qsec+vs+am+gear+carb)
vif(mod3)


#erwtima2
mod_fw   = step(lm(mpg~1),mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, direction = "forward",test="F")


mod_bw   = step(mod1, direction = "backward",test="F")


mod_both = step(lm(mpg~1),mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, direction = "both",test="F")

# Σύγκριση μοντέλων , υπολογισμοί AIC,Rsq,Radj,Rpred or PRESS, Cp mallows


AIC(mod_fw)
AIC(mod_bw)

#finding press. Press has a relationship with Rsq-pred. Minimum press equiv to Maximum Rsq-pred
library(MPV)
PRESS(mod_fw)
PRESS(mod_bw)

#finding Rsq and Rsq-adj
summary(mod_fw)
summary(mod_bw)

#finding cp-mallows
library(olsrr)
ols_mallows_cp(mod_fw, mod1)
ols_mallows_cp(mod_bw, mod1)
#best subsets using leaps
#library(leaps)


#Added variable and Component residual plots
#1st model
avPlots(mod_fw)
crPlots(mod_fw)

#2nd model
avPlots(mod_bw)
crPlots(mod_bw)


#box cox transformation
library(MASS)
bc = boxcox(mod_bw,lambda = seq(-3,3))

# erwtima 3 ~ eksetasi best model
par(mfrow=c(2,2))
mod_bw.inv = lm(log(mpg) ~ wt+cyl+am)
plot(mod_bw.inv)
vif(mod_bw.inv)


# dfbetas -> 2 / sqrt(32) = 0.35
# dffits -> 2 * sqrt(3/32) = 6.5 

dfb <- dfbetas(mod_bw.inv)
print(dfb[dfb > 0.35])

dff <- dffits(mod_bw.inv)
print(dff[dff > 6.5])

plot(cooks.distance(mod_bw.inv))
plot(hatvalues(mod_bw.inv))

