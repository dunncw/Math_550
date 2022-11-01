library(tidyverse)
library(GGally)
nyc <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/nyc.csv")


library(car)
scatterplotMatrix(~Price+Food+Decor+Service+East, data=nyc)

pairs(~Price+Food+Decor+Service+East, data=nyc, lower.panel=panel.smooth)
lm<-lm(Price~Food+Decor+Service+East,data=nyc)
summary(lm)

# finding points of leverage and outliers 
hatvalues(lm)
high_leverage <- ifelse(hatvalues(lm) > 2*(4+1)/nrow(nyc), 1, 0)

sres<-rstandard(lm)
outlier <- ifelse(abs(sres) > 2, 1, 0)

#plot all of these plots on one graph
par(mfrow=c(2,2))
plot(sres~nyc$Food)
plot(sres~nyc$Decor)
plot(sres~nyc$Service)
plot(sres~nyc$East)

# return to the original plot
par(mfrow=c(1,1))
plot(nyc$Price~fitted(lm))
abline(a=0,b=1)

par(mfrow=c(2,2))
plot(lm)


# new data set 
#valid model

#generated data 1 to support x1=1+0.5x2
#y=(x1+2x2)^2+e
# Fit model y~x1,x2 e with constant variability
quadraticwithconstanterror <- read.delim("chap_6/data/quadraticwithconstanterror.txt")

library(tidyverse)
library(GGally)
library(car)

attach(quadraticwithconstanterror)
scatterplotMatrix(~y+x1+x2, data=quadraticwithconstanterror)

par(mfrow=c(2,2))
lmsg1<-lm(y~x1+x2)
plot(rstandard(lmsg1)~x1)
plot(rstandard(lmsg1)~x2)
plot(rstandard(lmsg1)~fitted(lmsg1))
plot(y~fitted(lmsg1))

#generated data 2 to support x1=1+0.5x2
#y=x1+x2+e
# Fit model y~x1,x2 e increasing variability

nonlinearxincreasingerror <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/nonlinearxincreasingerror.txt")
attach(nonlinearxincreasingerror)

scatterplotMatrix(~y+x1+x2, data=nonlinearxincreasingerror)
# ask yourself do we have a linear relationship between predictors

lmsg2<-lm(y~x1+x2)
par(mfrow=c(2,2))
plot(rstandard(lmsg2)~x1)
plot(rstandard(lmsg2)~x2)
plot(rstandard(lmsg2)~fitted(lmsg2))
plot(y~fitted(lmsg2))
# ask yourself if the predictors have random error so no patter in the x1 x2 plots 
# also the y should be related to y_hat through a smooth function

#if the condition(s) are not satisfied then the standardized residual plots don’t provide direct information 

caution <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/caution.csv")
# in the generated data, E(Y|X)=|x1|/(2+(1.5+x2)^2)=g1(x1)/g2(x2) 
#Note two functions not one
#observations of X1 and X2 were sample from an elliptical distribution 
# note linearity among the predictors is a stronger conditions than 
#elliptical distribution
#errors from normal (0,1)

scatterplotMatrix(~y+x1+x2, data=caution)
cor(caution$x1,caution$x2)
lmg<-lm(caution$y~caution$x1+caution$x2)


sreg<-rstandard(lmg)
plot(sreg~caution$x1)
plot(sreg~caution$x2)
plot(sreg~fitted(lmg)) # 
plot(caution$y ~fitted(lmg)) # response should be related to y_hat through a smooth function


nyc <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/nyc.csv")
scatterplotMatrix(~Price+Food+Decor+Service+East, data=nyc)
attach(nyc)
plot(Price~Food)
plot(Price~Decor)
plot(Price~Service)
plot(Price~East)
# these plots look at the effect of a given predictor on Y , Price, ignoring the effects of the other predictors on Price
# added variable plots
library(car)
#reset figure margins to default
par(mfrow=c(4,4))
avPlots(lm(Price~Food+Decor+Service+East, data=nyc))