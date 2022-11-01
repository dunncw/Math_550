# you are missing some code here she hosuld have updated her code on oaks 
res_y_others <- residuals()

###

#transformation-Inverse response plots
library(tidyverse)
library(GGally)
library(car)

defects <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/defects.txt")
scatterplotMatrix (~Defective+Temperature+Density+Rate,data=defects)
# you want to have linear relationship between predictors and response
# this means you want the dashes line to overlap with the soild line

lmm<-lm(Defective~ Temperature+Density+Rate, data=defects)
restd<-rstandard(lmm)
#put all plots on one page
par(mfrow=c(2,2))
# for these you want a horizontal band of random points with no clear pattern between them
plot(restd~Temperature,data=defects)
plot(restd~Density,data=defects)
plot(restd~Rate,data=defects)

# if y_hat is a function of y then we can use inverse reposnes plot (to be a function you need to pass vertical line test)
par(mfrow=c(1,1))
plot(defects$Defective~fitted(lmm))

#explanation of inverse response plot
attach(defects)
newdata = data.frame(Temperature = Temperature, Density = Density, Rate= Rate )
predicty <- predict(lmm,newdata) #this is y_hat

library(alr3)

lam<-invResPlot(lmm, lambda=c(-1,-1/2,-1/3,-1/4,0,1/4,1/3,1/2,1))

lam$lambda
par(mfrow=c(1,1))
plot(lam$RSS~lam$lambda)
# the lambda that minimizes the RSS is the one that minimizes the sum of the squares of the residuals
# get the smallest lambda
lam$lambda[which.min(lam$RSS)]

#we found best lamda now transform the response
transd<-sqrt(defects$Defective)
tlmm<-lm(transd~ Temperature+Density+Rate, data=defects)
trestd<-rstandard(tlmm)

#put all plots on one page
par(mfrow=c(2,2))
plot(trestd~Temperature,data=defects)
plot(trestd~Density,data=defects)
plot(trestd~Rate,data=defects)
plot(transd~fitted(tlmm))

#Box-Cox method 
install.packages("psych")
library("psych")  
#transformation-Box-cox method
library(faraway)

library(MASS)

bc<-boxcox(lmm, lambda=seq(-3,3,1/10))

bestlambda<-bc$x[which(bc$y==max(bc$y))]
ndefective<-sqrt(defects$Defective)
lmn<-lm(ndefective~Temperature+Density+Rate, data=defects)
restdn<-rstandard(lmn)
#put all plots on one page
par(mfrow=c(2,2))
plot(restdn~Temperature,data=defects)
plot(restdn~Density,data=defects)
plot(restdn~Rate,data=defects)
plot(restdn~fitted(lmn))


plot(lmn)
summary(lmn)
avPlots(lm(ndefective~Temperature+Density+Rate, data=defects))

# here we will do power transformation which is when u want to transform the predictor and response
magazines <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/magazines.csv")
scatterplotMatrix (~AdRevenue + AdPages + SubRevenue + NewsRevenue,data= magazines)

# power transformation to apply box cox to predictors
pc<-powerTransform(cbind(magazines$AdPages, magazines$SubRevenue, magazines$NewsRevenue)~1)
summary(pc)
# rnd pwr is 0 so take log of predictors
library(alr3)
logap<-log(magazines$AdPages ,base=exp(1))
logsr<-log(magazines$SubRevenue,base=exp(1))
lognr<-log(magazines$NewsRevenue,base=exp(1))
scatterplotMatrix (~logap + logsr + lognr)

# now lets remake model with log predictors and look at invrese response plots
lmag<-lm(AdRevenue~ logap+ logsr+lognr, data=magazines)
plot(magazines$ AdRevenue~fitted(lmag))
lam<-invResPlot(lmag, lambda=c(-1,-1/2,-1/3,-1/4,0,1/4,1/3,1/2,1))
lam$lambda
plot(lam$RSS~lam$lambda)

# okay so we have a best lambda of .23 which is closest to .25 so we will use 4th root 
# she had not updated code after this point so we will need to wait for that then copy her code

fourthroot<-magazines$AdRevenue^(1/4)
lmag<-lm(fourthroot~ logap+ logsr+lognr)
res_forthroot<-residuals(lmag)
#put all plots on one page
par(mfrow=c(2,2))
plot(res_forthroot~logap)
plot(res_forthroot~logsr)
plot(res_forthroot~lognr)

# check on rediduals plot
plot(lmag)

# this part of code is not finished as she did not upload updated code 
pca<-powerTransform(cbind(magazines$AdRevenue, magazines$AdPages,  magazines$SubRevenue, magazines$NewsRevenue)~1)
summary(pca)
logar<- log(magazines$AdRevenue,base=exp(1))
res_log <- resta


# here are two examples to take note of 
#########
########
# example 1
#for multicollinearity
bridge <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_6/data/bridge.txt")
scatterplotMatrix (~Time+DArea+CCost+Dwgs+Length+Spans,data=bridge)
attach(bridge)
library(car)
pca<-powerTransform(cbind(bridge$Time, bridge$DArea,  bridge$CCost, bridge$Dwgs, bridge$Length, bridge$Spans)~1)
summary(pca)

logt<-log(bridge$Time,base=exp(1))
logd<-log(bridge$DArea,base=exp(1))
logc<-log(bridge$CCost,base=exp(1))
logl<-log(bridge$Length,base=exp(1))
logs<-log(bridge$Spans,base=exp(1))
logdw<- log(bridge$Dwgs,base=exp(1))

scatterplotMatrix (~logt+logd+logc+logl+logs)
log<-lm(logt~logd+logc+logdw+logl+logs)

reslog<-rstandard(log)
plot(reslog~logt)
plot(reslog~logd)
plot(reslog~logc)
plot(reslog~logl)
plot(reslog~logs)
plot(reslog~logdw)
plot(logt~fitted(log))
abline(0,1)
plot(log)

summary(log)
# so we have a model that is effect but none of the the predictors are useful this could be because the predictors are highly correlated 

cor(logd,logc)
cor(logd,logl)
cor(logd,logs)
cor(logd,logdw)
cor(logc,logl)
cor(logc,logs)
cor(logc,logdw)
cor(logs,logdw)
avPlots(log)

# we can do this with advanced fucntiosn vif 
# here is long from of what vif does for each predictor
lmd<-lm(logd~ logc+logdw+logl+logs)
out<-summary(lmd)
1/(1-out$r.squared)
#vif does it one function
vif(log)

#vif(log)


# last exasmple of forced correlation
storks <- read.delim("C:/Teaching@cofc/Math 550/Chapter 6/storks.txt")
lm<-lm(Babies~Storks,data=storks)
summary(lm)
attach(storks)
plot(Babies~Storks)
abline(lm)

lmw<-lm(Babies~Women,data=storks)
plot(Babies~Women)
abline(lmw)
summary(lmw)