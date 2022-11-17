#best subsets
library(leaps)
bridge <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_7/data/bridge.txt")
logt<-log(bridge$Time,base=exp(1))
logd<-log(bridge$DArea,base=exp(1))
logc<-log(bridge$CCost,base=exp(1))
logl<-log(bridge$Length,base=exp(1))
logs<-log(bridge$Spans,base=exp(1))
logdw<- log(bridge$Dwgs,base=exp(1))

forsub <- data.frame(logt,logd,logc,logdw,logl,logs)
regfit.full = regsubsets(logt ~ ., data = forsub, nvmax = 19)
reg.summary = summary(regfit.full)
reg.summary
# shows the predictors you should choose for a model of that size. 

#for each case report 2 best subsets
regfit.full2= regsubsets(logt ~ ., data = forsub, nvmax = 19,nbest=2)
reg.summary2 = summary(regfit.full2)

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Predictors ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Predictors ", ylab="Adjusted RSq",type="l")
plot(reg.summary$cp ,xlab="Number of Predictors ",ylab="Cp", type='l')
plot(reg.summary$bic ,xlab="Number of Predictors ",ylab="BIC",type='l')

# model with 2 and 3 predicotrs we should use according to reg.summary
lm2<-lm(logt~logdw+logs,data=forsub)
summary(lm2)
lm3<-lm(logt~logdw+logs+logc,data=forsub)
summary(lm3)
# from this we can see that we dont need logc in the model with 3 predictors as its p value is not significant and the adjusted r squared is around the same for both models.

#AIC,BIC, AICc calculation by definition
p<-c(1,2,3,4,5) # num of predictors
v<-45*log(reg.summary$rss/45,base=exp(1))
aic<-v+45+45*log(2*pi, base=exp(1)) +2*(p+2)
bic<-v+45+45*log(2*pi, base=exp(1))+(p+2)*log(45, base=exp(1))
aicc<-aic+2*(p+2)*(p+3)/(45-p-1) # keep the contants just need to define n and p n is number of observatoin

aic
bic
aicc

# > aic
# [1] 34.80693 27.33412 27.29240 29.06413 30.99311
# aic for model with 1 predictors, aic for model with 2 predicts and so on up to number or preditcors

reg.summary$bic - bic

# by definition using AIC, BIC functions
lm1<-lm(logt~logdw)
lm2<-lm(logt~logdw+logs)
lm3<-lm(logt~logc+ logdw+logs)
lm4<-lm(logt~logd+ logc+ logdw+logs)
lm5<- lm(logt~logd+ logc+ logdw+logs+logl)
AICV<-c(AIC(lm1),AIC(lm2),AIC(lm3),AIC(lm4),AIC(lm5))
BICV<- c(BIC(lm1),BIC(lm2),BIC(lm3),BIC(lm4),BIC(lm5))
AICCV<-c(AIC(lm1),AIC(lm2),AIC(lm3),AIC(lm4),AIC(lm5)) +2*(p+2)*(p+3)/(45-p-1)

# by definition using likelihood functions
A1cV<-c(-2*logLik(lm1), -2*logLik(lm2), -2*logLik(lm3), -2*logLik(lm4), -2*logLik(lm5))+ 2*(p+2)
B1cV<- c(-2*logLik(lm1), -2*logLik(lm2), -2*logLik(lm3), -2*logLik(lm4), -2*logLik(lm5))+(p+2)*log(45, base=exp(1))
A1ccV<-A1cV+ 2*(p+2)*(p+3)/(45-p-1)

# advanced fucntion
#backwards selection
fullmodb<-lm(logt~1, data=forsub)
nullmodb<-lm(logt~ logd+logc+logdw+logl+logs,data=forsub)
reg1b<-step(nullmodb,scope=list(lower=fullmodb,upper=nullmodb), direction ="backward")

#forward selection
nullmod<-lm(logt~1,data=forsub)
fullmod<-lm(logt~ logd+logc+logdw+logl+logs,data=forsub)

reg1A<-step(nullmod,scope=list(lower=nullmod, upper=fullmod), direction="forward")
reg1A

#stepwise selection
fullmodb<-lm(logt~1, data=forsub)
nullmodb<-lm(logt~ logd+logc+logdw+logl+logs,data=forsub)
reg1c<-step(fullmodb,scope=list(lower=fullmodb,upper=nullmodb), direction ="both",k=2)


############################################
# For predictive ability
prostateTraining <- read.delim("C:/Teaching@cofc/Math 550/Chapter 7/prostateTraining.txt")
library(tidyverse)
library(GGally)

pairs(~lpsa+lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostateTraining)
lm<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostateTraining)

res<-rstandard(lm)
plot(res~lcavol,data= prostateTraining)
plot(res~lweight,data= prostateTraining)
plot(res~age,data= prostateTraining)
plot(res~lbph,data= prostateTraining)
plot(res~svi,data= prostateTraining)
plot(res~lcp,data= prostateTraining)
plot(res~gleason,data= prostateTraining)
plot(res~pgg45,data= prostateTraining)
plot(res~fitted(lm))
plot(lpsa~fitted(lm),data=prostateTraining)

plot(lm)

###
ibrary(car)
vif(lm)
library(leaps)
attach(prostateTraining)
forsub <- data.frame(lpsa,lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
p<-c(1,2,3,4,5,6,7,8)
regfit.full = regsubsets(lpsa ~ ., data = forsub, nvmax = 16)
reg.summary = summary(regfit.full)
reg.summary$adjr2
reg.summary$bic
par(mfrow=c(1,1))
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
n<-67
v<-n*log(reg.summary$rss/n,base=exp(1))
aic<-v+2*(p+2)+n+n*log(2*pi, base=exp(1))
bic<-v+n+n*log(2*pi, base=exp(1))+(p+2)*log(n, base=exp(1))
aicc<-aic+2*(p+2)*(p+3)/(n-p-1)
