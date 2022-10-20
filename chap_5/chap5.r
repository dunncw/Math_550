# read it data csv
nyc <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_5/nyc.csv")

lmf<-lm(Price~Food+Decor+Service+East, data=nyc)
summary(lmf)
#  Food = customer rating of the food (out of 30)
# Décor = customer rating of the decor (out of 30)
# Service = customer rating of the service (out of 30)

lmr<-lm(Price~Food+Decor+East, data=nyc)
summary(lmr)

# this is advanced functions to chose whether u would use full or reduced model
anova(lmr,lmf)

nyc <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_5/nyc.csv")
attach(nyc)
Foodi<-Food*East
Decori<-Decor*East
Servicei<-Service*East

Lmi<-lm(Price~Food+Decor+Service+East+Foodi+Decori+Servicei, data=nyc) # full model
Lmwi<-lm(Price~Food+Decor+Service+East, data=nyc) # without interaction model # 4 predictors 
Lm<-lm(Price~Food+Decor+East, data=nyc) # 3 predictors

anova(Lm,Lmi) #most reduced and full model comparison
#h0 = beta3=beta5=beta6=beta7=0
#h1 = at least one of the beta is not equal to 0

#large p value fail to reject h0

travel <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chap_5/travel.txt")
attach(travel)

AgeC<-Age*C
LmA<-lm(Amount~Age, data=travel) # i think reduced model
LmANCO<-lm(Amount~Age+C+AgeC, data=travel) # full model

anova(LmA, LmANCO) # reject h0 so we cant use reduced model cuz full models p value is so sigifigat(small) that its predictors have a huge impact on model and need to be kept

