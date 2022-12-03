#set the working directory to the current directory 
setwd(getwd())

#print the working directory
getwd()
#print everything in the working directory
list.files()

# read in data from csv
math_course =read.table("final_project/student/student-mat.csv",sep=";",header=TRUE)
portuguese_course =read.table("final_project/student/student-por.csv",sep=";",header=TRUE)

#combine the two data sets
student_data = rbind(math_course,portuguese_course)

#remove the datasets that are not needed
rm(math_course)
rm(portuguese_course)
# Scale response and predictors with zeros by 1
student_data$G3=(student_data$G3 + 1)
student_data$G2=(student_data$G2 + 1)
student_data$G1=(student_data$G1 + 1)
student_data$absences=(student_data$absences + 1)
student_data$failures=(student_data$failures + 1)
student_data$Medu=(student_data$Medu + 1)
student_data$Fedu=(student_data$Fedu + 1)

# Initial model
attach(student_data)
ilm<-lm(G3 ~., data = student_data)
summary(ilm)
# plot 2 by 2
par(mfrow=c(2,2))
plot(ilm)

library(car)
pc<-powerTransform(cbind(student_data$age, student_data$Medu, student_data$Fedu, student_data$traveltime, student_data$studytime, student_data$failures, student_data$famrel, student_data$freetime, student_data$goout, student_data$Dalc, student_data$Walc, student_data$health, student_data$absences,student_data$G1, student_data$G2, student_data$G3)~1)
summary(pc)

# Transform numeric predictors
tage<-student_data$age^(-1)
tFedu<-student_data$Fedu^(0.67)
ttraveltime<-student_data$traveltime^(-1.66)
tstudytime<-log(student_data$studytime)
tfailures<-student_data$failures^(-6.22)
tfamrel<-student_data$famrel^(2)
tfreetime<-student_data$freetime^(1)
tgoout<-student_data$goout^(1)
tDalc<-student_data$Dalc^(-2.69)
tWalc<-log(student_data$Walc)
thealth<-student_data$health^(1.33)
tabsences<-student_data$absences^(-0.09)
tG1<-student_data$G1^(1.43)
tG2<-student_data$G2^(1.82)
tG3<-student_data$G3^(2)

# Check model with transformed predictors and response
tlm<-lm(tG3~school+sex+tage+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+ttraveltime+tstudytime+tfailures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+tfamrel+tfreetime+tgoout+tDalc+tWalc+thealth+tabsences+tG1+tG2, data = student_data)
abline(tlm)
summary(tlm)
plot(tlm)

# Inverse response method using transformed predictors
transy<-lm(G3~school+sex+tage+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+ttraveltime+tstudytime+tfailures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+tfamrel+tfreetime+tgoout+tDalc+tWalc+thealth+tabsences+tG1+tG2, data = student_data)
lamy<-invResPlot(transy, lambda=c(-1,-1/2,-1/3,-1/4,0,1/4,1/3,1/2,1))
lamy$lambda
plot(lamy$RSS~lamy$lambda)
iG3 <- student_data$G3^(2.019768)

inverse_res_model<-lm(iG3~school+sex+tage+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+ttraveltime+tstudytime+tfailures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+tfamrel+tfreetime+tgoout+tDalc+tWalc+thealth+tabsences+tG1+tG2, data = student_data)
summary(inverse_res_model)
plot(inverse_res_model)

# how many observations are in the dataset
nrow(student_data)

#from here we will try several things to genereate a better model
# 1. boxcox transformation and inverse transformation
# 2. remove outliers
# 3. variable selection in the form of forward selection and backward selection and bestsubset selection

# create a list of all the predictors
predictors = names(student_data)

#plot the first 10 predictors against G3 #a good bit of my predictors are non numerical how should i look at them
library(car)
suppressWarnings(scatterplotMatrix(~G3+studytime+failures+famrel, data=student_data, main="Scatterplot Matrix of Student Data", pch=19, cex=0.5))
suppressWarnings(scatterplotMatrix(~G3+age+Medu+Fedu+traveltime, data=student_data, main="Scatterplot Matrix of Student Data", pch=19, cex=0.5))
suppressWarnings(scatterplotMatrix(~G3+freetime+goout+Dalc, data=student_data, main="Scatterplot Matrix of Student Data", pch=19, cex=0.5))
suppressWarnings(scatterplotMatrix(~G3+Walc+health+absences, data=student_data, main="Scatterplot Matrix of Student Data", pch=19, cex=0.5))
#suppress warnings 
options(warn=-1)
suppressWarnings()

# should i use a box plot to look at non numerical predictors?
boxplot(G3~school, data=student_data, main="Boxplot of G3 by School", xlab="School", ylab="G3")
boxplot(G3~sex, data=student_data)
boxplot(G3~address, data=student_data)
boxplot(G3~famsize, data=student_data)
boxplot(G3~Pstatus, data=student_data)
boxplot(G3~Mjob, data=student_data)
boxplot(G3~Fjob, data=student_data)
boxplot(G3~reason, data=student_data)
boxplot(G3~guardian, data=student_data)
boxplot(G3~schoolsup, data=student_data)
boxplot(G3~famsup, data=student_data)
boxplot(G3~paid, data=student_data)
boxplot(G3~activities, data=student_data)
boxplot(G3~nursery, data=student_data)
boxplot(G3~higher, data=student_data)
boxplot(G3~internet, data=student_data)
boxplot(G3~romantic, data=student_data)
boxplot(G3~higher, data=student_data)
boxplot(G3~higher, data=student_data)
boxplot(G3~higher, data=student_data)

# 3. variable selection in the form of forward selection and backward selection and best subset selection
# we will choose the best model among this variable selection group with the metrics AIC, BIC, and adj R^2

#define a function called aicc to calculate the AICc
aicc_func <- function(model,p){
  AIC(model)+2*(p+2)*(p+3)/(nrow(student_data)-p-1)
}

# forward selection
library(leaps)

nullmod<-lm(G3~1,data=student_data)
fullmod<-lm(G3~.,data=student_data)

forward_AIC_step<-step(nullmod,scope=list(lower=nullmod, upper=fullmod), direction="forward")
Summary(forward_AIC_step)
AIC(forward_AIC_step)
aicc_func(forward_AIC_step, ncol(forward_AIC_step)-1)
BIC(forward_AIC_step)

forward_BIC_step<-step(nullmod,scope=list(lower=nullmod, upper=fullmod), direction="forward", k=log(nrow(student_data)))
Summary(forward_BIC_step)
AIC(forward_BIC_step)
aicc_func(forward_BIC_step, ncol(forward_BIC_step)-1)
BIC(forward_BIC_step)

# backward selection
Backwards_aic_model <- step(student_model)
summary(Backwards_aic_model)
AIC(Backwards_aic_model)
aicc_func(Backwards_aic_model, ncol(Backwards_aic_model)-1)
BIC(Backwards_aic_model)

Backwards_bic_model <- step(student_model, k=log(nrow(student_data)))
summary(Backwards_bic_model)
AIC(Backwards_bic_model)
aicc_func(Backwards_bic_model, ncol(Backwards_bic_model)-1)
BIC(Backwards_bic_model)

# create a table of the models and their metrics
# creeat a vector of the models
models = c("forward_AIC_step","forward_BIC_step","Backwards_aic_model","Backwards_bic_model")
# create a vector of the AICs
AICs = c(AIC(forward_AIC_step),AIC(forward_BIC_step),AIC(Backwards_aic_model),AIC(Backwards_bic_model))
# create a vector of the BICs
BICs = c(BIC(forward_AIC_step),BIC(forward_BIC_step),BIC(Backwards_aic_model),BIC(Backwards_bic_model))
# create the vector of the adj R^2s
adj_R2s = c(summary(forward_AIC_step)$adj.r.squared,summary(forward_BIC_step)$adj.r.squared,summary(Backwards_aic_model)$adj.r.squared,summary(Backwards_bic_model)$adj.r.squared)
#create the data frame
model_metrics = data.frame(models,AICs,BICs,adj_R2s)

summary(forward_AIC_step)
summary(Backwards_aic_model)

summary(forward_BIC_step)
summary(Backwards_bic_model)





# stepwise selection
#import libraries
library(leaps)

#(a) Identify the optimal model or models based on adj R2 , AIC, AIC , BIC from the approach based on all possible subsets.
regfit.full = regsubsets(G3 ~ ., data = student_data, nvmax = 32)
reg.summary = summary(regfit.full)
reg.summary

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Predictors ",ylab="RSS",type="l")
# mark the minimum
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)],col="red",cex=2,pch=20)
plot(reg.summary$adjr2 ,xlab="Number of Predictors ", ylab="Adjusted RSq",type="l")
# mark the maximum
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Predictors ",ylab="Cp", type='l')
# mark the minimum
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Predictors ",ylab="BIC",type='l')
# mark the minimum
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)

which.min(reg.summary$rss)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

#build all subset models from 4 - 12 predictors and compare the metrics
# build best subset model with 4 predictors
coef(regfit.full, 4)
best_subset_4_model <- lm(G3 ~ failures+paid+absences+G2, data = student_data)

# build best subset model with 5 predictors
coef(regfit.full, 5)
best_subset_5_model <- lm(G3 ~ traveltime+failures+paid+absences+G2, data = student_data)

# build best subset model with 6 predictors
coef(regfit.full, 6)
best_subset_6_model <- lm(G3 ~ traveltime+failures+paid+absences+G2+famsup, data = student_data)

# build best subset model with 7 predictors
coef(regfit.full, 7)
best_subset_7_model <- lm(G3 ~ traveltime+failures+paid+absences+G2+famsup+Fjob, data = student_data)

# build best subset model with 8 predictors
best_subset_8_model = lm(G3~Fjob+traveltime+failures+famsup+paid+famrel+absences+G2, data=student_data)

# build best subset model with 9 predictors
coef(regfit.full, 9)
best_subset_9_model = lm(G3~Fjob+traveltime+failures+famsup+paid+famrel+absences+G2+Mjob, data=student_data)

# build best subset model with 10 predictors
coef(regfit.full, 10)
best_subset_10_model = lm(G3~Fjob+traveltime+failures+famsup+paid+famrel+absences+G2+Mjob+activities, data=student_data)

# build best subset model with 11 predictors
coef(regfit.full, 11)
best_subset_11_model = lm(G3~Fjob+traveltime+failures+famsup+paid+famrel+absences+G2+Mjob+activities+Pstatus, data=student_data)

# build the best subset model with 12 predictors
coef(regfit.full, 12)
best_subset_12_model <- lm(G3 ~ Pstatus+Mjob+Fjob+Fjob+traveltime+failures+famsup+paid+activities+famrel+absences+G2 , data = student_data)


#####
#this is wrong currently but this is what i want to do
# create a table with each modesl and its AIC, AICc, BIC, and adjR2
p <- c(4,5,6,7,8,9,10,11,12)
best_subset_models <- list(best_subset_4_model, best_subset_5_model, best_subset_6_model, best_subset_7_model, best_subset_8_model, best_subset_9_model, best_subset_10_model, best_subset_11_model, best_subset_12_model)
best_subset_models_adjR2 <- sapply(best_subset_models, function(x) summary(x)$adj.r.squared)
best_subset_models_names <- c("best_subset_4_model", "best_subset_5_model", "best_subset_6_model", "best_subset_7_model", "best_subset_8_model", "best_subset_9_model", "best_subset_10_model", "best_subset_11_model", "best_subset_12_model")
best_subset_models_AIC <- sapply(best_subset_models, AIC)
#calculate AICc for each model
best_subset_models_AICc <- best_subset_models_AIC + (2*p*(p+1))/(nrow(student_data)-p-1)

best_subset_models_BIC <- sapply(best_subset_models, BIC)

# make a data frame and name the columns 
best_subset_models_df <- data.frame(best_subset_models_names, best_subset_models_AIC, best_subset_models_AICc, best_subset_models_BIC, best_subset_models_adjR2)
# rename the columns
colnames(best_subset_models_df) <- c("Model", "AIC", "AICc", "BIC", "adjR2")

# find the row in the data frame with the highest adjR2 as well as the lowest AIC, AICc, and BIC
best_subset_models_df[which.max(best_subset_models_df$adjR2),]
best_subset_models_df[which.min(best_subset_models_df$AIC),]
best_subset_models_df[which.min(best_subset_models_df$AICc),]
best_subset_models_df[which.min(best_subset_models_df$BIC),]

