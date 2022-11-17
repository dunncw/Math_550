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

# remove the column "G1" and "G2" from the dataset
student_data = student_data[, -which(names(student_data) %in% c("G1"))]

# Create a naive baseline model to comapre too
student_model = lm(G3~.,data=student_data)
summary(student_model)

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
  AIC(model)+2*(p+2)*(p+3)/(nrow(pga_data)-p-1)
}

# forward selection
library(leaps)

nullmod<-lm(G3~1,data=student_data)
fullmod<-lm(G3~.,data=student_data)

forward_step<-step(nullmod,scope=list(lower=nullmod, upper=fullmod), direction="forward")
summary(forward_step)

AIC(forward_step)
BIC(forward_step)
aicc_func(forward_step, length(forward_step$coefficients))

# backward selection
backwardsstep_model <- step(student_model)
summary(backwardsstep_model)

AIC(backwardsstep_model)
BIC(backwardsstep_model)
aicc_func(backwardsstep_model, length(backwardsstep_model$coefficients))

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

#  for best subset selection grab the predictors from model with 8 predictors in the object "coef(regfit.full, 8)" 
coef(regfit.full, 8)
# build a model with these predictors
best_subset_model = lm(G3~Fjob+traveltime+failures+famsup+paid+famrel+absences+G2, data=student_data)
summary(best_subset_model)

AIC(best_subset_model)
BIC(best_subset_model)
aicc_func(best_subset_model, length(best_subset_model$coefficients))