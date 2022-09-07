#if you dont understand a function use help(functionname) in console for a detailed description of function
#read in the text file
Production <- read.delim("/Users/cindydunn/Desktop/Grad_School/math550/production.txt") #read in the text file
#Attach Set of R Objects to Search Path
#Description: The database is attached to the R search path. This means that the database is searched by R when evaluating a variable, so objects in the database can be accessed by simply giving their names.
attach(Production) #attach the data frame

#plot the data
plot(RunSize, RunTime, main="Production Scatterplot",xlab="Size ", ylab="Time", pch=19)

# the review of formula but not necessary because there is predict function in R.
Sxy <- sum((RunSize-mean(RunSize))*(RunTime-mean(RunTime))) #sum of the product of the deviations from the mean
Sxx <- sum((RunSize-mean(RunSize))^2) #sum of the squared deviations from the mean
slope<-Sxy/Sxx# #slope of the regression line
intercept<-mean(RunTime)-slope*mean(RunSize) #intercept of the regression line
s_sqd<-sum((RunTime-(intercept+slope*RunSize))^2)/18 #sum of the squared deviations from the regression line
se_slope=sqrt(s_sqd/sum((RunSize-mean(RunSize))^2)) #standard error of the slope


t_slope<- slope/ se_slope #t-statistic for the slope
pvalue_slope<-2*pt(-t_slope,18) #p-value for the slope
se_intercept=sqrt(s_sqd*(1/20+mean(RunSize)^2/sum((RunSize-mean(RunSize))^2))) #standard error of the intercept
t_intercept=intercept/se_intercept #t-statistic for the intercept
pvalue_intercept<-2*pt(-t_intercept,18) #p-value for the intercept

# for two-tailed test, p-value is calculated by doubling the left tail area.
SXX<-sum((RunSize-mean(RunSize))^2)
# confidence interval for mean of the response when X=50.
pmx<-50-mean(RunSize)
lmtc<-intercept+slope*50-qt(0.975,18)*sqrt(s_sqd)*sqrt(1/20+pmx^2/SXX)
umtc<-intercept+slope*50+qt(0.975,18)*sqrt(s_sqd)*sqrt(1/20+pmx^2/SXX)
lmtp<-intercept+slope*50-qt(0.975,18)*sqrt(s_sqd)*sqrt(1+1/20+pmx^2/SXX)
umtp<-intercept+slope*50+qt(0.975,18)*sqrt(s_sqd)*sqrt(1+1/20+pmx^2/SXX)
# end of the review of formula.
linearMod <- lm(RunTime~ RunSize, data=Production)
abline(linearMod, col="red")
out<-summary(linearMod)
out$coefficients[2,1]
out$coefficients[2,2]
qt(0.975,18)
lm <- out$coefficients[2,1]- qt(0.975,18)* out$coefficients[2,2]
um <- out$coefficients[2,1]+ qt(0.975,18)* out$coefficients[2,2]
lmi <- out$coefficients[1,1]- qt(0.975,18)* out$coefficients[1,2]
umi <- out$coefficients[1,1]+ qt(0.975,18)* out$coefficients[1,2]
confint(linearMod, level=0.95)
new.RunSize <- data.frame(RunSize = c(50, 100, 150))
predict(linearMod,newdata = new.RunSize, interval = "confidence",level=0.95)
predict(linearMod,newdata = new.RunSize, interval = "prediction", level = 0.95)
anova(linearMod)
changeover <- read.delim("/Users/cindydunn/Desktop/Grad_School/math550/changeover_times.txt")
attach(changeover)
linearmodC<-lm(Changeover~New,data=changeover)
summary(linearmodC)
confint(linearmodC)
#if left-tailed test is of interest
pt(-2.254,118,lower.tail=TRUE)
# create indicator variable
Old<-ifelse(Method=='Existing',1,0)
test<-cbind(changeover, Old)