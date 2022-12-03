math_course = read.table("~/student-mat.csv",sep=";",header=TRUE)
portuguese_course = read.table("~/student-por.csv",sep=";",header=TRUE)
#combine the two data sets
student_data = rbind(math_course,portuguese_course)
#remove the datasets that are not needed
rm(math_course)
rm(portuguese_course)

# Remove G1 and G2
student_data = student_data[, -32]
student_data = student_data[, -31]
# Scale response and predictors with zeros by 1
student_data$G3=(student_data$G3 + 1)
student_data$absences=(student_data$absences + 1)
student_data$failures=(student_data$failures + 1)
student_data$Medu=(student_data$Medu + 1)
student_data$Fedu=(student_data$Fedu + 1)
View(student_data)

# Initial model
attach(student_data)
ilm<-lm(G3 ~., data = student_data)
summary(ilm)
plot(ilm)
# plots indicate bad model

# plot(student_data$school, student_data$G3, main="Final Grade vs School", xlab="school", ylab="G3")
# plot(student_data$sex, student_data$G3, main="Final Grade vs Sex", xlab="sex", ylab="G3")
# plot(student_data$age, student_data$G3, main="Final Grade vs Age", xlab="age", ylab="G3")
# plot(student_data$address, student_data$G3, main="Final Grade vs Address", xlab="address", ylab="G3")
# plot(student_data$famsize, student_data$G3, main="Final Grade vs Family Size", xlab="famsize", ylab="G3")
# plot(student_data$Pstatus, student_data$G3, main="Final Grade vs Pstatus", xlab="pstatus", ylab="G3")
# plot(student_data$Medu, student_data$G3, main="Final Grade vs Medu", xlab="Medu", ylab="G3")
# plot(student_data$Fedu, student_data$G3, main="Final Grade vs Fedu", xlab="Fedu", ylab="G3")
# plot(student_data$Mjob, student_data$G3, main="Final Grade vs Mjob", xlab="Mjob", ylab="G3")
# plot(student_data$Fjob, student_data$G3, main="Final Grade vs Fjob", xlab="Fjob", ylab="G3")
# plot(student_data$reason, student_data$G3, main="Final Grade vs Reason", xlab="reason", ylab="G3")
# plot(student_data$guardian, student_data$G3, main="Final Grade vs Guardian", xlab="guardian", ylab="G3")
# plot(student_data$traveltime, student_data$G3, main="Final Grade vs Traveltime", xlab="traveltime", ylab="G3")
# plot(student_data$studytime, student_data$G3, main="Final Grade vs Studytime", xlab="studytime", ylab="G3")
# plot(student_data$failures, student_data$G3, main="Final Grade vs Failures", xlab="failures", ylab="G3")
# plot(student_data$schoolsup, student_data$G3, main="Final Grade vs Schoolsup", xlab="schoolsup", ylab="G3")
# plot(student_data$famsup, student_data$G3, main="Final Grade vs Famsup", xlab="famsup", ylab="G3")
# plot(student_data$paid, student_data$G3, main="Final Grade vs Paid", xlab="paid", ylab="G3")
# plot(student_data$activities, student_data$G3, main="Final Grade vs Activities", xlab="activities", ylab="G3")
# plot(student_data$nursery, student_data$G3, main="Final Grade vs Nursery", xlab="nursery", ylab="G3")
# plot(student_data$higher, student_data$G3, main="Final Grade vs Higher", xlab="higher", ylab="G3")
# plot(student_data$internet, student_data$G3, main="Final Grade vs Internet", xlab="internet", ylab="G3")
# plot(student_data$romantic, student_data$G3, main="Final Grade vs Romantic", xlab="romantic", ylab="G3")
# plot(student_data$famrel, student_data$G3, main="Final Grade vs Famrel", xlab="famrel", ylab="G3")
# plot(student_data$freetime, student_data$G3, main="Final Grade vs Freetime", xlab="freetime", ylab="G3")
# plot(student_data$goout, student_data$G3, main="Final Grade vs Goout", xlab="goout", ylab="G3")
# plot(student_data$Dalc, student_data$G3, main="Final Grade vs Dalc", xlab="Dalc", ylab="G3")
# plot(student_data$Walc, student_data$G3, main="Final Grade vs Walc", xlab="Walc", ylab="G3")
# plot(student_data$health, student_data$G3, main="Final Grade vs Health", xlab="health", ylab="G3")
# plot(student_data$absences, student_data$G3, main="Final Grade vs Absences", xlab="absences", ylab="G3")

#abline(ilm)


# Check normality of numeric predictors
dage<-density(student_data$age)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="age"  )

dage<-density(student_data$Medu)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$Fedu)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$traveltime)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$studytime)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$failures)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$famrel)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$freetime)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$goout)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$Dalc)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$Walc)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$health)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )

dage<-density(student_data$absences)
plot(dage)
qqnorm(student_data$age, pch = 1, frame = FALSE)
qqline(student_data$age, col = "steelblue", lwd = 2)
boxplot(student_data$age, data=student_data, main="data",ylab="G3"  )



# Apply Box-Cox method on response and numeric predictors
library(car)
pc<-powerTransform(cbind(student_data$age, student_data$Medu, student_data$Fedu, student_data$traveltime, student_data$studytime, student_data$failures, student_data$famrel, student_data$freetime, student_data$goout, student_data$Dalc, student_data$Walc, student_data$health, student_data$absences, student_data$G3)~1)
summary(pc)

# Transform numeric predictors
tage<-student_data$age^(-1)
tMedu<-student_data$Medu^(1)
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
tabsences<-student_data$absences^(-0.08)
tG3<-student_data$G3^(1.5)

# Check model with transformed predictors and response
tlm<-lm(tG3~school+sex+tage+address+famsize+Pstatus+tMedu+Fedu+Mjob+Fjob+reason+guardian+ttraveltime+tstudytime+tfailures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+tfamrel+tfreetime+tgoout+tDalc+tWalc+thealth+tabsences)
abline(tlm)
summary(tlm)
plot(tlm)

lamy<-invResPlot(tlm, lambda=c(-1,-1/2,-1/3,-1/4,0,1/4,1/3,1/2,1))
lamy$lambda
plot(lamy$RSS~lamy$lambda)

# Inverse response method using original/nontransformed predictors
transyy<-lm(age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences~tG3)
lamyy<-invResPlot(transyy, lambda=c(-1,-1/2,-1/3,-1/4,0,1/4,1/3,1/2,1))
lamyy$lambda
plot(lamyy$RSS~lamyy$lambda)

# Check normality of transformed numeric predictors
dtage<-density(tage)
plot(dtage)
qqnorm(tage, pch = 1, frame = FALSE)
qqline(tage, col = "steelblue", lwd = 2)
boxplot(tage, data=student_data, main="data",ylab="transformed age")

dtMedu<-density(tMedu)
plot(dtMedu)
qqnorm(tMedu, pch = 1, frame = FALSE)
qqline(tMedu, col = "steelblue", lwd = 2)
boxplot(tMedu, data=student_data, main="data",ylab="transformed Medu")

dtFedu<-density(tFedu)
plot(dtFedu)
qqnorm(tFedu, pch = 1, frame = FALSE)
qqline(tFedu, col = "steelblue", lwd = 2)
boxplot(tFedu, data=student_data, main="data",ylab="transformed Fedu"  )

dttraveltime<-density(ttraveltime)
plot(dttraveltime)
qqnorm(ttraveltime, pch = 1, frame = FALSE)
qqline(ttraveltime, col = "steelblue", lwd = 2)
boxplot(ttraveltime, data=student_data, main="data",ylab="transformed traveltime"  )

dtstudytime<-density(tstudytime)
plot(dtstudytime)
qqnorm(tstudytime, pch = 1, frame = FALSE)
qqline(tstudytime, col = "steelblue", lwd = 2)
boxplot(tstudytime, data=student_data, main="data",ylab="transformed studytime"  )

dtfailures<-density(tfailures)
plot(dtfailures)
qqnorm(tfailures, pch = 1, frame = FALSE)
qqline(tfailures, col = "steelblue", lwd = 2)
boxplot(tfailures, data=student_data, main="data",ylab="transformed failures"  )

dtfamrel<-density(tfamrel)
plot(dtfamrel)
qqnorm(tfamrel, pch = 1, frame = FALSE)
qqline(tfamrel, col = "steelblue", lwd = 2)
boxplot(tfamrel, data=student_data, main="data",ylab="transformed famrel"  )

dtfreetime<-density(tfreetime)
plot(dtfreetime)
qqnorm(tfreetime, pch = 1, frame = FALSE)
qqline(tfreetime, col = "steelblue", lwd = 2)
boxplot(tfreetime, data=student_data, main="data",ylab="transformed freetime"  )

dtgoout<-density(tgoout)
plot(dtgoout)
qqnorm(tgoout, pch = 1, frame = FALSE)
qqline(tgoout, col = "steelblue", lwd = 2)
boxplot(tgoout, data=student_data, main="data",ylab="transformed goout"  )

dtDalc<-density(tDalc)
plot(dtDalc)
qqnorm(tDalc, pch = 1, frame = FALSE)
qqline(tDalc, col = "steelblue", lwd = 2)
boxplot(tDalc, data=student_data, main="data",ylab="transformed Dalc"  )

dtWalc<-density(tWalc)
plot(dtWalc)
qqnorm(tWalc, pch = 1, frame = FALSE)
qqline(tWalc, col = "steelblue", lwd = 2)
boxplot(tWalc, data=student_data, main="data",ylab="transformed Walc"  )

dthealth<-density(thealth)
plot(dthealth)
qqnorm(thealth, pch = 1, frame = FALSE)
qqline(thealth, col = "steelblue", lwd = 2)
boxplot(thealth, data=student_data, main="data",ylab="transformed health"  )

dtabsences<-density(tabsences)
plot(dtabsences)
qqnorm(tabsences, pch = 1, frame = FALSE)
qqline(tabsences, col = "steelblue", lwd = 2)
boxplot(tabsences, data=student_data, main="data",ylab="transformed absences"  )
