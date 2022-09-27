#read in deliminated file
salarygov <- read.delim(file.path("/Users/cindydunn/Desktop/Grad_School/Math_550/hw2/data/salarygov.txt"))
attach(salarygov)
plot(salarygov$MaxSalary~salarygov$Score)

MS<-density(salarygov$MaxSalary)
plot(MS)
qqnorm(salarygov$MaxSalary, pch = 1, frame = FALSE) 
qqline(salarygov$MaxSalary, col = "steelblue", lwd = 2)
boxplot(salarygov$MaxSalary,data=salarygov, main="Salary data",ylab="Max salary")

S<-density(salarygov$Score)
plot(S)
qqnorm(salarygov$Score, pch = 1, frame = FALSE) 
qqline(salarygov$Score, col = "steelblue", lwd = 2)
boxplot(salarygov$Score,data=salarygov, main="Salary data",ylab="Score")

# apply Box-Cox method on both response and predictor
library(car)
pc<-powerTransform(cbind(salarygov$MaxSalary,salarygov$Score)~1)
summary(pc)
tx<-sqrt(salarygov$Score)
ty<-log(salarygov$MaxSalary,base=exp(1))