#8
# Part 1a
diamond_data <- read.delim(file.path("/Users/cindydunn/Desktop/Grad_School/Math_550/hw2/data/diamonds.txt"))

slr = lm(Price~Size, data=diamond_data)
summary(slr)
#view the diagnostic charts for our current model
layout(matrix(1:4,2,2))
plot(slr)
# issues with this graph. the scale location graph fails line test. there are also heavy tails on q-q plot. 

plot(diamond_data$Size, diamond_data$Price, main="Diamond Price vs Size", xlab="Size", ylab="Price")
abline(slr)

#adj R^2 val is 97.8% so this model is perfect for predicting the price of diamonds using only size

# Part 1b

<<<<<<< HEAD
#plot the normality on the predictor of the response see if its normal or skewed
layout(matrix(1:4,2,2))
MS<-density(diamond_data$Size)
plot(MS)
qqnorm(diamond_data$Size, pch = 1, frame = FALSE) 
qqline(diamond_data$Size, col = "steelblue", lwd = 2)
boxplot(diamond_data$Size,data=diamond_data, main="data",ylab="Max size")
#according to the density plot, the data is skewed to the right
#according to the qqnorm plot, the data is right skewed and has haeavy tails
#according to the boxplot, the data is majority smaller sizes with few large size observations
#nomality plot not close to the line
=======
# Part 2a
library(caret)
feature_engineered_dd <- diamond_data
feature_engineered_dd$Size <- (feature_engineered_dd$Size)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
feature_engineered_dd$Price <- range01(feature_engineered_dd$Price)
>>>>>>> 4d0a89f12e6a74c1453c6b495fd949da78d9fead

#look at the normality for the predictor 
layout(matrix(1:4,2,2))
S<-density(diamond_data$Price)
plot(S)
qqnorm(diamond_data$Price, pch = 1, frame = FALSE) 
qqline(diamond_data$Price, col = "steelblue", lwd = 2)
boxplot(diamond_data$Price,data=diamond_data, main="data",ylab="Price"  )
#according to the density plot, the data is skewed to the left
#according to the qqnorm plot, the data is right skewed and has haeavy tails
#according to the boxplot, the data is majority smaller prices with few large price observations

#p2
# apply Box-Cox method on both response and predictor
library(car)
pc<-powerTransform(cbind(diamond_data$Size,diamond_data$Price)~1)
summary(pc)
#rounded power is 0 for both so we use log transformation
tprice<-log(diamond_data$Price)
tsize<-log(diamond_data$Size)

#here we look at the new data after the log transformation
layout(matrix(1:4,2,2))
S<-density(diamond_data$Price)
plot(S)
tS<-density(tprice)
plot(tS)
qqnorm(tprice, pch = 1, frame = FALSE) 
qqline(tprice, col = "steelblue", lwd = 2)
boxplot(tprice,data=diamond_data, main="Salary data",ylab="transformed Score")

#compare densitys of orginal and transformed data
layout(matrix(1:4,2,2))
MS<-density(diamond_data$Size)
plot(MS)
#transformed data
mty<-density(tsize)
plot(mty)
qqnorm(tsize, pch = 1, frame = FALSE) 
qqline(tsize, col = "steelblue", lwd = 2)
boxplot(tsize,data=diamond_data, main=" Salary data",ylab="transformed Max salary")

#now we take a look at the new model with our transformed data
plot(tprice~tsize)
tlm<-lm(tprice~tsize)
abline(tlm)
summary(tlm)
layout(matrix(1:4,2,2))
plot(tlm)

#Box-Cox method on predictor then Inverse Response method on response
pscore<-powerTransform(diamond_data$Size~1)
summary(pscore)
#gives us a -1 so we need to transform diamond_data$Size with a -1 power
tsize<-diamond_data$Size^(-1)

mty<-density(tsize)
plot(mty)
qqnorm(tsize, pch = 1, frame = FALSE) 
qqline(tsize, col = "steelblue", lwd = 2)
boxplot(tsize,data=diamond_data, main=" Salary data",ylab="transformed Max salary")


transy<-lm(diamond_data$Price~tsize)
lam<-invResPlot(transy, lambda=c(-1,-1/2,-1/3,-1/4,1/4,1/3,1/2,1))
lam$lambda
plot(lam$RSS~lam$lambda)
ptprice<- diamond_data$Price^(-0.48)

ptyx<-lm(ptprice~tsize)
plot(ptprice~tsize)
abline(ptyx)
plot(ptyx)
#round to -0.25
npty<- diamond_data$Price^(-0.5)

nptyx<-lm(npty~tsize)
plot(npty~tsize)
abline(nptyx)
layout(matrix(1:4,2,2))
plot(nptyx)
summary(nptyx)
