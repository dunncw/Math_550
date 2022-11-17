# read in csv data
pga_data <- read.csv("/Users/cindydunn/Desktop/Grad_School/Math_550/hw5/data/pgatour2006.csv")
#ignore the column 'name' as it provides no predictive power
pga_data <- pga_data[,-1]
# keep only these columns 
# Y = PrizeMoney; x1= Driving Accuracy; x2= GIR; 
# x3= PuttingAverage; x4= BirdieConversion; x5= SandSaves; 
# x6= Scrambling; and x7= PuttsPerRound.
# remove column 1
pga_data <- pga_data[, -1]
# remove column 2
pga_data <- pga_data[, -2]
# remove the second to last column
pga_data <- pga_data[, -7]

#import libraries
library(leaps)


#(a) Identify the optimal model or models based on adj R2 , AIC, AIC , BIC from the approach based on all possible subsets.
regfit.full = regsubsets(PrizeMoney ~ ., data = pga_data, nvmax = 19)
reg.summary = summary(regfit.full)
reg.summary
coef(regfit.full, 1:7)

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Predictors ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Predictors ", ylab="Adjusted RSq",type="l")
plot(reg.summary$cp ,xlab="Number of Predictors ",ylab="Cp", type='l')
plot(reg.summary$bic ,xlab="Number of Predictors ",ylab="BIC",type='l')

#define a function called aicc to calculate the AICc
aicc_func <- function(model,p){
  AIC(model)+2*(p+2)*(p+3)/(nrow(pga_data)-p-1)
}

# model with 3 predictors we should use according to reg.summary
lm3<-lm(PrizeMoney~GIR+BirdieConversion+SandSaves,data=pga_data)
summary(lm3)
AIC(lm3)
BIC(lm3)
aicc_func(lm3,3)

# model with 5 predictors we should use according to reg.summary
lm5<-lm(PrizeMoney~DrivingAccuracy+PuttsPerRound+GIR+BirdieConversion+SandSaves,data=pga_data)
summary(lm5)
AIC(lm5)
BIC(lm5)
aicc_func(lm5,5)

# (b) Identify the optimal model or models based on AIC and BIC from the
# approach based on backward selection.

# fullmodb <- lm(PrizeMoney~1, data=pga_data)
# nullmodb <- lm(PrizeMoney~.,data=pga_data)
# reg1b<-step(nullmodb,scope=list(lower=fullmodb,upper=nullmodb), direction ="backward")
# #or

backwards_step_model <- step(lm(PrizeMoney ~ ., data = pga_data), direction = "backward")
summary(backwards_step_model)
#calculate the AIC and BIC for the model
# or
AIC(backwards_step_model)
BIC(backwards_step_model)
aicc_func(backwards_step_model,length(backwards_step_model$coefficients))

##### do I need aic for backwards and forwards model 

# (c) Identify the optimal model or models based on AIC and BIC from the
# approach based on forward selection.
nullmod<-lm(PrizeMoney~1,data=pga_data)
fullmod<-lm(PrizeMoney~.,data=pga_data)

reg1A<-step(nullmod,scope=list(lower=nullmod, upper=fullmod), direction="forward")
summary(reg1A)
#calculate the AIC and BIC for the model
AIC(reg1A)
BIC(reg1A)
aicc_func(reg1A,length(reg1A$coefficients))

# (d) Carefully explain why the models chosen in (a) & (c) are not the same while
# those in (a) and (b) are the same.

# best subset selection finds a model the best model for each possible number of predictors.
# forward selection starts with a null model and adds predictors one at a time and choose the best model based on AIC. 
# there is a difference here as forward selection builds on itself and best subset selection does not. 
# When forward slection goes to add another predictor it can only choose from the remaining predictors and compare the the current models aic. 
# this is fundementaly different then best subset selection which looks at all possible subset models with all possible subset predictors.
# Backwards selection starts with a full model and removes predictors one at a time and choose the best model based on AIC.
# this has the possibility of alinging with best subset selection as it is possible to remove predictors and end up with the same model as best subset selection. but it is not garunteed.
# backwards selection is not garunteed to find the same model as best subset selection because it is possible to remove predictors and end up with a worse model than the best subset selection model.

# (e) Recommend a final model. Give detailed reasons to support your choice.

#create a dataframe with each model name and its AIC and BIC
model_names <- data.frame(
    model_name = c('lm3', 'lm5', "backwards_step_model", "reg1A"),
    hw_part = c('a', 'a', 'b', 'c'),
    aic = c( AIC(lm3), AIC(lm5), AIC(backwards_step_model), AIC(reg1A)), 
    bic = c( BIC(lm3), BIC(lm5), BIC(backwards_step_model), BIC(reg1A)),
    aicc = c(aicc_func(lm3,3), aicc_func(lm5,5), aicc_func(backwards_step_model,length(backwards_step_model$coefficients)),
     aicc_func(reg1A,length(reg1A$coefficients))))
model_names

# the best model is the one with the lowest AICc value
# according to the table above the best model is the lm5 model
# this model has 5 predictors and the lowest AICc value
# the lm5 model has the lowest AICc value and the lowest BIC value

#######
# what metric is king or is it just best prefoming model acrossed the board
# what else should be said here
# do my model metrics look correct i feel like i got something wrong because a and c are same and a and b are diffenret

# (f) Interpret the regression coefficients in the final model. Is it necessary to be
# cautious about taking these results to literally?

summary(lm5)

# according to the textbook we know that the p-values obtained after variable selection are much smaller than there true values before variable selsciton.
# this means we have added a level of bais to our data and we should be cautious about taking the results to literally as they can be highly misleading.
# the best way to prove this model would be to see how it preformes on a novel set of test data and see how well it predicts the prize money of said test data.