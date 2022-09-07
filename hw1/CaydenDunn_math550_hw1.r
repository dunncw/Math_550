library(knitr)

# Cayden Dunn 
## hw1, Math 550, 9/4/2022

## playbill

### read in the data
playbill <- read.csv(file.path("/Users/cindydunn/Desktop/Grad_School/math550/hw1/data/playbill.csv"))

# plot the data
plot(playbill$CurrentWeek, playbill$LastWeek, main="Scatterplot",xlab="last week gross ", ylab="current week gross", pch=19)

# a
#create a linear model
#### y - current gross, x - last week gross
pd_linearMod <- lm(CurrentWeek ~ LastWeek, data = playbill) # create the linear model
abline(pd_linearMod, col="red") # add the regression line to the plot
kable(summary(pd_linearMod)$coef) # print the coefficients

# confidence intervals for β1
confint(pd_linearMod)[2, ] # confidence interval for the slope

# 95% confident that the slope is between 0.95 and 1.01
# Week to week returns are bound to be similar but its highly unlikely
# that the returns would be exactly the same from one week to the next making 1 unlikely but a value
# around 1 rather likely.

# b
h_0 <- 10000 # null hypothesis
h_hat <- coef(pd_linearMod)[[1]] # observed value
h_hat_se <- summary(pd_linearMod)$coef[1, 2] # standard error of the observed value
t <- (h_hat - h_0) / h_hat_se # t-statistic
p <- 2 * pt(abs(t), nrow(playbill) - 2, lower.tail = FALSE) # p-value
kable(data.frame(h_0, h_hat, h_hat_se, t, p)) # print the results

#p values - range from 0-1(closer to 0 the more likley reject the null hypothesis)
# t values - numbers between -infinity and infinity that quantify how far away our observed value is from the null hypothesis
# this leads us to accept the null hypothesis, t = -0.32, p = 0.75

# c

# make a prediction, including prediction interval, for a 400,000$ box office result in the previous week:
predict(pd_linearMod, newdata = data.frame(LastWeek = 400000), interval = "prediction", level = 0.95)

# we are 95% confident that the box office will earn be between 359,832 and 439,442 dollars
# this current week if last week it grossed 400,000. the prediction of 450000 is not feasible
# value for the box office to gross this current week given the range of our prediction

# d

par(mfrow = c(2, 2)) # make a 2x2 plot
plot(pd_linearMod) # plot the linear model and its residuals and qq plot and scale location plot and leverage plot

# given that there is a close to perfect correlation from one week's revenue to the next its commonly okay to apply
# the prediction rule to forecast the next week's revenue. However seen in the above plots
# there are 3 weeks 6, 14, 8 that are outliers. These weeks suggest that the prediction rule should not be the end 
# all be all for forecasting next weeks revenue as it is not entirely accurate.

# Invoices 

### read in the data
invoices <- read.delim(file.path("/Users/cindydunn/Desktop/Grad_School/math550/hw1/data/invoices.txt"))

lm(Time ~ Invoices, data = invoices) # create the linear model print verbose output

# a
beta_0 <- coef(lm(Time ~ Invoices, data = invoices))[1] # intercept
beta_0_se <- summary(lm(Time ~ Invoices, data = invoices))$coef[1, 2] # standard error of the intercept
beta_0_t <- beta_0 / beta_0_se # t-statistic
beta_0_margin <- qt(0.975, nrow(invoices) - 2) * beta_0_se # margin of error
beta_0_ci <- c(beta_0 - beta_0_margin, beta_0 + beta_0_margin) # confidence interval
kable(data.frame(beta_0, beta_0_se, beta_0_t, beta_0_margin, beta_0_ci)) # print the results

# 95% confident that intercept is between 0.39, 0.89

# b

beta_1 <- 0.01 # null hypothesis
beta_1_hat <- coef(lm(Time ~ Invoices, data = invoices))[2] # observed value
beta_1_se <- summary(lm(Time ~ Invoices, data = invoices))$coef[2, 2] # standard error of the observed value
beta_1_t <- (beta_1_hat - beta_1) / beta_1_se # t-statistic
beta_1_p <- 2 * pt(abs(beta_1_t), nrow(invoices) - 2, lower.tail = FALSE) # p-value
kable(data.frame(beta_1, beta_1_hat, beta_1_se, beta_1_t, beta_1_p)) # print the results

# reject the null hypothesis t = 1.57, p = 0.12
# if p value is low null hypo must go(be rejected) uf p value is large null hypo is accepted (im not sure if this is right)(dont know how to intepert tvalues and pvalues)
#interperate p values - range from 0-1(closer to 0 the more likley reject the null hypothesis)(but how small does it have to be)

# c 

# time = beta 0 + beta 1 * invoices
beta_0 <- coef(lm(Time ~ Invoices, data = invoices))[1] # intercept
beta_1 <- coef(lm(Time ~ Invoices, data = invoices))[2] # slope
rse <- summary(lm(Time ~ Invoices, data = invoices))$sigma # residual standard error
n <- nrow(invoices) # number of observations
df <- n - 2 # degrees of freedom
rss <- summary(lm(Time ~ Invoices, data = invoices))$sigma^2 * df # residual sum of squares
mse <- rss / df # mean squared error

time <- beta_0 + beta_1 * 130 # time to process 130 invoices
err <- qt(0.975, df) * sqrt(mse) * sqrt(1 + 1 / n + (130 - mean(invoices$Invoices))^2 / sum((invoices$Invoices - mean(invoices$Invoices))^2)) # error
upr <- time + err # upper prediction interval
lwr <- time - err # lower prediction interval
kable(data.frame(time, err, upr, lwr)) # print the results
