#8
# Part 1a
diamond_data <- read.delim(file.path("C:/Users/cayde/Desktop/grad_school_code/Math550/Math_550/hw2/data/diamonds.txt"))

slr = lm(Price~Size, data=diamond_data)
summary(slr)

#adj R^2 val is 97.8% so this model is perfect for predicting the price of diamonds using only size

# Part 1b
slr

# Part 2a
library(caret)
feature_engineered_dd <- diamond_data
feature_engineered_dd$Size <- (feature_engineered_dd$Size)
feature_engineered_dd$Price <- preProcess(feature_engineered_dd$Price, method = c('range'))

slr = lm(Price~., data=feature_engineered_dd)
summary(slr)

# Part 2b
slr
