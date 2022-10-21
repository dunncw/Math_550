# read in delmited file
overdue_df <- read.delim(file.path("/Users/cindydunn/Desktop/Grad_School/Math_550/hw3/data/overdue.txt"))
overdue_df$type <- c(rep(1,48), rep(0,48)) # create a type variable and have 1 = residential and 0 = commercial
overdue_df$type <- as.factor(overdue_df$type) # convert type to a factor
attach(overdue_df)

#q1
mlr <- lm(LATE~., data=overdue_df)

