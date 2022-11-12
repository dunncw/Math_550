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
# student_data = student_data[, -which(names(student_data) %in% c("G1","G2"))]

# create a regression model for the data
student_model = lm(G3~.,data=student_data)
summary(student_model)

step.model <- step(student_model)
summary(step.model)

scatterplotMatrix(~Price+Food+Decor+Service+East, data=nyc)







