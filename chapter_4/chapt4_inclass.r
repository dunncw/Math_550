cleaning <- read.delim("/Users/cindydunn/Desktop/Grad_School/Math_550/chapter_4/cleaning.txt")
attach(cleaning)

plot(Crews,Rooms)
# look at standard deviation of crews by rooms
sd<-aggregate(Rooms,  by=list(Crews =Crews), FUN=sd)
# create a new data frame that is the standard deviation of crews by rooms
Newdata<-merge(cleaning,sd)
attach(Newdata)

wls <- lm(Rooms~Crews, data=Newdata, weights=1/x^2)# weight by 1/x^2 is the same as 1/sd^2 because are x is now the sd for each room
summary(wls)

sw<-sum(1/x^2)
mean_crews<-sum(1/x^2*Crews)/sw #mean of the predictor
mean_rooms<-sum(1/x^2*Rooms)/sw # mean of the response (these two comments could be flip flopped)

WSxx<-sum(1/x^2*(Crews-mean_crews)^2)
WSxy<-sum(1/x^2*(Rooms-mean_rooms)*(Crews-mean_crews))

slope<-WSxy/WSxx
intercept<- mean_rooms - slope * mean_crews

S_sq<-sum(1/x^2*resid(wls)^2)/(nrow(Newdata)-2)
sd_betahat_1<-sqrt(S_sq)*sqrt(1/ WSxx)
sd_betahat_1
sd_betahat_0<-sqrt(S_sq)*sqrt(1/sw+mean_crews ^2/ WSxx)
sd_betahat_0
#so these guys match error in the summary of wls
summary(wls)
