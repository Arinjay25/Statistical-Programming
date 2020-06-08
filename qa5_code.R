#read data from external file.
data5 <- read.csv("F:/Assigmnents/Appstat/data5.csv", sep="")
View(data5)

#Assign predictor variable x is the latitude
x<-data5$Lat

#mean of x as no. of variable are 48
xbar<-mean(x)
c(xbar)

#Assign response variable y is the mortality
y<-data5$Mort

#mean of x as no. of variable are 48
ybar<-mean(y)
c(ybar)

#the point of estimate b1 for B1
b1<-sum((x-xbar)*(y-ybar))/sum((x-xbar)*(x-xbar))
c(b1)

# t value as (1-alpha/2 = 0.975) and (n-2 = 46)DOF
t<-qt(0.95,46)
c(t)

#bo value
bo<-ybar-b1*xbar
c(bo)

#estimated Yhat of response variable y
yhat<-bo+b1*x


#MSE
MSE<- sum((y-yhat)*(y-yhat))/46
c(MSE)

#Standard error (Se) of b1
s<-sqrt(MSE)/sqrt(sum((x-xbar)*(x-xbar)))
c(s)

#Confidince Interval C.I. [a,b]
a<-b1-t*s
c(a)

b<-b1+t*s
c(b)

# verify the 
fit<-lm(y~x)
c(fit)

confint(fit)

summary(fit)

#solution of Part2 for question5
#SSR
SSR<-(sum((yhat-ybar)*(yhat-ybar)))
c(SSR)

#SSE
SSE<-(sum((y-yhat)*(y-yhat)))
c(SSE)

#SST
SST<-(sum((y-ybar)*(y-ybar)))
c(SST)

rsquare<- SSR/SST
c(rsquare)

plot(x,y)
lines(x,yhat,col="red", lwd ="2")
abline(h=0,v=0)
