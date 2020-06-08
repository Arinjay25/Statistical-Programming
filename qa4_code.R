library(datasets)

# Create a sample of 40 numbers which are normally distributed.
norm <- rnorm(40,0,0.1)

x <- seq(-1,1,length.out = 40)

y <- 2*x+norm

plot(y)
#SLR pass from origin point (0,0) then y = b1*x;
b1<- sum(x*y)/sum(x*x)
# Y origin
y0<- c(b1*x)
c(y0)

plot(x,y)
lines(x,y0,col="blue", lwd="2")

#Error in origin line

e = abs(sum(y-y0))
c(e)

#r^2 is SSR/SST 
ybar0 = sum(y)/40


SSR = sum((y0-ybar0)*(y0-ybar0))
c(SSR)
c(ybar0)

SST = sum((y-ybar0)*(y-ybar0))
c(SST)

rsqaur <- SSR/SST
c(rsqaur)

#ordinary linear regression y^i = b0 + b1xi

b1<- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))*(x-mean(x)))
c(b1)
#bo is very small near to zero '0', so it looks like RTO
bo <- mean(y)-b1*mean(x)
c(bo)

yhat = bo+b1*x
c(yhat)
plot(x,y)
lines(x,yhat,col ="red",lwd ="2")
abline(h=0,v=0)

#calculate error e1
e1<-abs(sum(y-yhat))
c(e1)
#calculate r^2 = SSR/SST
# SSR
SSR1 <- sum((yhat -mean(y))*(yhat -mean(y)))
c(SSR1)
# SST
SST1 <- sum ((y-mean(y))*(y -mean(y)))
c(SST1)
rsquare1 <-SSR1/SST1
c(rsquare1)


