#random 100 row from given data.
df <- APPENC07[sample(nrow(APPENC07), 100), ]
#sort data in ascending order
sortdataFrame <- df[order(df$V3), ]

# assigning secound column sales price (Y)
Ygiven <- sortdataFrame$V2
# assigning 3rd column finished square feet (X)
Xgiven <- sortdataFrame$V3

# L is sum((Xgiven-mean(Xgiven))^2)
L <- sum((Xgiven - mean(Xgiven)) ^ 2)

#the point of estimate b1 for B1
b1 <-  sum((Xgiven - mean(Xgiven)) * (Ygiven - mean(Ygiven))) / L
c(b1)
#the point of estimate bo for BO
bo <- mean(Ygiven) - b1 * mean(Xgiven)
c(bo)
#estimated Yhat of response variable y
yhat <- bo + b1 * Xgiven

plot(Xgiven, Ygiven)
lines(Xgiven, yhat, col = "red", lwd = "2")
abline(h = 0, v = 0)
#Dof n-2
n <- 100
# t value as (1-alpha/2 = 0.975) and (n-2 = 98)DOF
t <- qt(0.95, n - 2)
c(t)

#MSE
MSE <- sum((Ygiven - yhat)^2) / n - 2
c(MSE)

#Standard error (Se) of yhat for confident interval
for (i in 1:length(Xgiven)) {
  #Some part of Se, G=(xh-xbar)^2/sum((Xgiven-xbar)^2)
  G[i] <- ((Xgiven[i] - mean(Xgiven)) ^ 2) / L
  # Se of mean response yh
  Se[i] <- sqrt(MSE * (1 / n + G[i]))
  sePI[i] <- sqrt(MSE * (1 + 1 / n + G[i]))
  #yhat H (yh) for every value of Xh (Xgiven[i])
  yh[i] <- bo + b1 * Xgiven[i]
  #confident Interval of mean response.
  CIpos <- yh + t * Se
  CIneg <- yh - t * Se
  PIpos <- yh + t * sePI
  PIneg <- yh - t * sePI
}
lines(Xgiven, CIpos, col = 'blue')
lines(Xgiven, CIneg, col = 'blue')
lines(Xgiven, PIpos, col = 'green')
lines(Xgiven, PIneg, col = 'green')
#text(locator(),labels = c("blue lines is CI", "green lines is PI"))
text(x=Xgiven[80], y=Ygiven[90],label="3->SLR, (2,4)-> CI, (1,5)-> PI")
#CI and PI at X=xbar mean
CImeanpos<-mean(Ygiven)+t*sqrt(MSE * (1 / n ))
c(CImeanpos)
CImeanneg<-mean(Ygiven)-t*sqrt(MSE * (1 / n ))
c(CImeanneg)
PImeanpos<-mean(Ygiven)+t*sqrt(MSE * (1+1 / n ))
c(PImeanpos)
PImeanneg<-mean(Ygiven)-t*sqrt(MSE * (1+1 / n ))
c(PImeanneg)
fit<-lm(Ygiven~Xgiven)
summary(fit)
con