#sort data in ascending order
sortdataFrame <- Ass2data[order(Ass2data$X1), ]
Y<-sortdataFrame$Y
X1<-sortdataFrame$X1
X2<-sortdataFrame$X2
mean(Y)
# MRL model y = B1*X1+B2*X2+e1
model1<-lm(Y~X1+X2)
# get a summary of the model 
summary(model1)
confint(model1)
# the estimated regression function.
b0<--0.9225
b1<-15.0461
b2<-0.7587
yhat<-b0+b1*X1+b2*X2
#Large (X2=0): If the number of coppiers serived increases by 1 unit then 
#number of minutes spend on the phone increase 15.0461. Y = -0.9225 + 15.0461 (Coppiers Serviced)

#Small (X2=1): If the copier is a small copier time on a service call is
#will increase that time by .7587 in comparison to large coppiers. Y= -0.1638 + 15.0461 (Coppiers Serviced)

# plot of model
plot(model1)

#Estimate the effect of copier model X2 on mean service time µy with a 95% confidence interval.
#Model2 only have X2 
model2<-lm(Y~X2)
summary(model2)
confint(model2, level = .95)

#residuals of model1
resid1<-resid(model1)
# X1 *X2 = X12
X12<-X1*X2

#Plot residual again X12
plot(X12,resid1)

#4 Fit the regression model yi = ??0 + ??1xi1 + ??2xi2 + ??12xi1xi2 + ei and provide the
#estimated regression function.
model2<-lm(Y~X1+X2+X12)
# get a summary of the model2 
summary(model2)

#T-test Decicion Rule
t <- qt(0.975, 41)
c(t)

