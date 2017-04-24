# MATH 4070 COURSE PROJECT

# Read in the data and split into separate variables  

sealevel <- read.csv("zone1sealevel.csv")
sealevel$ID <- seq.int(nrow(sealevel))
sublevel <- sealevel[,c(6,3)]
y <- sublevel[,2]
sealevel2016 <- read.csv("sealevelzone12016.csv")

# QUESTION A

plot(y) # Initial plot of the data
lines(y)
times <- c(1:length(y))

M1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),24) # Creating the monthly dummy variables
M2=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),24)
M3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),24)
M4=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),24)
M5=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),24)
M6=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),24)
M7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),24)
M8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),24)
M9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),24)
M10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),24)
M11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),24)

lsres <- lm(y~times+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11)$resid # Calculating the linear regression residuals
par(mfrow=c(1,2))
acf(lsres,lag.max=48) # ACF has spikes at lags 1 and 2 -> MA(2)
pacf(lsres,lag.max=48) # PACF has spikes at lag 1 -> AR(1)
X <- cbind(times,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11) 

# Test a variety of different models - for report pick the best and two others
 
# fit1a <- arima(y, order = c(2, 0, 1), xreg = X, include.mean = 0) 
#   Box.test(fit1a$resid,lag=24,fitdf=3,type="Ljung-Box")
fit2a <- arima(y, order = c(1, 0, 2), xreg = X, include.mean = 0) # Select this one
  Box.test(fit2a$resid,lag=24,fitdf=3,type="Ljung-Box")
fit3a <- arima(y, order = c(1, 0, 0), xreg = X, include.mean = 0)
  Box.test(fit3a$resid,lag=24,fitdf=1,type="Ljung-Box")
fit4a <- arima(y, order = c(0, 0, 1), xreg = X, include.mean = 0)
  Box.test(fit4a$resid,lag=24,fitdf=1,type="Ljung-Box")
fit5a <- arima(y, order = c(1, 0, 1), xreg = X, include.mean = 0)
  Box.test(fit5a$resid,lag=24,fitdf=2,type="Ljung-Box")
# fit6a <- arima(y, order = c(2, 0, 0), xreg = X, include.mean = 0)
#  Box.test(fit6a$resid,lag=24,fitdf=2,type="Ljung-Box")
fit7a <- arima(y, order = c(0, 0, 2), xreg = X, include.mean = 0)
  Box.test(fit7a$resid,lag=24,fitdf=2,type="Ljung-Box")

# Test if the sea level is increasing

seatest <- lm(formula = y ~ times)
summary(seatest)
testdata <- c(1:300)
testplots <- -215.5556281 + 0.3399659*testdata
lines(testdata,testplots,col="red")

# Confidence intervals for parameters

confint(fit2a, leval = 0.95)
library(lmtest)
coeftest(fit2a)

# Testing if the sea level is rising
# 95% confidence interval for monthly rate of increase

# QUESTION B

par(mfrow=c(2,2))
n=length(y)
plot(1:n,y)
lines(1:n,y)
z1 = diff(y,lag=1) # Just for testing; does not look good
z12 = diff(y,lag=12)
z121 =diff(z12,lag=1) 
plot(z121)
lines(z121)
acf(z121,lag.max=60)
pacf(z121,lag.max=60) # We will select differencing at lag 12, then lag 1

# ACF spikes at 1, 12, and 13
# PACF spikes at 1, 10, 11, 12, 13, and 24
# Testing multiple models - for report pick the best and two others
# Seasonal AR with MA(1) and seasonal MA(1)

fit1b <- arima(y,order=c(0,0,1),xreg=1:n,seasonal=list(period=12,order=c(4,1,1)))
  Box.test(fit1b$resid,lag=24,fitdf=6,type="Ljung")
fit2b <- arima(y,order=c(0,0,1),xreg=1:n,seasonal=list(period=12,order=c(3,1,1))) 
  Box.test(fit2b$resid,lag=24,fitdf=5,type="Ljung")
fit3b <- arima(y,order=c(0,0,1),xreg=1:n,seasonal=list(period=12,order=c(2,1,1)))
  Box.test(fit3b$resid,lag=24,fitdf=4,type="Ljung")
fit4b <- arima(y,order=c(0,0,1),xreg=1:n,seasonal=list(period=12,order=c(1,1,1)))
  Box.test(fit4b$resid,lag=24,fitdf=3,type="Ljung")
fit5b <- arima(y,order=c(0,0,1),xreg=1:n,seasonal=list(period=12,order=c(0,1,1)))
  Box.test(fit5b$resid,lag=24,fitdf=2,type="Ljung")
fit6b <- arima(y,order=c(1,0,1),xreg=1:n,seasonal=list(period=12,order=c(1,1,1))) # Select this model
  Box.test(fit6b$resid,lag=24,fitdf=4,type="Ljung")

# QUESTION C

# Predictions

newx=cbind(289:300,c(1,0,0,0,0,0,0,0,0,0,0,0),c(0,1,0,0,0,0,0,0,0,0,0,0),c(0,0,1,0,0,0,0,0,0,0,0,0),
           c(0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,1,0,0,0,0,0,0,0),c(0,0,0,0,0,1,0,0,0,0,0,0),
           c(0,0,0,0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,0,1,0,0,0,0),c(0,0,0,0,0,0,0,0,1,0,0,0),
           c(0,0,0,0,0,0,0,0,0,1,0,0),c(0,0,0,0,0,0,0,0,0,0,1,0))
preda=predict(fit2a,n.ahead=12,newxreg=newx) # The prediction using Question A's model
predb=predict(fit6b,n.ahead=12,newxreg=c(289:300)) # The prediction using Question B's model

# Calculating Prediction Error - these loops calculate the error equation given in the project document

aerror <- 0
for(i in 1:12) {
  aerror <- aerror + (sealevel2016[i,3] - preda$pred[i])^2
}
berror <- 0
for(i in 1:12) {
  berror <- berror + (sealevel2016[i,3] - predb$pred[i])^2
}

# BIC Calculations 

library(stats4)
BIC(fit2a)
BIC(fit6b)
