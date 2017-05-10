# Regression and Error Fitting

# Read in the data and split into separate variables  

sealevel <- # Read in sample sea level data
sealevel$ID <- seq.int(nrow(sealevel))
sublevel <- sealevel[,c(6,3)]
y <- sublevel[,2]
sealevel2016 <- read.csv("sealevelzone12016.csv")

plot(y,ann="false") # Initial plot of the data
lines(y)
title(main="Sea Level Data")
title(xlab="Month")
title(ylab="Sea Level")
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

ls <- lm(y~times+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11) # This way it displays the intercept
lsres <- ls$resid # Calculating the linear regression residuals
acf(lsres,lag.max=48) # ACF has spikes at lags 1 and 2 -> MA(2)
pacf(lsres,lag.max=48) # PACF has spikes at lag 1 -> AR(1)
X <- cbind(times,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11) 

# Test a variety of different models
 
fit1a <- arima(y, order = c(1, 0, 2), xreg = X, include.mean = TRUE)
  Box.test(fit1a$resid,lag=24,fitdf=3,type="Ljung-Box")
fit2a <- arima(y, order = c(1, 0, 0), xreg = X, include.mean = TRUE)
  Box.test(fit2a$resid,lag=24,fitdf=1,type="Ljung-Box")
fit3a <- arima(y, order = c(0, 0, 1), xreg = X, include.mean = TRUE)
  Box.test(fit3a$resid,lag=24,fitdf=1,type="Ljung-Box")
fit4a <- arima(y, order = c(1, 0, 1), xreg = X, include.mean = TRUE)
  Box.test(fit4a$resid,lag=24,fitdf=2,type="Ljung-Box")
fit5a <- arima(y, order = c(0, 0, 2), xreg = X, include.mean = TRUE) # Select this one
  Box.test(fit5a$resid,lag=24,fitdf=2,type="Ljung-Box")

# Test if the sea level is increasing

library(lmtest)
beta0 <- fit5a$coef[4] # calculate t-value for this
se0 <- 0.0579
t <- beta0/se0
confint(fit5a, level=0.95)

n=length(y)
plot(1:n,y)
lines(1:n,y)
z1 = diff(y,lag=1) # Just for testing; does not look good
z12 = diff(y,lag=12)
z121 = diff(z12,lag=1) 
plot(z121, ann="false")
lines(z121)
title(main="Sea Level Data at Lag 12 Then Lag 1")
title(xlab="Differenced Index")
title(ylab="Sea Level")
acf(z121,lag.max=60)
pacf(z121,lag.max=60) # We will select differencing at lag 12, then lag 1

# ACF spikes at 1, 12, and 13
# PACF spikes at 1, 10, 11, 12, 13, and 24
# Testing multiple models
# Seasonal AR with MA(1) and seasonal MA(1)

fit1b <- arima(y,order=c(0,1,1),xreg=1:n,seasonal=list(period=12,order=c(4,1,1)))
  Box.test(fit1b$resid,lag=24,fitdf=6,type="Ljung")
fit2b <- arima(y,order=c(0,1,1),xreg=1:n,seasonal=list(period=12,order=c(3,1,1))) 
  Box.test(fit2b$resid,lag=24,fitdf=5,type="Ljung")
fit3b <- arima(y,order=c(1,1,1),xreg=1:n,seasonal=list(period=12,order=c(1,1,1)))
  Box.test(fit3b$resid,lag=24,fitdf=4,type="Ljung")
fit4b <- arima(y,order=c(1,1,1),xreg=1:n,seasonal=list(period=12,order=c(0,1,1))) # Select this one
  Box.test(fit4b$resid,lag=24,fitdf=3,type="Ljung")

# Predictions

newx=cbind(289:300,c(1,0,0,0,0,0,0,0,0,0,0,0),c(0,1,0,0,0,0,0,0,0,0,0,0),c(0,0,1,0,0,0,0,0,0,0,0,0),
           c(0,0,0,1,0,0,0,0,0,0,0,0),c(0,0,0,0,1,0,0,0,0,0,0,0),c(0,0,0,0,0,1,0,0,0,0,0,0),
           c(0,0,0,0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,0,1,0,0,0,0),c(0,0,0,0,0,0,0,0,1,0,0,0),
           c(0,0,0,0,0,0,0,0,0,1,0,0),c(0,0,0,0,0,0,0,0,0,0,1,0))
preda=predict(fit5a,n.ahead=12,newxreg=newx) # The prediction using first model
predb=predict(fit4b,n.ahead=12,newxreg=c(289:300)) # The prediction using second model

# Calculating Prediction Error 

aerror <- 0
aerror2 <- 0
for(i in 1:12) {
  aerror <- aerror + (sealevel2016[i,3] - preda$pred[i])^2
  aerror2 <- aerror2 + abs(sealevel2016[i,3] - preda$pred[i])
}
berror <- 0
berror2 <- 0
for(i in 1:12) {
  berror <- berror + (sealevel2016[i,3] - predb$pred[i])^2
  berror2 <- berror2 + abs(sealevel2016[i,3] - predb$pred[i])
}

# BIC Calculations 

library(stats4)
BIC(fit5a)
BIC(fit4b)
