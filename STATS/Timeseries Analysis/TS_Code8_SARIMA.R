library(itsmr)
data <- airpass
data.log <- log(data) # Determined to use log transformation to stabilize Var.
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
plot(data.log, type='l')
title('Log(airpass')
acf(data.log, lag=50)
pacf(data.log, lag=50)


# Hmm, it seems here exists both linear trend and seasonality.
# I'm going to remove linear trend first. (By Differenciation)

data1 <- diff(data.log, 1)
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
plot(data1, type='l')
title('Log(Airpass) - detrended')
acf(data1, lag=50)
pacf(data1, lag=50)


# Here seems still seasonlaity left, so lag12 diffrentiation 
data2 <- diff(data1, lag=12)
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
plot(data2, type='l')
title('Log(Airpass) - (1-B^12)(1-B)X_t')
acf(data2, lag=50)
pacf(data2, lag=50)
## Hmm. Variation got severe
## acf, pacf at lag1 still exists. 


# By looking at the acf, pacf of data2, 
# I can remind two SARIMA(1,1,0)X(1,0,0) or SARIMA(0, 1, 1)X(1,0,0)
# Then I'm going to fit this two models

fit.1 <- arima(data.log, order=c(1,1,0), season=list(order=c(1,0,0), period=12))
fit.2<- arima(data.log, order=c(0,1,1), season=list(order=c(1,0,0), period=12))


fit.1
fit.2 # It seems second model is better in terms of BIC


library(forecast)
plot(forecast(fit.2, lag=12))


par(mfrow=c(1,2))
plot(forecast(fit.1, lag=12))
plot(forecast(fit.2, lag=12))



### Or use auto.arima model 
dat.ff <- ts(data.log, frequency=12)

fitted_sarima <- auto.arima(dat.ff)
plot(forecast(fit, lag=12))

