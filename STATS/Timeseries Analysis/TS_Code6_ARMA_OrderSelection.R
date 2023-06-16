getwd()
setwd('C:/Users/승우/Desktop/4-2/시계열분석')


source('TS-library.R')


lake = c(10.38,11.86,10.97,10.8,9.79,
         10.39,10.42,10.82,11.4,11.32,11.44,11.68,11.17,
         10.53,10.01,9.91,9.14,9.16,9.55,9.67,8.44,
         8.24,9.1,9.09,9.35,8.82,9.32,9.01,9,9.8,9.83,9.72,9.89,
         10.01,9.37,8.69,8.19,8.67,9.55,8.92,8.09,
         9.37,10.13,10.14,9.51,9.24,8.66,8.86,8.05,
         7.79,6.75,6.75,7.82,8.64,10.58,9.48,7.38,
         6.9,6.94,6.24,6.84,6.85,6.9,7.79,8.18,
         7.51,7.23,8.42,9.61,9.05,9.26,9.22,9.38,
         9.1,7.95,8.12,9.75,10.85,10.41,9.96,9.61,
         8.76,8.18,7.21,7.13,9.1,8.25,7.91,6.89,
         5.96,6.8,7.68,8.38,8.52,9.74,9.31,9.89,9.96)

plot.ts(lake)

# ACF/ PACF plot
par(mfrow=c(1,2))
acf2(lake); title('ACF of lake data') # Looking at the acf2, -> ARMA(1,1) might be possible
pacf(lake) # Looking at the pacf, -> ARMA(2) seems plausible


############################################################################
#  Method1: See the ACF/PACF -> generate models -> test -> model selection #
############################################################################


## Method1-1: Consider the Yule-Walker estimation of AR(2) model
# In principle, YW equation assumes the mean is included! 
ar.yw(lake, aic=FALSE, order.max=2, demean=F) 


## Method1-2: Use MLE
ar11.out <- arima(lake, order=c(1,0,1))
ar1.out <- arima(lake, order=c(1,0,0))
ar2.out <- arima(lake, order=c(2,0,0))
ma1.out <- arima(lake, order=c(0,0,1))


# tsdiag: a generic function to plot time-series diagnostics
tsdiag(ar11.out)# seems fine
tsdiag(ar1.out)# not really good in terms of IID
tsdiag(ar2.out) # seems fine
tsdiag(ma1.out) # not really (worst among these four model)


library(itsmr)
test(resid(ar11.out))
test(resid(ar2.out))


######################################
## Method2: Use informatin criteria ##
######################################

### Method 2-1: Hard-Coding~

AICC <- BIC <- AIC <- P <- Q <- NULL
pmax <- 3
qmax <- 3
n <- length(lake)

for (p in 0:qmax){
  
  for (q in 0:qmax){
    fit <- arima(lake, order=c(p, 0, q), include.mean=T)
    m <- p+q+1
    AIC <- c(AIC, -2*fit$loglik + 2*m)
    AICC <- c(AICC, -2*fit$loglik + 2*m*n/(n-m-1))
    BIC <- c(BIC, -2*fit$loglik + m*log(n))
    P <- c(P, p)
    Q <- c(Q, q)
  }
  
}


id1 <- which.min(AICC)
id2 <- which.min(BIC)

c(P[id1], Q[id1])
c(P[id2], Q[id2])

plot(AICC)
plot(BIC)


### Method 2-2: estimate the estimator automatically
library(forecast)

fit <- auto.arima(lake, d=0)
summary(fit)

# If I want to test whetehr all coefficients are away from zero
2*(1-pnorm(fit$coef/(sqrt(diag(fit$var.coef)))))

# Do the forecast
forecast(fit, 30)
plot(forecast(fit, 30))



###############################################
### Method 3: Out-of-sample forasting error ###
###############################################

myst <- scan('mysterious.txt')
library(forecast)
auto.arima(myst, d=0) # ARMA(3,1) is the best in terms of smallest BIC

  
m <- 30
n <- length(myst)
N <- n-m
p <- 3
q <- 1

err <- numeric(m)
for (i in 1:m){
  trainindex <- 1:(N+i-1)
  fit <- arima(myst[trainindex], order=c(p,0,q), include.mean=FALSE)
  Xhat <- forecast(fit, h=1)$mean
  err[i] <- (myst[N+i - Xhat]^2)
}


p <- q <- 1
err <- numeric(m)
for (i in 1:m){
  trainindex <- 1:(N+i-1)
  fit <- arima(myst[trainindex], order=c(p,0,q), include.mean=FALSE)
  Xhat <- forecast(fit, h=1)$mean
  err[i] <- (myst[N+i] - Xhat)^2
}

mean(err)
