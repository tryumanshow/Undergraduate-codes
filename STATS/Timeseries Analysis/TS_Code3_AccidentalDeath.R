rm(list=ls(all=TRUE))
source('TS-library.R')

data <- scan('deaths.txt')
data <- ts(data, start=1973, end=1979, freq=12)
n <- length(data)
par(mfrow=c(1,2))
plot.ts(data)
title('US accidental deaths')
acf2(data, 35)
title('SACF') # Here exists seasonality!



# Estimate 'Seasonality' and remove it from the observations.

#####################################################
########### Method1: 'Harmonic Regression'###########
#####################################################

t <- 1:n
m1 <- floor(length(data) / 12) # 12: period 
m2 <- 2*m1

costerm1 <- cos(m1*2*pi/n*t)
sinterm1 <- sin(m1*2*pi/n*t)
costerm2 <- cos(m2*2*pi/n*t)
sinterm2 <- sin(m2*2*pi/n*t)


out.lm1 <- lm(data ~ 1 + costerm1 + sinterm1)
out.lm2 <- lm(data ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)

summary(out.lm1)
summary(out.lm2) # out.lm1 looks little bit better


x <- as.vector(time(data))
plot.ts(data)
lines(x, out.lm1$fitted, col='blue')
lines(x, out.lm2$fitted, col='red')
legend(1975, 11500, lty=c(1,1), col=c('blue', 'red'), c('k=1', 'k=2'))

# It seems that second model is better, so get the dignostics of residuals

par(mfrow=c(2,2))
plot.ts(data)
title('US accidental deaths')
lines(x, out.lm2$fitted, col='red')

plot(out.lm2$fitted, out.lm2$residuals)
title('Residuals vs Fitted')
acf2(out.lm2$residuals)
title('SACF-residuals')

qqnorm(out.lm2$residuals)
qqline(out.lm2$residuals) # Still need some other models...




####################################################
########### Method2: 'Seasonal Smoothing'###########
####################################################

library(itsmr)

season.avg <- season(data, d=12)
plot.ts(data)
title('US accidental deaths')
lines(x, season.avg + mean(data), col='red')

plot.ts(data - (season.avg + mean(data)))



#######################################################
########### Method3: 'Seasonal Differencing'###########
#######################################################

diff12 <- diff(data, lag=12)
par(mfrow=c(1,2))
plot.ts(data)
title('US accidental deaths')
plot(x[13:length(data)], diff12, type='l', col='red')
title('Seasonal Differencing') # No seasonality but some linear or quadratic trend might be needed









### cf. Classical Decomposition to remove trend and seasonality at once.

par(mfrow=c(2,2))

out <- classical(data, d=12, order=1)
plot.ts(data)
title('step1')
lines(x, out$m1, col='red')

plot.ts(data-out$m1)
title('step2')
lines(x, out$st, col='red')

plot.ts(data-out$st)
title('step3')
lines(x, out$m, col='red')

plot.ts(data)
lines(x, out$fit, col='red')
title('Final')
