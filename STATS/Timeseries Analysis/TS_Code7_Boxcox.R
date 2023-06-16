data <- scan('airpass.txt')


plot_acf_pacf <- function(data){
  layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
  plot.ts(data, type='l')
  title('Airpass')
  acf(data)
  pacf(data)
}

# It seems I need some variance stabilization 

# Method1. Plotting

plot_acf_pacf(data)
plot_acf_pacf(data^(1/2))
plot_acf_pacf(data^(1/3))
plot_acf_pacf(log(data))

par(mfrow=c(1,2))
plot.ts(data)
title('Raw data')
qqnorm(data); qqline(data)

par(mfrow=c(1,2))
plot.ts(data^(1/2))
title('Raw data^(1/2) Transformation')
qqnorm(data^(1/2)); qqline(data^(1/2))

par(mfrow=c(1,2))
plot.ts(data^(1/3))
title('Raw data^(1/3) Transformation')
qqnorm(data^(1/3)); qqline(data^(1/3))

par(mfrow=c(1,2))
plot.ts(log(data))
title('log(Raw data) Transformation')
qqnorm(log(data)); qqline(log(data))


# Method2. Box-cox Transformation

## Estimate the lambda by MLE (Use MASS Library)

library(MASS)

x <- length(data)
fit <- boxcox(data ~ 1, plotit=T)
lambda <- fit$x[which.max(fit$y)] # The lambda chosen in Box-Cox Transformation

abline(v=lambda, col='red')


dat.bx <- data^(lambda)
layout(matrix(c(1,2,3,4), 2, 2, byrow=T))
plot(dat.bx, type='l')
qqnorm(dat.bx); qqline(dat.bx)
acf(dat.bx, lag=50)
pacf(dat.bx, lag=50)
