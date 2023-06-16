library(itsmr)
data <- scan('huron.txt')

n <- length(data)
x <- seq(1, n, 1)
out.lm <- lm(data~1+x)

test(out.lm$residuals)

# Test result says:
## It's not a iid series. There might be some dependent structure!
### (Ljung-Box Q, McLeod-Li Q, Turning points T)




# one by one

## Box-pierce
Box.test(out.lm$residuals, 20)

## Ljung-Box
Box.test(out.lm$residuals, 20, type='Ljung')

## Both test says there's some dependent structure.
## So it might be not enough to remove trend or seasonality.
## Need to consider another model like AR, MA, ARMA, ARIMA, SARIMA!


# Checking Gaussianity

qqnorm(out.lm$residuals)
qqline(out.lm$residuals, col='red')

shapiro.test(out.lm$residuals) # Yes it's absolutely normal!


# cf
## Anderson-Darling normality test
library(nortest)
ad.test(out.lm$residuals)

## Cramer-von Mises normality test
lillie.test(out.lm$residuals)

## Jacque-bera test
library(tseries)
jarque.bera.test(out.lm$residuals)