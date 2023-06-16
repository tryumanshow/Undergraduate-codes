library(forecast)

data <- scan('huron.txt')

n <- length(data)
x <- 1:n
out.lm <- lm(data ~ 1 + x)

layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
plot.ts(data); title('Lake huron data') # Time plot
lines(out.lm$fitted, col='red') # Fittd Line
acf(data)
pacf(data) # Going to fit AR(2)!

res <- out.lm$residuals
const <- rep(1, n)
design_mat <- cbind(const, x)

# GLS model (not OLS)
fit.reg <- Arima(data, order=c(2, 0, 0), xreg=design_mat, 
                 include.mean=FALSE)


# Forecast Lag 30
par(mfrow=c(1,1))
h <- 30
news <- (n+1):(n+h)
plot(forecast(fit.reg, h=30, xreg=cbind(rep(1, h), newx)))

     