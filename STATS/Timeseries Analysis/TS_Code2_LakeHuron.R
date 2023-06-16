getwd()
setwd('C:/Users/승우/Desktop/4-2/시계열분석')


rm(list=ls(all=T))
source('TS-library.R')
data <- scan('huron.txt')


# plot the data
plot.ts(data)
title('Lake Huron Water level')


# Estimate 'Trend' and remove it from the observations.

#######################################################
########### Method1: 'Polynomial Regression'###########
#######################################################

# Suppose linear order

n <- length(data)
x <- seq(1, n, 1)
out.lm <- lm(data~1+x)
summary(out.lm)

# Draw fitted lines
plot.ts(data)
title('Lake Huron Water level')
lines(out.lm$fitted.values, col='red')

# See the residual plots
par(mfrow=c(2,2))
plot(out.lm) # I can see slight quadratic trend. 

# See ACF
par(mfrow=c(2,2))
acf2(out.lm$residuals) # Hmmm.. I think rather AR model might be needed 
title('sample ACF of residuals')

plot(out.lm$residuals[1:(n-1)], out.lm$residuals[2:n], 
     xlab='Y_{t-1}', ylab='Y_t')
title('Plot of residuals - Lag1')
# cor(out.lm$residuals[1:(n-1)], out.lm$residuals[2:n])

plot(out.lm$residuals[1:(n-2)], out.lm$residuals[3:n], 
     xlab='Y_{t-2}', ylab='Y_t')
title('Plot of residuals - Lag2')
# cor(out.lm$residuals[1:(n-2)], out.lm$residuals[3:n])

plot(out.lm$residuals[1:(n-3)], out.lm$residuals[4:n], 
     xlab='Y_{t-3}', ylab='Y_t')
title('Plot of residuals - Lag3')
# cor(out.lm$residuals[1:(n-3)], out.lm$residuals[4:n])



# Just fit quadratic model to compare with linear model (For fun)
out.lm2 <- lm(data~1+x+I(x^2))
summary(out.lm)
summary(out.lm2)

plot(out.lm)
plot(out.lm2)  # Statistics are lttle bit better but.... not really satisfying





###############################################
########### Method2: Moving Average ###########
###############################################

# Bandwidth selection first

h.ma <- optimize(f=ma.cv, interval=c(5, length(data)/2), 
                 Y = data, l=1, tol=.Machine$double.eps^0.25)

h.ma$minimum # about 32

out.ma <- smooth.ma(data, h.ma$minimum)
par(mfrow=c(1,3))
plot.ts(data)
lines(out.ma, col='red')
title('Detrend - MA')

plot.ts(data-out.ma)
title('Residuals - MA')
  
acf2(data-out.ma)
title('SACF of Residuals - MA') # Not realy satisfying for this result
