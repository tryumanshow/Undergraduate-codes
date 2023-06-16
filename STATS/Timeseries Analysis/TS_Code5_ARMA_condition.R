## Check stationary condition

# Suppose this model: 
# X_t = 2.7607X_{t-1} - 3.8106X_{t-2} + 2.6535X_{t-3} - .9238X_{t-4} + Z_t

ch <- polyroot(c(1, -2.7607, 3.8106, -2.6535, .9238))
Mod(ch) # All of them are bigger than one! Being at the outside of unit root! Stationary!



## Inverting ARMA(p,q) to MA(infty)

# For example,  (1-B+.25B^2)X_t = Z_t + Z_{t-1}  : 
ARMAtoMA(ar=c(1.0, -0.25), ma=c(1), lag.max=10)


## Calculating theoretical ACF/PACF for ARMA(p,q)
ACF <- ARMAacf(ar=c(1.0, -0.25), ma=c(1), lag.max=10)
PACF <- ARMAacf(ar=c(1.0, -0.25), ma=c(1), lag.max=10, pacf=T)

n1 <- 0:10
n2 <- 1:10

par(mfrow=c(1,2))
plot(n1, ACF, type='h')
title('Theoretical ACF')

plot(n2, PACF, type='h')
title('Theoretical PACF')
