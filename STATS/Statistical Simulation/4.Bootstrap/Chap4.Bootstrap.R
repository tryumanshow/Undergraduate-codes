# Bootstrap 

# EX: Estimates of Standard Error and Bias using Bootstrap ==================

library(bootstrap)

law # law school data

# Sample Correlation
cor(law$LSAT, law$GPA)


#set up the bootstrap
m <- 10000
n <- nrow(law)
R <- numeric(m)

for (j in 1:m)
{
	i <- sample(1:n, size = n, replace = TRUE)
	boot.sample <- law[i,]
	R[j] <- cor(boot.sample[,1], boot.sample[,2])
}

# Estimated standard error of boostrap correlation estimator
sd(R)

# Empirical distribution of R
hist(R, prob = TRUE)

# Bias Estimation
mean(R) - cor(law$LSAT, law$GPA)


########## Bootstrap Confidence Intervals ##########

## EX: =============================================== 


# Sample Mean
theta.hat = mean(dat)

library(boot)

boot.f = function(x,i) mean(x[i])	
b.obj = boot(dat, statistic = boot.f, R=2000)
# Bootstrap samples
theta.j = as.vector(b.obj$t)


alpha=0.05

## Standard Normal Bootstrap Confidence Interval
LB = theta.hat - qnorm((1-alpha/2))*sd(theta.j)
UB = theta.hat + qnorm((1-alpha/2))*sd(theta.j)
c(LB,UB)


## Basic Bootstrap Confidence Interval
basic = 2*theta.hat - quantile(theta.j,prob=c((1-alpha/2),(alpha/2)))
basic


## Percentile Bootstrap Confidence Interval
perce = quantile(theta.j,prob=c((alpha/2),(1-alpha/2)))
perce


## R built-in function for CIs
boot.ci(b.obj, conf = 0.95, type = c("norm","basic","perc"))


# Ex: Bootstrap in Regression =====================================

# Regression: Y1 = 2 + 3 X1 - 0.5 X2 + epsilon
# epsilon's ~ N(0,1)
# Empirical (1-alpha)% Confidence Interval for beta1 & beta2: 

m=1000
alpha=0.05
X1 = 1:30
X2 = sample(1:30,30)
n = length(X1)
sigma = 5

Y = 2 + 3 * X1 - 0.5 * X2 +rnorm(n,0,sigma)
dat = data.frame(Y,X1,X2)
fit = lm(Y ~ X1+X2, data=dat)
X = cbind(1,X1,X2)
sigma2 = sum(fit$residuals^2) / (length(Y)-2-1)
betahat = fit$coefficients
VB = sigma2 * solve(t(X) %*% X)


# Bootstrap using pairs of obs. ------------------

beta = NULL
for (i in 1:m)
{
  idx = sample(1:n,n,replace=T)
  dt = dat[idx,]
  reg = lm(Y ~ X1+X2, data=dt)
  beta = rbind(beta,reg$coefficients[2:3])
}

# Original data
reg1 = lm(Y ~ X1+X2, data=dat)
summary(reg1)

# Standard error of beta
apply(beta,2,sd)

# Confidence interval for beta
apply(beta,2,quantile,prob=c(0.025,0.975))

# Empirical distribution of beta
b1 = seq(2.5,3.5,0.01)
b2 = seq(-1,0,0.01)


par(mfrow=c(1,2))
hist(beta[,1],xlab='Beta1',main='Beta1',freq=F)
lines(b1,dnorm(b1,mean=betahat[2],sd=sqrt(VB[2,2])),col='blue')
hist(beta[,2],xlab='Beta2',main='Beta2',freq=F)
lines(b2,dnorm(b2,mean=betahat[3],sd=sqrt(VB[3,3])),col='blue')


# Bootstrap using residuals -------------------

bhat = reg1$coefficient
r = reg1$residual

beta = NULL
for (i in 1:m)
{
  idx = sample(1:n,n,replace=T)
  res = r[idx]
  y = X %*% bhat + res
  dt = data.frame(y,X1,X2)
  reg = lm(y ~ X1+X2, data=dt)
  beta = rbind(beta,reg$coefficients[2:3])
}


# Standard error of beta
apply(beta,2,sd)

# Confidence interval for beta
apply(beta,2,quantile,prob=c(0.025,0.975))


# Empirical distribution of beta

par(mfrow=c(1,2))
hist(beta[,1],xlab='Beta1',main='Beta1',freq=F)
lines(b1,dnorm(b1,mean=betahat[2],sd=sqrt(VB[2,2])),col='blue')
hist(beta[,2],xlab='Beta2',main='Beta2',freq=F)
lines(b2,dnorm(b2,mean=betahat[3],sd=sqrt(VB[3,3])),col='blue')



