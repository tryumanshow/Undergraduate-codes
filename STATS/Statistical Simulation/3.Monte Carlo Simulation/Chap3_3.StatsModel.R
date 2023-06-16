set.seed(0)
X1 <- seq(1, 30, 2)
X2 <- sample(100:120, 15)
n <- length(X1)
Y <- 1 - 0.5 * X1 + 0.7 * X2 + rnorm(n, 0,  10)

X <- cbind(1, X1, X2)
# suppose it is a observed data!


# Four Tasks to Do
# 1. (1-alpha)*100% CI for beta_1
# 2. (1-alpha)*100% CI for Mean Response
# 3. (1-alpha)*100% Prediction Interval for Observation
# 4. Test for H0: beta_1 = 0, H1: beta_1 != 0


out.lm <- lm(Y ~ 1 + X1 + X2)
summary(out.lm)
beta_hat <- out.lm$coefficients

MSE <- sum(out.lm$residuals^2)/(n-3) # If error ~ NIID, then might be used in error generation in repitition. 
se_beta <- MSE * solve(t(X)%*%X)

threshold <- beta_hat[2] / sqrt(se_beta[2,2])

m <- 100 # How much betas to generate

beta1_values <- vector('numeric', m)
mean_response_interval <- NULL
pred_interval <- vector('numeric', m)

for (i in 1:m){
  
  new_Y <- beta_hat[1] + beta_hat[2]*X1 + beta_hat[3]*X2 + 
    rnorm(n, 0, sqrt(MSE)) # Use MSE calculated from above
  
  new.lm <- lm(new_Y ~ 1 + X1 + X2)
  new_betas <- new.lm$coefficients 
  beta1_values[i] <- new_betas[2] # For Case 1
  mean_response_interval <- rbind(mean_response_interval, 
                                  new.lm$coefficients[1]+
                                    new.lm$coefficients[2]*X1 +
                                    new.lm$coefficients[3]*X2) # For Case 2
  pred_interval[i] <- new_betas[1] + new_betas[2]*10 + new_betas[3]*110
}


# For Case 1
beta1_values
hist(beta1_values) # kind of symmetric, so just use 0.25, 0.975 quantile
quantile(beta1_values, prob=c(0.025, 0.975))


# For Case 2 
mean_response
hist(mean_response[,1]) # kind of symmetric, so just use 0.25, 0.975 quantile
apply(mean_response, 2, quantile, prob=c(0.025, 0.975))

# For Case 3
pred_interval
hist(pred_interval)
quantile(pred_interval, prob=c(0.025, 0.975))


m <- 5000


# For Case 4
beta1_result <- vector('numeric', m)

for (i in 1:m){
  
  # Beta1 값을 뺀 체로 observation generation
  new_Y <- beta_hat[1] + beta_hat[3]*X2 + rnorm(n, 0, sqrt(MSE))
  new.lm <- lm(new_Y ~ 1 + X1 + X2) # 모델 생성시엔 X1 넣어주어야.
  new.beta <- new.lm$coefficients
  
  MSE <- sum(new.lm$residuals^2) / (n-3)
  denominator <- MSE * solve(t(X) %*% X)

  beta1_result[i] <- (new.beta[2] - 0) / sqrt(denominator[2,2])
}

mean(beta1_result > abs(threshold) | beta1_result < -abs(threshold))