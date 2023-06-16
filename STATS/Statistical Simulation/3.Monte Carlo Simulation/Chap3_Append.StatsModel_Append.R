# It is just my situation assumption

# There are three independent variables X1, X2

# To-do List
# 1. Get (1-alpha)% CI for beta_2
# 2. Get (1-alpha)% Confidence Interval for Mean Response
# 3. Get (1-alpha)% Prediction Interval for Specific observation
# 4. Test whether beta_2 is 0 or not 

# Caution: error is not 'NIID', but just following IID ``t with df 1``
# Suppose new observation for 3 is (10, 110)


# And Suppose there are 20 observations.


shortest_ci <- function(simulated_data){
  
  alpha_seq <- seq(0.001, 0.049, 0.001)
  ci <- NULL
  
  for (j in alpha_seq){
    
    ci_temp <- quantile(simulated_data, prob=c(j, (0.95+j)))
    ci <- rbind(ci, ci_temp)
  }
  
  len <- ci[,2]-ci[,1]
  shortest <- ci[which.min(len),]
  
  return(shortest)
}





set.seed(0)
X1 <- seq(1, 30, 2)
X2 <- sample(100:120, 15)
n <- length(X1)
Y <- 1 - 0.5 * X1 + 0.7 * X2 + rt(n, 1)

# Because I don't have data, I just generated the data,
# and suppose these as observed data.


Design_matrix <- cbind(1, X1, X2)
out.lm <- lm(Y ~ 1 + X1 + X2)
beta_temp <- out.lm$coefficients
MSE <- sum(out.lm$residuals^2) / (n-3)
sd.beta_temp <- MSE * solve(t(Design_matrix)%*%Design_matrix)

threshold <- (beta_temp[3] - 0) / (sqrt(sd.beta_temp[3,3]))# For 4
  

m <- 1000 # How many random numbers to generate

new_beta2 <- vector('numeric', m)
mean_resp <- NULL # Space for Mean Response 
pred_itv <- vector('numeric', m) # Space for Prediction Interval 


# 1.(1-alpha)% CI for beta_2

for (i in 1:m){
  
  new.Y <- beta_temp[1] + beta_temp[2]*X1 + beta_temp[3]*X2 + rt(n, 1)
  new.lm <- lm(new.Y ~ 1 + X1 + X2)
  new.beta <- new.lm$coefficients
  
  new_beta2[i] <- new.beta[3]
  mean_resp <- rbind(mean_resp, 
                     new.beta[1] + new.beta[2]*X1 + new.beta[3]*X2)
  pred_itv[i] <- new.beta[1] + new.beta[2]*10 + new.beta[3]*110
  
}


hist(new_beta2, breaks=100) 
shortest_ci(new_beta2)

# Drawing a plot, I think it is not really meaningful to get the confidence
# interval because dispersion is so small



# 4. Test whether beta_2 is 0 or not 

temp <- vector('numeric', m)

for (i in 1:m){
  
  new.Y <- beta_temp[1] + beta_temp[2]*X1 + rt(n,1)  # No X2
  new.lm <- lm(new.Y ~ 1 + X1 + X2)
  new.beta2 <- new.lm$coefficients[3]
  MSE2 <- sum(new.lm$residuals^2) / (n-3)
  sd.new_beta2 <- MSE * solve(t(Design_matrix) %*% Design_matrix)
  
  beta2.stats <- (new.beta2 - 0) / sqrt(sd.new_beta2[3,3])
  
  temp[i] <- ifelse(beta2.stats > abs(threshold) | 
                      beta2.stats < -abs(threshold), 1, 0)
  
}

mean(temp)

# beta2 is not 0 <- Consistent with the result of summary(out.lm)
