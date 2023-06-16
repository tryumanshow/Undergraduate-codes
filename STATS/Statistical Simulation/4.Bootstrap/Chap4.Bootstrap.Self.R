# Bootstrap

#############################################################
## 1. Estimate the Standard Error and Bias using Bootstrap ##
#############################################################

library(bootstrap)

law # The data I might use
cor(law$LSAT, law$GPA) # observed data


## The Function to get the correlation using Bootstrap Method


# The thing to put
  # 데이터
  # Boostrap 사이즈 결정
  # 아래 cor로 되어있는 걸, 검사하고 싶은 통계량으로 바꾸기. 


get.corr <- function(data, m){
  
  result <- vector('numeric', m)
  
  for (i in 1:m){
    idx <- sample(1:nrow(data), size=nrow(data), replace=T)
    boot.sample <- data[idx,]
    result[i] <- cor(boot.sample[,1], boot.sample[,2])
    
  }
  
  return(result)
}

cor.object <- get.corr(law, 1000)

cor.object
hist(cor.object, prob=T)

# Bias estimation 
bias <- mean(cor.object) - cor(law$LSAT, law$GPA)
bias/sd(cor.object) # < .25







#######################################
## 2. Bootstrap Confidence Interval  ##
#######################################

# Using the Package

library(boot)

# If interest is mean,

dat <- rchisq(30, df=2) # 시험용 데이터 


BCI <- function(data, m, alpha){
  
  library(boot)
  boot.f <- function(data, i) mean(data[i])
  boot.obj <- boot(data, statistic=boot.f, R=m)
  
  result <- boot.ci(boot.obj, conf=(1-alpha), type=c('norm', 'basic',
                                                   'perc'))
  
  return(result)
}

BCI(dat, 2000, 0.05)





# Just myself (Hard Coding)

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




get.bt.ci <- function(data, m, alpha){ # data는 matrix 형태라고 가정 
  
  result <- vector('numeric', m)
  
  if (is.vector(data) == T) {
    data <- as.matrix(data, nrow=length(data))
  }
  
  for (i in 1:m){
    
    idx <- sample(1:nrow(data), size=n, replace=T)
    boot.sample <- data[idx,]
    result[i] <- mean(boot.sample)  # Interest Part
  }
  
  
  # Standard Normal Bootstrap C.I
  LB.1 <- mean(data) - qnorm(1-alpha/2)*sd(result)
  UB.1 <- mean(data) + qnorm(1-alpha/2)*sd(result)
  
  # Basic C.I
  LB.2 <- 2*mean(data) - quantile(result, prob=c(0.025, 0.975))[2]
  UB.2 <- 2*mean(data) - quantile(result, prob=c(0.025, 0.975))[1]
  
  # Percentile C.I
  LB.3 <- shortest_ci(result)[1]
  UB.3 <- shortest_ci(result)[2]
  
  print(LB.3)

  mat <- matrix(c(LB.1, UB.1, LB.2, UB.2, LB.3, UB.3), 3, 2, byrow=T)
  
  return(mat)
  
}

dat <- rchisq(30, 2)
get.bt.ci(dat, 100, 0.05)







#########################
## 3. Hypothesis Test  ##
#########################
# 1. One sample Test: [H0: mu=mu0, H1: mu!=mu0]
# 2. Two sample Test (Equal Var): [H0: mu_x = mu_y, H1: mu_x != mu_y]
# 3. Two sample Test (Diff Var): [H0: mu_x = mu_y, H1: mu_x != mu_y]


# For example, 
dat <- rchisq(1000, df=2) # H0: mu=3

# Case1: One sample Test: [H0: mu=mu0, H1: mu!=mu0]
one.sample.test <- function(data, h0, m){

  # data: vector 형태 가정
  
  n <- length(data)
  result <- vector('numeric', m)
  threshold <- (mean(data) - h0) / (sd(data)/sqrt(n))
  
  data.transform <- (data - mean(data)) + h0
  
  for (i in 1:m){
    idx <- sample(1:n, size=n,replace=T)
    boot.sample <- data.transform[idx]
    t.stats <- (mean(boot.sample) - h0) / (sd(boot.sample)/sqrt(n))
  
    result[i] <- ifelse(t.stats > abs(threshold) |
                          t.stats < -abs(threshold), 1, 0)
  }

  return(mean(result))
  
}
one.sample.test(dat, 3, 1000)



# Case2: Two sample Test (Equal Var): [H0: mu_x = mu_y, H1: mu_x != mu_y]
two.sample.test.eq.var <- function(data1, data2, m){
  
  # data1, data2: 벡터 형태 가정 
  # 단, 두 data의 평균이 같다고 가정했음. 문제에 따라 변경할 것 
  
  n1 <- length(data1)
  n2 <- length(data2)
  
  result <- vector('numeric', m)
  
  sp <- sqrt(((n1-1)*sd(data1)^2 + (n2-1)*sd(data2)^2) / (n1+n2-2))
  threshold <- (mean(data1)-mean(data2)) / (sp*sqrt(1/n1 + 1/n2))
  
  data.transform <- c(data1, data2)
  
  for (i in 1:m){
    idx1 <- sample(1:n1, size=n1, replace=T)
    idx2 <- sample(1:n2, size=n2, replace=T)
    
    boot.sample1 <- data.transform[idx1]
    boot.sample2 <- data.transform[idx2]
    
    sp <- sqrt(((n1-1)*sd(boot.sample1)^2 + (n2-1)*sd(boot.sample2)^2) 
               / (n1+n2-2))
    t.stats <- (mean(boot.sample1)-mean(boot.sample2)) / 
      (sp*sqrt(1/n1 + 1/n2))
    
    result[i] <- ifelse(t.stats > abs(threshold) |
                          t.stats < -abs(threshold), 1, 0)
  }
  
  return(mean(result))
  
}
two.sample.test.eq.var(rchisq(500, 3), rchisq(500, 3), 1000)




# Case3: Two sample Test (Diff Var): [H0: mu_x = mu_y, H1: mu_x != mu_y]
two.sample.test.diff.var <- function(data1, data2, m){
  
  # data1, data2: 벡터 형태 가정 
  # 단, 두 data의 평균이 같다고 가정했음. 문제에 따라 변경할 것 
  
  n1 <- length(data1)
  n2 <- length(data2)
  
  result <- vector('numeric', m)

  threshold <- (mean(data1)-mean(data2)) / sqrt(sd(data1)^2/n1 +
                                                  sd(data2)^2/n2)
    
  z <- (sum(data1)+sum(data2))/(n1+n2)
  data1.transform <- data1 - mean(data1) + z
  data2.transform <- data2 - mean(data2) + z
  
  
  for (i in 1:m){
    idx1 <- sample(1:n1, size=n1, replace=T)
    idx2 <- sample(1:n2, size=n2, replace=T)
    
    boot.sample1 <- data1.transform[idx1]
    boot.sample2 <- data2.transform[idx2]
    
    denominator <- sqrt((sd(boot.sample1)^2)/n1 +
                          (sd(boot.sample2)^2)/n2)
      
    t.stats <- (mean(boot.sample1) - mean(boot.sample2)) / denominator
    result[i] <- ifelse(t.stats > abs(threshold) |
                          t.stats < -abs(threshold), 1, 0)
  }
  
  return(mean(result))
  
}
two.sample.test.diff.var(rchisq(500, 3), rchisq(500, 4), 1000)




###########################
## 4. Statistical Model  ##
###########################

setwd('C:\\Users\\승우\\Desktop')
data <- read.csv('dat.csv') # 이 데이터, col1: Y, col2: X1, col2: X2

set.seed(0)

# Four Tasks to Do
# 1. (1-alpha)*100% CI for beta_1
# 2. (1-alpha)*100% CI for Mean Response
# 3. (1-alpha)*100% Prediction Interval for Observation
# 4. Test for H0: beta_1 = 0, H1: beta_1 != 0

X1 <- data[,2]
X2 <- data[,3]

out.lm <- lm(Y ~ 1 + X1 + X2)
summary(out.lm)
beta_hat <- out.lm$coefficients
error <- out.lm$residuals

m <- 1000 # How much betas to generate
n <- nrow(data)

beta1.values <- vector('numeric', m)
mean.response <- NULL
pred.interval <- vector('numeric', m)

for (i in 1:m){
  
  index <- sample(1:n, n, replace=TRUE)
  shuffled_error <- error[index]
  
  new_Y <- beta_hat[1] + beta_hat[2]*X1 + beta_hat[3]*X2 + 
    shuffled_error # Use MSE calculated from above
  
  new.lm <- lm(new_Y ~ 1 + X1 + X2)
  new_betas <- new.lm$coefficients 
  beta1.values[i] <- new_betas[2] # For Case 1
  mean.response <- rbind(mean.response, 
                         new.lm$coefficients[1] +
                           new.lm$coefficients[2]*X1 + 
                           new.lm$coefficients[3]*X2) # For Case 2
  pred.interval[i] <- new_betas[1] + new_betas[2]*10 + new_betas[3]*110
}


# For Case 1
beta1.values
hist(beta1.values) # kind of symmetric, so just use 0.25, 0.975 quantile
quantile(beta1.values, prob=c(0.025, 0.975))


# For Case 2 
mean.response
hist(mean.response[,1]) # kind of symmetric, so just use 0.25, 0.975 quantile
apply(mean.response, 2, quantile, prob=c(0.025, 0.975))

# For Case 3
pred.interval
hist(pred.interval)
quantile(pred.interval, prob=c(0.025, 0.975))
