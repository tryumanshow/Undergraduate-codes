# 1 (Need to carefully grasp what the 'estimator' is for true parameter)

pb1 <- function(n, m){
  
  random_number <- vector('numeric', m)
  
  for (i in 1:m){
    
    min_collection <- vector('numeric', n)
    
    exp1 <- rexp(n, rate=2)
    exp2 <- rexp(n, rate=3)
    exp_group <- rbind(exp1, exp2)
    min_exp <- apply(exp_group, 2, min)
    
    random_number[i] <- mean(min_exp)
    
  }
  return (random_number)
}

mc_values <- pb1(10, 1000)

sd(mc_values)
hist(mc_values)

# 95% Confidence Interval for asymetric case

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

shortest_ci(mc_values)

  
#########################################################


# 2 

pb2 <- function(pop=1, num, gen){
  
  # pop: choose between Chi-square, Uniform, and Exponential
  # num: the number of samples to generate in each simulation
  # gen: the number of simulating numbers
  
  pop_mean <- 1
  mc_values <- vector('numeric', gen)

  for (i in 1:gen){
    # Chi-square Dist'n ~ chi(1)
    if (pop == 1) distn <- rchisq(num, 1)
    
    # Uniform Dist'n ~ Unif(0,2)  
    else if (pop == 2) distn <- runif(num, min=0, max=2)
    
    # Exponential Dist'n ~ Exp(1)    
    else distn <- rexp(num, 1)
    
    temp <- (mean(distn) - pop_mean)/(sd(distn)/sqrt(num))
    mc_values[i] <- ifelse(abs(temp) >= qt(0.975, num-1), 1, 0)
  }
  
  result <- mean(mc_values)
  
  return(result)
}

pb2(1, 10, 100)
pb2(1, 30, 100)
pb2(1, 50, 100)

pb2(2, 10, 100)
pb2(2, 30, 100)
pb2(2, 50, 100)


pb2(3, 10, 100)
pb2(3, 30, 100)
pb2(3, 50, 100)


################################################################

# 3 (Need to grasp what is 'constant' and what is 'random')

pb3 <- function(gen, beta1){
  
  # gen: the number of beta1 values to generate
  # beta1: assumed beta1 
  
  x <- runif(30, 1, 10)
  error_part <- 36 / x
  w <- (1 / x)^(-1)
  
  beta1_for_ols <- vector('numeric', gen)
  beta1_for_wls <- vector('numeric', gen)
  

  for (i in 1:gen){
    
    y <- vector('numeric', 30)
    
    for (j in 1:30){ 
      y[j] <- 2 + 3*x[j] + rnorm(1, mean=0, sd=sqrt(error_part[j]))
    }
    
    lm.ols <- lm(y ~ 1 + x)
    lm.wls <- lm(y ~ 1 + x, weights=w)
    
    beta1_for_ols[i] <- lm.ols$coefficients[2]
    beta1_for_wls[i] <- lm.wls$coefficients[2]
    
  }  
  
  # result <- data.frame(beta1_for_ols, beta1_for_wls)
  
  MSE_for_ols <- mean((beta1_for_ols - beta1)^2)
  MSE_for_wls <- mean((beta1_for_wls - beta1)^2)

  result <- c(MSE_for_ols, MSE_for_wls)
  names(result) <- c('MSE_for_OLS', 'MSE_for_WLS')

  return(result)

}

result <- pb3(1000, 3)
result


# boxplot(x=result[,1])
# abline(h=3, col='red')
# title('Beta1_for_OLS')
# boxplot(x=result[,2])
# abline(h=3, col='blue')
# title('Beta1_for_WLS')
