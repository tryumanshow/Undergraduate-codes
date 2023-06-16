#################################################
# Case1 ~ Case3 : Estimation of Point Estimator #
# Case4: Calculation of Convfidence Level       #
#################################################


##########################################################################
# Case1: Estimation of Standard Error using Monte Carlo method
# Suppose Pseudo-Pop'n ~ Standard Normal Distribution
# Suppose the estimator interested in: X bar

estimate_se <- function(n, m){
  
  # n: The # of observations in Pseudo-Pop'n
  # m: The # of estimates to generate
  
  simulated <- vector('numeric', m)
  
  for (i in 1:m){
    temp <- rnorm(n, 0, 1)
    temp_mean <- mean(temp)
    simulated[i] <- temp_mean
  }
  
  result <- sd(simulated)
  return(result)
}

estimate_se(10, 1000)


# Or in a more efficient way not using for loop:

estimate_se <- function(n, m){
  
  simulated <- vector('numeric', m)
  
  random_matrix <- matrix(rnorm(n*m, 0, 1), n, m)
  simulated <- apply(random_matrix, 2, mean)
  
  result <- sd(simulated)
  return(result)
}

estimate_se(10, 1000)


##########################################################################
# Case2: Estimation of MSE using Monte Carlo method
# Suppose being interested in Three estimators: mean, median, trimmed mean 
# Pseudo-Pop -> Exponential Distribution with rate=2


estimate_mse <- function(n, m, rate, k){
  
  # k: how many observations to trim?
  # if k is integer: the number of observations to trim. 
  # if k is probability: the percentage of observations to trim.
  
  simulated_mean <- vector('numeric', m)
  simulated_median <- vector('numeric', m)
  simulated_trimmed <- vector('numeric', m)
  
  for (i in 1:m){
    
    random <- rexp(n, rate=2)
    random <- sort(random)
    
    simulated_mean[i] <- mean(random)
    simulated_median[i] <- median(random)
    
    if (is.double(k)){
      simulated_trimmed[i] <- mean(random, trim=k) 
    }
    else {
      simulated_trimmed[i] <- mean(random[k+1:n-k])
    }
  }
  
  mean_mse <- mean((simulated_mean - (1/rate))^2)
  median_mse <- mean((simulated_median - (1/rate))^2)
  trimmed_mse <- mean((simulated_trimmed - (1/rate))^2)

  result <- c(mean_mse, median_mse, trimmed_mse)
  names(result) <- c('MSE of Mean', 'MSE of Median', 'MSE of Trimmed Mean')
  
  return(result)
}


estimate_mse(30, 1000, 2, 2)

##########################################################################
# Case3: Estimation of theta = E(|X1-X2|)
# X1, X2 ~ iid Standard Normal


estimate_theta <- function(n, m){

  simulated <- vector('numeric', m)
  
  for (i in 1:m){
    x1 <- rnorm(1, 0, 1)
    x2 <- rnorm(1, 0, 1)
    dif <- abs(x1-x2)
    simulated[i] <- dif
  }
  
  result <- c(mean(simulated), sd(simulated))
  return(result)
}

estimate_theta(10, 100)


##########################################################################
# Case4: 
# X1, ..., Xn ~ iid N(2, variance), n>=2
# Want to calculate the confidence level of sigma square

confi_lev <- function(n, m, p){
  
  # n: The number of (pseudo-)population observations 
  # m: The number of groups to generate
  
  simulated <- vector('numeric', m)
  
  for (i in 1:m){
    
    temp <- rnorm(n, 2, variance)
    threshold <- ((n-1)*sd(temp)) / qchisq(0.05, n-1)
    
    if(variance < threshold) simulated[i] <- 1
    else simulated[i] <- 0
  }
  return(simulated)
}


mean(confi_lev(30, 10, 5))
