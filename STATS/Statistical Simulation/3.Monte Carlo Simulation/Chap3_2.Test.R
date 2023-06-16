###############################################################
# Case1: Test of parameters for observed data                 #
# Case2: Evaluation of test method (data: unobserved)         #
# Case3: Comparison between test methods  (data: unobserved)  #
###############################################################


# Case1: Test of parameters for observed data

# X1, ..., X10 ~ iid (Exp(rate))
# Data given (1.77, 0.78, 0.02, 0.18, 1.34, 0.15, 2.28, 0.65, 0.55, 0.46)
# Let theta = E(X)
# Let alpha = 0.05, use 't-statistics', H0: theta = 0.5, H1: theta > 0.5  


# Generate Empirical P-Value
emp_pvalue <- function(m, null_param, data){
  
  # m: the number of simulated dataset (bags)
  # data: given data (observed)
  # rate: the parameter to be used to generate null distribution.
  #       & gotten from null hypothesis. 
  
  empirical_table <- vector('numeric', m)
  rate <- 1/null_param #### 
  n <- length(data)
  
  threshold <- (mean(data) - null_param) / (sd(data)/sqrt(n))
  
  
  for (i in 1:m){
    # Data generation under H0 on 'Empirical P-value'
    random_numbers <- rexp(n, rate) ####
    statistic <- (mean(random_numbers) - null_param) / 
      (sd(random_numbers)/sqrt(n)) # t-statistics
    
    if (statistic > threshold) empirical_table[i] <- 1 ####
    else empirical_table[i] <- 0 
  }
  
  empirical_pvalue <- mean(empirical_table)
  return(empirical_pvalue)
}

emp_pvalue(100, 0.5, 
           c(1.77, 0.78, 0.02, 0.18, 1.34, 
             0.15, 2.28, 0.65, 0.55, 0.46))




# Result always changes, so 'Majority-Vote'


counting <- NULL

for (i in 1:10){
  
  result <- emp_pvalue(100, 0.5, 
                       c(1.77, 0.78, 0.02, 0.18, 1.34, 
                         0.15, 2.28, 0.65, 0.55, 0.46)) < 0.05
  counting <- c(counting, result)
  
}

mean(counting)



#########################################################################
#########################################################################

# Case2: Evaluation of test method (data: unobserved)   

# Suppose X1, ..., X20 ~ iid N(mu, 100)
# Suppose alpha=0.05
# Estimate Type 1 error rate. 
# H0: mu = 500, H1: mu > 500
# Test statistic: t_19


# Generate Empirical Type-1 Error Rate
emp_t1error <- function(m, n, null_param){
 
  empirical_table = vector('numeric', m)
  threshold <- qt(0.95, n-1) # t-test, one-sided ####
  
  for (i in 1:m){
    random_number <- rnorm(n, null_param, sqrt(100))
    statistic <- (mean(random_number)- null_param) /
      (sd(random_number)/sqrt(n)) # t-statistics
    
    if (statistic > threshold) empirical_table[i] <- 1
    else empirical_table[i] <- 0
  }
  
  empirical_type1 <- mean(empirical_table)
  return(empirical_type1)
  
}

emp_t1error(100, 20, 500)
# Almost always, the empirical Type1 error < alpha (0.05)
# T-test Method on Normal Distribution: Fine!




#########################################################################
# Case3: Comparison between test methods  (data: unobserved)
# Ignore comparison, just calculate Empirical Power

# Suppose X1, ..., X20 ~ iid N(mu, 100)
# alpha = 0.05, estimate power for a fixed mu1
# Use T-test, as well.
# H0: mu = 500, & H1: mu > 500


# Generate Empirical Power
emp_power <- function(m, n, null_param, alter_param){
  
  empirical_table <- vector('numeric', m)
  threshold <- qt(0.95, n-1) # t-test
  
  for (i in 1:m){
    
    random_number <- rnorm(n, alter_param, sqrt(100))
    statistics <- (mean(random_number)- null_param) / 
      (sd(random_number) / sqrt(n)) # t-statistics
    
    if (statistics > threshold) empirical_table[i] <- 1
    else empirical_table[i] <- 0
  }
  
  result <- mean(empirical_table)
  return(result)
}

emp_power(30, 20, 500, 505)