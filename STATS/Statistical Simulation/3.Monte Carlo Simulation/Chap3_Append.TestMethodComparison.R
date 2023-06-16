# Task

# X1, ..., X20 ~ iid N(mu, 100)
# alpha = 0.05
# H0: mu=500, H1: mu>510
# Which method is better for testing this task? T? Z?

source("Chap3_2.Test.R")

##############
# For T-test #
##############

emp_t1error(m=10000, n=20, null_param=500) # Passed Level-alpha test
# Then deserves to calculate 'empirical power'.


mu1_seq <- seq(500, 520, 5)
t_test_power <- vector('numeric', length(mu1_seq)) # 500, 505, 510, 515, 520

for (i in 1:5){
  t_test_power[i] <- emp_power(m=10000, n=20, 
                            null_param=500, alter_param=mu1_seq[i])
}

t_test_power # Empirical Power for T-test 



##############
# For Z-test #
##############


emp_t1error_z <- function(m, n, null_param){
  
  empirical_table = vector('numeric', m)
  threshold <- qnorm(0.95, 0, 1) # 'Z-test' 
  
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

emp_t1error_z(m=10000, n=20, null_param=500) # Does not pass 'level-alpha test'



# So, no need to calculate the empirical power for comparison






#####################################################################





# Just for fun, plot the empirical power of two test methods on same problem
# t-test and Z-test

emp_power_z <- function(m, n, null_param, alter_param){
  
  empirical_table <- vector('numeric', m)
  threshold <- qnorm(0.95, 0, 1) 

  for (i in 1:m){
    
    random_number <- rnorm(n, alter_param, sqrt(100))
    statistics <- (mean(random_number)- null_param) / 
      (sd(random_number) / sqrt(n))
    
    if (statistics > threshold) empirical_table[i] <- 1
    else empirical_table[i] <- 0
  }
  
  result <- mean(empirical_table)
  return(result)
}

z_test_power <- vector('numeric', length(mu1_seq)) # 500, 505, 510, 515, 520

for (i in 1:5){
  z_test_power[i] <- emp_power(m=10000, n=20, 
                               null_param=500, alter_param=mu1_seq[i])
}

z_test_power # Empirical Power for T-test 



plot(mu1_seq, t_test_power, col='red', type='b')
lines(mu1_seq, z_test_power, col='blue')
legend(510, 0.6, legend=c('t-test', 'z-test'), lty=c(1,1), col=c('red', 'blue') )

# Though Z-test is better on empirical power test, in fact, 
# as mentioned above, because Z-test didn't pass the level-alpha test
# t-test is better in this case. 