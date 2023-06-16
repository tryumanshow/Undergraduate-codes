# Empirical Type-1 Error for Both-Sided Test

emp_t1error <- function(m, n, null_param){
  
  empirical_table = vector('numeric', m)
  threshold <- qt(0.975, n-1)
  
  for (i in 1:m){
    random_number <- rnorm(n, null_param, sqrt(100))
    statistic <- (mean(random_number)- null_param) /
      (sd(random_number)/sqrt(n))
    
    if (abs(statistic) > threshold) empirical_table[i] <- 1
    else empirical_table[i] <- 0
  }
  
  empirical_type1 <- mean(empirical_table)
  return(empirical_type1)
  
}

emp_t1error(100, 20, 500)


# Empirical Power for Both-Sided Test

emp_power <- function(m, n, null_param, alter_param){
  
  empirical_table <- vector('numeric', m)
  threshold <- qt(0.975, n-1) 
  
  for (i in 1:m){
    
    random_number <- rnorm(n, alter_param, sqrt(100))
    statistics <- (mean(random_number)- null_param) / 
      (sd(random_number) / sqrt(n))
    
    if (abs(statistics) > threshold) empirical_table[i] <- 1
    else empirical_table[i] <- 0
  }
  
  result <- mean(empirical_table)
  return(result)
}

emp_power(30, 20, 500, 510)
