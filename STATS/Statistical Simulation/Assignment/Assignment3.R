##################################################################

# 1

# 1-(1)


library(pander)
library(dplyr)

setwd('C:\\Users\\½Â¿ì\\Desktop')
data <- read.csv('dat.csv')

par(mfrow=c(2,2))
out.lm <- lm(Y~X1+X2, data=data)
out.lm.beta <- out.lm$coefficients
plot(out.lm)
par(mfrow=c(1,1)) 
# Normality assumption is violated
# So, I can use Bootstrap method to test whether coefficients are 0 or not.

# 1-(2)

pb1 <- function(data, which_beta, mu, m){
  
    n <- nrow(data)
    error <- out.lm$residuals
    stats_value <- vector('numeric', m)
    
    for(i in 1:m){
      
      index <- sample(1:n, n, replace=TRUE)
      shuffled_error <- error[index]
    
      if (which_beta == 1){
        
        threshold <- coef(summary(out.lm))[2,3]
        new_Y <- out.lm.beta[3]*data[,3] + shuffled_error
        new.lm <- lm(new_Y ~ data[,2] + data[,3])
        
        stats_value[i] <- coef(summary(new.lm))[2,3]
      }
    
      else{
        
        threshold <- coef(summary(out.lm))[3,3]
        new_Y <- out.lm.beta[2]*data[,2] + shuffled_error
        new.lm <- lm(new_Y ~ data[,2] + data[,3])
        
        stats_value[i] <- coef(summary(new.lm))[3,3]
      }
    }
    
    result <- sum(stats_value > abs(threshold) | 
                    stats_value < -abs(threshold)) / m
    return(result)
}


pb1_result <- c(pb1(data, 1, 0, 1000), pb1(data, 2, 0, 1000))
names(pb1_result) <- c('ASL of Beta1', 'ASL of Beta2')
pb1_result %>% pander

##################################################################

#2 

lambda.t <- function(t){
  
  t <- t %% 480
  value <- -0.0001*t^2 + 0.05*t
  return(value) 
}


NPPS <- function(lambda, s){
  
  # lambda: Maximum intensity function value in time interval [0,T]
  # s: Current time
  
  Z <- s
  U <- 2
  ratio <- 1
  while(U > ratio){
    
    Z <- Z + rexp(1, lambda)
    U <- runif(1)
    ratio <- lambda.t(Z) / lambda
  }
  return(Z)
}


Three.in.a.row <- function(lambdaA, lambda1, lambda2, lambda3, End_time){
  
  # t: current time
  # Na: # of arrival at time t
  # Nd: # of departure at time t
  # lambdaA: Maximum arrival rate in [0, T] (NPP)
  # lambda1: Service time rate of server1 (PP)
  # lambda2: Service time rate of server2 (PP)
  # lambda3: Service time rate of server3 (PP)
  
  t <- Na <- Nd <- n1 <- n2 <- n3 <- 0  
  Xt <- NPPS(lambdaA, t)
  tA <- Xt
  
  t1 <- t2 <- t3 <- Inf
  A1 <- A2 <- A3 <- D <- NULL
  
  while (min(tA, t1, t2, t3) <= End_time){
    
    # Case1: Arrival event occurs
    if (tA == min(tA, t1, t2, t3)){
      t <- tA
      Na <- Na + 1
      n1 <- n1 + 1
      Xt <- NPPS(lambdaA, t)
      tA <- Xt
      if (n1 == 1) t1 <- t + rexp(1, lambda1)
      A1 <- c(A1, t)
    }
    
    # Case2: Service completion at server1
    else if(t1 < tA && t1 <= t2 && t1 <= t3){
      t <- t1
      n1 <- n1 - 1
      n2 <- n2 + 1
      if (n1 == 0) t1 <- Inf
      if (n1 > 0) t1 <- t + rexp(1, lambda1)
      if (n2 == 1) t2 <- t + rexp(1, lambda2)
      A2 <- c(A2, t)
      
    }
    
    # Case3: Service completion at server2
    else if (t2 < tA && t2 < t1 && t2 <= t3){
      t <- t2
      n2 <- n2 - 1
      n3 <- n3 + 1
      if (n2 == 0) t2 <- Inf
      if (n2 > 0) t2 <- t + rexp(1, lambda2)
      if (n3 == 1) t3 <- t + rexp(1, lambda3)
      A3 <- c(A3, t)
    }
    
    # Case4: Service completion at server3
    else if (t3 < tA && t3 < t1 && t3 < t2){
      t <- t3
      Nd <- Nd + 1
      n3 <- n3 - 1
      if (n3 == 0) t3 <- Inf
      if (n3 > 0) t3 <- t + rexp(1, lambda3)
      D <- c(D, t)
    }
  }
  
  output <- cbind(A1[1:Nd], A2[1:Nd], A3[1:Nd], D[1:Nd])
  return(output)
}

R <- 100
pb2_matrix <- matrix(NA, R, 3)
colnames(pb2_matrix) <- c('mean at server1', 'mean at server2', 'mean at server3')

for (i in 1:R){
  pb2_result <- Three.in.a.row(6.25, 5, 4, 3.5, 480)
  pb2_matrix[i,1] <- mean(pb2_result[,2] - pb2_result[,1])
  pb2_matrix[i,2] <- mean(pb2_result[,3] - pb2_result[,2])
  pb2_matrix[i,3] <- mean(pb2_result[,4] - pb2_result[,3])
}


apply(pb2_matrix, 2, mean) %>%
  pander


##################################################################

# 3

lambda.t <- function(t){
  t <- t %% 480
  if (0 <= t && t <= 240) npp <- t/120 + 2
  if (240 < t && t <= 480) npp <- -t/120 + 6
  return (npp)
}

NPPS <- function(lambda, s){
  
  # lambda: Maximum intensity function value in time interval [0,T]
  # s: Current time
  
  Z <- s
  U <- 2
  ratio <- 1
  while(U > ratio){
    
    Z <- Z + rexp(1, lambda)
    U <- runif(1)
    ratio <- lambda.t(Z) / lambda
  }
  return(Z)
}


Three.in.a.parallel <- function(lambdaA, lambda1, lambda2, lambda3, End_time){
  
  # lambdaA: Maximum arrival rate in [0, T] (NPP)
  # lambda1: Service time rate of server1 (PP)
  # lambda2: Service time rate of server2 (PP)
  # lambda3: Service time rate of server3 (PP)
  
  t <- Na <- C1 <- C2 <- C3 <- 0
  n <- i1 <- i2 <- i3 <- 0
  Xt <- NPPS(lambdaA, t)
  tA <- Xt
  t1 <- Inf
  t2 <- Inf
  t3 <- Inf
  
  A <- numeric(round(lambdaA*End_time))
  D <- numeric(round(lambdaA*End_time))
  
  while (min(tA, t1, t2, t3) <= End_time){
    
    ## Case1: Arrival Event occurs
    if (tA <= t1 && tA <= t2 && tA <= t3){
      t <- tA
      Na <- Na + 1
      Xt <- NPPS(lambdaA, t)
      tA <- Xt
      A[Na] <- t
    
      if (n == 0 && i1 == 0 && i2==0 && i3==0){
        n <- 1
        i1 <- Na
        t1 <- t + rexp(1, lambda1)
      }
      else if (n == 1 && i1 > 0 && i2 == 0 && i3 == 0){
        n <- 2
        i2 <- Na
        t2 <- t + rexp(1, lambda2)
      }
      else if (n == 1 && i1 == 0 && i2 > 0 && i3 == 0){
        n <- 2
        i1 <- Na
        t1 <- t + rexp(1, lambda3)
      }
      else if (n == 1 && i1 == 0 && i2 == 0 && i3 > 0){
        n <- 2
        i1 <- Na
        t1 <- t + rexp(1, lambda1)
      }
      
      else if (n == 2 && i1 > 0 && i2 > 0 && i3 == 0){
        n <- 3
        i3 <- Na
        t3 <- t + rexp(1, lambda3)
      }
      
      else if (n == 2 && i1 > 0 && i2 == 0 && i3 > 0){
        n <- 3
        i2 <- Na
        t2 <- t + rexp(1, lambda2)
      }
      
      else if (n == 2 && i1 == 0 && i2 > 0 && i3 > 0){
        n <- 3
        i1 <- Na
        t1 <- t + rexp(1, lambda1)
      }
      
      else{
        n <- n + 1
      }
    }
    
    # Case2: Departure happens at Server1
    if (t1 < tA && t1 <= t2 && t1 <= t3){
      t <- t1
      C1 <- C1 + 1
      D[i1] <- t
    
      if (n==1){
        n <- 0
        i1 <- 0
        t1 <- Inf
      }
      
      else if (n==2){
        n <- 1
        i1 <- 0
        t1 <- Inf
      }
      
      else if (n==3){
        n <- 2
        i1 <- 0
        t1 <- Inf
      }
      
      else if (n>3){
        n <- n-1
        i1 <- max(i1, i2, i3) + 1
        t1 <- t + rexp(1, lambda1)
      }
    }
    
    # Case3: Departure happens at Server2
    if (t2 < tA && t2 < t1 && t2 <= t3){
      t <- t2
      C2 <- C2 + 1
      D[i2] <- t
      
    if (n==1){
      n <- 0
      i2 <- 0
      t2 <- Inf
    }
    else if (n==2){
      n <- 1
      i2 <- 0
      t2 <- Inf
    }
    else if (n==3){
      n <- 2
      i2 <- 0
      t2 <- Inf
    }
    else if (n>3){
      n <- n-1
      i2 <- max(i1, i2, i3) + 1
      t2 <- t + rexp(1, lambda2)
    }
    }
    
    # Case4: Departure happens at Server3
    if (t3 < tA && t3 < t1 && t3 < t2){
      t <- t3
      C3 <- C3 + 1
      D[i3] <- t
      
      if (n==1){
        n <- 0
        i3 <- 0
        t3 <- Inf
      }
      else if (n==2){
        n <- 1
        i3 <- 0
        t3 <- Inf
      }
      else if (n==3){
        n <- 2
        i3 <- 0
        t3 <- Inf
      }
      else if (n>3){
        n <- n-1
        i3 <- max(i1, i2, i3) + 1
        t3 <- t + rexp(1, lambda3)
      }
    }
  }
  output <- list(A[1:(C1+C2+C3)], D[1:(C1+C2+C3)], C1, C2, C3)
  names(output) <- c('A', 'D', 'C1', 'C2', 'C3')
  return(output)
}


R <- 1000

for (i in 1:R){
  pb3 <- Three.in.a.parallel(4, 1.5, 0.5, 1, 480)
  mean_stay_time[i] <- mean(pb3$D - pb3$A)
}

mean(mean_stay_time) 
