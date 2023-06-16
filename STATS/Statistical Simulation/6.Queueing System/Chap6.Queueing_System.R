# Queueing System

# 1. Single Server Queueing System
# 2. Queueing System with Two Servers in Sereis
# 3. Queueing System with Two Parallel Servers 


####################################
# 1. Single Server Queueing System #
####################################

set.seed(100)

# Intensity function
lambda.t <- function(t)
{
  tt <- t %% 480
  rate <- numeric(length(tt))
  rate[tt >= 0 & tt <= 120] <- 0.5
  rate[tt > 120 & tt <= 240] <- 1
  rate[tt > 240 & tt <= 360] <- 2
  rate[tt > 360 && tt <= 480] <- 1.5
  return(rate)
} 

# Subroutine for generating X ~ NPP(lambda(t))
NPPS <- function(lambda, s)
  # lambda: Maximum intensity function value in time interval [0,T]
  # s: Current time
{
  Z <- s
  U <- 2		# The first loop should be performed.
  ratio <- 1
  while (U > ratio)
  {
    Z <- Z + rexp(1,lambda)
    U <- runif(1)
    ratio <- lambda.t(Z)/lambda
  }
  return(Z)
}

# Single Server Queueing
# Our Interests:
# (1) Average time that a customer spends in the system.
# (2) Average time past T that the last customer departs.

SSQ <- function(lambdaA, lambdaD, Time)
  # lambdaA: Maximum arrival rate in [0,T] (NPP)
  # lambdaD: Service time rate (PP)
  # Time: System ending time
{
  t <- Na <- Nd <- n <- 0
  Xt <- NPPS(lambdaA,t)
  tA <- Xt
  tD <- Inf
  
  A <- NULL
  D <- NULL
  
  while (min(tA,tD) <= Time || n > 0)
  {
    if (tA <= tD && tA < Time)				# Case 1
    {
      t <- tA
      Na <- Na + 1
      n <- n + 1
      Xt <- NPPS(lambdaA,t)
      tA <- Xt
      if (n == 1) tD <- t + rexp(1,lambdaD)
      A <- c(A,t)
    } else if (tD < tA && tD <= Time)		# Case 2
    {
      t <- tD
      Nd <- Nd + 1
      n <- n - 1
      if (n == 0) tD <- Inf
      if (n > 0) tD <- t + rexp(1,lambdaD)	# Case 3
      D <- c(D,t)
    } else if (min(tA,tD) > Time && n > 0)
    {
      t <- tD
      Nd <- Nd + 1
      n <- n - 1
      if (n > 0) tD <- t + rexp(1,lambdaD)
      D <- c(D,t)
    }
  }
  # case 4:
  Tp <- max(t-Time,0)
  
  output <- list(A, D, Tp)
  names(output) <- c('A', 'D', 'Tp')
  return(output)
}

# Answer for Our Interests:
R <- 100
Q1 <- numeric(R)
Q2 <- numeric(R)
for (i in 1:R)
{
  ss <- SSQ(2, 1.5, 480)
  Q1[i] <- mean(ss$D - ss$A)
  Q2[i] <- ss$Tp
}

mean(Q1)
mean(Q2)

#################################################
# 2. Queueing System with Two Servers in Series #
#################################################

# Our interest:
# (1) Distribution of the amount of time that a customer 
# 	spends both at server 1 and at server 2.

QSTSS = function(lambdaA, lambda1, lambda2, T)
  # lambdaA: Maximum arrival rate in [0,T] (NPP)
  # lambda1: Service time rate of server 1 (PP)
  # lambda2: Service time rate of server 2 (PP)
  # T: System ending time
{
  t = Na = Nd = n1 = n2 = 0
  Xt = NPPS(lambdaA,t)
  tA = Xt
  t1 = Inf
  t2 = Inf
  
  A1 = NULL
  A2 = NULL
  D = NULL
  
  while (min(tA,t1,t2) <= T)
  {
    if (tA == min(tA,t1,t2))			# Case 1
    {
      t = tA
      Na = Na + 1
      n1 = n1 + 1
      Xt = NPPS(lambdaA,t)
      tA = Xt
      if (n1 == 1) t1 = t + rexp(1,lambda1)
      A1 = c(A1,t)
    } else if (t1 < tA && t1 <= t2)		# Case 2
    {
      t = t1
      n1 = n1 - 1
      n2 = n2 + 1
      if (n1 == 0) t1 = Inf
      if (n1 > 0) t1 = t + rexp(1,lambda1)
      if (n2 == 1) t2 = t + rexp(1,lambda2)
      A2 = c(A2,t)
    } else if (t2 < tA && t2 < t1)		# Case 3
    {
      t = t2
      Nd = Nd + 1
      n2 = n2 - 1
      if (n2 == 0) t2 = Inf
      if (n2 > 0) t2 = t + rexp(1,lambda2)
      D = c(D,t)
    }
  }
  output = cbind(A1[1:Nd],A2[1:Nd],D[1:Nd])
  return(output)
}

# Answer for Our Interests:
R = 100
Q1 = NULL
Q2 = NULL
for (i in 1:R)
{
  s = QSTSS(2, 1.5, 1, 480)
  Q1 = c(Q1, (s[,2] - s[,1]))
  Q2 = c(Q2, (s[,3] - s[,2]))
}

par(mfrow=c(1,2))
hist((s[,2] - s[,1]), xlab='Time', main='Service Time at Server 1')
hist((s[,3] - s[,2]), xlab='Time', main='Service Time at Server 2')

#############################################
# Queueing System with Two Parallel Servers #
#############################################

# Our interest:
# (1) Amount of time that each customner spends in the system.
# (2) The number of services performed by each server.

QSTPS = function(lambdaA, lambda1, lambda2, T)
  # lambdaA: Maximum arrival rate in [0,T] (NPP)
  # lambda1: Service time rate of server 1 (PP)
  # lambda2: Service time rate of server 2 (PP)
  # T: System ending time
{
  t = Na = C1 = C2 = 0
  n = i1 = i2 = 0
  Xt = NPPS(lambdaA,t)
  tA = Xt
  t1 = Inf
  t2 = Inf
  
  A = numeric(round(lambdaA*T))
  D = numeric(round(lambdaA*T))
  
  while (min(tA,t1,t2) <= T)
  {
    if (tA <= t1 && tA <= t2)		# Case 1
    {
      t = tA
      Na = Na + 1
      Xt = NPPS(lambdaA,t)
      tA = Xt
      A[Na] = t
      
      if (n == 0 && i1 == 0 && i2 == 0)
      {
        n = 1; i1 = Na
        t1 = t + rexp(1,lambda1)
      } else if (n == 1 && i1 > 0 && i2 == 0)
      {
        n = 2; i2 = Na
        t2 = t + rexp(1,lambda2) 
      } else if (n == 1 && i1 == 0 && i2 > 0)
      {
        n = 2; i1 = Na
        t1 = t + rexp(1,lambda1)
      } else if (n > 1)
      {
        n = n + 1
      }
    } else if (t1 < tA && t1 <= t2)	# Case 2
    {
      t = t1
      C1 = C1 + 1
      D[i1] = t
      
      if (n == 1)
      {
        n = 0; i1 = 0; i2 = 0
        t1 = Inf
      } else if (n == 2)
      {
        n = 1; i1 = 0
        t1 = Inf
      } else if (n > 2)
      {
        n = n - 1; i1 = max(i1,i2)+1
        t1 = t + rexp(1,lambda1)
      }
    } else if (t2 < tA && t2 < t1)	# Case 3
    {
      t = t2
      C2 = C2 + 1
      D[i2] = t
      
      if (n == 1)
      {
        n = 0; i1 = 0; i2 = 0
        t2 = Inf
      } else if (n == 2)
      {
        n = 1; i2 = 0
        t2 = Inf
      } else if (n > 2)
      {
        n = n - 1; i2 = max(i1,i2)+1
        t2 = t + rexp(1,lambda2)
      }
    }
  }
  output = list(A[1:(C1+C2)], D[1:(C1+C2)], C1, C2)
  names(output) = c('A', 'D', 'C1', 'C2')
  return(output)
}

# Answer for our interest:

R = 100
Q1 = numeric(R)
Q2.1 = numeric(R)
Q2.2 = numeric(R)
for (i in 1:R)
{
  s = QSTPS(2, 1.5, 1, 480)
  Q1[i] = mean(s$D - s$A)
  Q2.1[i] = s$C1
  Q2.2[i] = s$C2
}

mean(Q1)
mean(Q2.1)
mean(Q2.2)

################################################################
# Queueing system with parallel servers using 'simmer' package #
################################################################

install.packages('simmer')
install.packages('dplyr')
library(simmer)


# Intensity function
lambda.t = function(t)
{
  tt = t %% 480
  rate = numeric(length(tt))
  rate[tt >= 0 & tt <= 120] = 0.5
  rate[tt > 120 & tt <= 240] = 1
  rate[tt > 240 & tt <= 360] = 2
  rate[tt > 360 && tt <= 480] = 1.5
  return(rate)
} 

# Subroutine for generating interarrival times with NPP(lambda(t))

INPP = function(lambda, T)
  # lambda: Poisson process rate
  # T: Ending time
{
  N = rpois(1, (lambda*T))
  St = runif(N) * T
  St = sort(St)
  pt = lambda.t(St) / lambda
  s = St[runif(length(St)) <= pt]
  m = length(s)
  return(c(s[1],s[2:m] - s[1:(m-1)]))
}


# Single server queueing system

set.seed(100)

customer <-
  trajectory() %>%
  seize("server") %>%
  timeout(rexp(1, 1.5)) %>%
  release("server")
  
res <- NULL
for (i in 1:100)
{
  bank <-
    simmer("bank") %>%
    add_resource("server", 1) %>%
    add_generator("Customer", customer, function(){INPP(2,480)})
  
  bank %>% run(until = 480)
  
  result <- get_mon_arrivals(bank)  %>%   
    transform(total_time = end_time - start_time) 
  
  res = rbind(res,result)
}

hist(res$total_time, main='Total time spend in the bank',freq=F, xlab='Total time')


# Queueing system with two servers in series

set.seed(100)

customer <-
  trajectory() %>%
  seize("server1") %>%
  timeout(rexp(1, 1.5)) %>%
  release("server1") %>%
  seize("server2") %>%
  timeout(rexp(1, 1)) %>%
  release("server2")
  
res <- NULL
for (i in 1:100)
{
  bank <-
    simmer("bank") %>%
    add_resource("server1", 1) %>%
    add_resource("server2", 1) %>% 
    add_generator("Customer", customer, function(){INPP(2,480)})

  bank %>% run(until = 480)

  result <- get_mon_arrivals(bank, per_resource = T)  %>%   
            # per_resource: Event time by each resource
      transform(waiting_time = end_time - start_time) 
  
  res = rbind(res,result)
}

par(mfrow=c(1,2))
hist(res$waiting_time[res$resource=='server1'],
     main='Time for server1', freq=F,xlab='Waiting time')  
hist(res$waiting_time[res$resource=='server2'],
     main='Time for server2', freq=F,xlab='Waiting time')  
  
  
# Queueing system with two parallel servers 
# It is assumed that both server have the same average service time.

set.seed(100)

customer <-
  trajectory() %>%
  seize("server") %>%
  timeout(rexp(1, 1.5)) %>%
  # both servers have the same average service time.
  release("server")

res <- NULL
for (i in 1:100)
{
  bank <-
    simmer("bank") %>%
    add_resource("server", 2) %>%
    # two parallel servers
    add_generator("Customer", customer, function(){INPP(2,480)})
  
  bank %>% run(until = 480)
  
  result <- get_mon_arrivals(bank)  %>%   
    transform(total_time <- end_time - start_time) 
  
  res <- rbind(res,result)
}

hist(res$total_time, main='Total time spend in the bank',freq=F, xlab='Total time')




