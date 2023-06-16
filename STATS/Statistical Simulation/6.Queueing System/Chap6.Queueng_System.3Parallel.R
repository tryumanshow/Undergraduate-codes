
#############################################
# Queueing System with Two Parallel Servers #
#############################################

# Our interest:
# (1) Amount of time that each customner spends in the system.
# (2) The number of services performed by each server.



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



Three.Parallel = function(lambdaA, lambda1, lambda2, lambda3, T)
  # lambdaA: Maximum arrival rate in [0,T] (NPP)
  # lambda1: Service time rate of server 1 (PP)
  # lambda2: Service time rate of server 2 (PP)
  # T: System ending time
{
  t = Na = C1 = C2 = C3 = 0
  n = i1 = i2 = i3 = 0
  Xt = NPPS(lambdaA,t) #lambdaA: maximum 
  tA = Xt
  t1 = Inf
  t2 = Inf
  t3 = Inf
  
  A = numeric(round(lambdaA*T))
  D = numeric(round(lambdaA*T))
  
  while (min(tA,t1,t2,t3) <= T)
  {
    # Case 1: Arrival Event 
    if (tA <= t1 && tA <= t2 && tA <= t3)		
    {
      t = tA
      Na = Na + 1
      Xt = NPPS(lambdaA, t)
      tA = Xt
      A[Na] = t
      
      if (n == 0 && i1 == 0 && i2 == 0 && i3 == 0)
      {
        n = 1; i1 = Na
        t1 = t + rexp(1,lambda1)
      } 
      else if (n == 1 && i1 > 0 && i2 == 0 && i3 == 0)
      {
        n = 2; i2 = Na
        t2 = t + rexp(1,lambda2) 
      } 
      else if (n == 1 && i1 == 0 && i2 > 0 && i3 == 0)
      {
        n = 2; i1 = Na
        t1 = t + rexp(1,lambda1)
      } 
      else if (n == 1 && i1 == 0 && i2 == 0 && i3 > 0){
        n = 2; i1 = Na
        t1 = t + rexp(1,lambda1)
      } 
      else if (n == 2 && i1 == 0 && i2 > 0 && i3 > 0){
        n = 3; i1 = Na
        t1 = t + rexp(1,lambda1)
      } 
      else if (n == 2 && i1 > 0 && i2 == 0 && i3 > 0){
        n = 3; i2 = Na
        t2 = t + rexp(1,lambda2)
      } 
      else if (n == 2 && i1 > 0 && i2 > 0 && i3 == 0){
        n = 3; i3 = Na
        t3 = t + rexp(1,lambda3)
      }
      else if (n > 2)
      {
        n = n + 1
      }
    } 
    
    # Case 2: Departure at Server 1 (1st Parallel Server)
    else if (t1 < tA && t1 <= t2 && t1 <= t3)	
    {
      t = t1
      C1 = C1 + 1
      D[i1] = t
      
      if (n == 1)
      {
        n = 0; i1 = 0; i2 = 0; i3 = 0;
        t1 = Inf
      } 
      else if (n == 2){
        n = 1; i1 = 0 # i2, i3는 변화 x 뭐든지 간에 놔두면 돼 
        t1 = Inf
      } 
      else if (n == 3){
        n = 2; i1 = 0
        t1 = Inf
      }
      else if (n > 3){
        n = n - 1; i1 = max(i1,i2,i3)+1
        t1 = t + rexp(1,lambda1)
      }
    }
    
    # Case 2: Departure at Server 2 (2nd Parallel Server)
    else if (t2 < tA && t2 < t1 && t2 <= t3)	# Case 3
    {
      t = t2
      C2 = C2 + 1
      D[i2] = t
      
      if (n == 1)
      {
        n = 0; i1 = 0; i2 = 0; i3 = 0;
        t2 = Inf
      } 
      else if (n == 2)
      {
        n = 1; i2 = 0
        t2 = Inf
      } 
      else if (n == 3)
      {
        n = 2; i2 = 0 
        t2 = Inf
      }
      else if (n > 3)
      {
        n = n - 1; i2 = max(i1,i2,i3)+1
        t2 = t + rexp(1,lambda2)
      }
    }
    
  
    # Case3: Departure at Server 3 (3rd Parallel Server)
    else if (t3 < tA && t3 < t1 && t3 < t2)	# Case 3
    {
      t = t3
      C3 = C3 + 1
      D[i3] = t
      
      if (n == 1)
      {
        n = 0; i1 = 0; i2 = 0; i3 = 0;
        t3 = Inf
      } 
      else if (n == 2)
      {
        n = 1; i3 = 0
        t3 = Inf
      } 
      else if (n == 3){
        n = 2; i3 = 0 
        t3 = Inf
      }
      else if (n > 3)
      {
        n = n - 1; i3 = max(i1,i2,i3)+1
        t3 = t + rexp(1, lambda3)
      }
    }
  }

  # C1+C2+C3 명의 출과 입에 대한 결과 출력 
  output = list(A[1:(C1+C2+C3)], D[1:(C1+C2+C3)], C1, C2, C3)
  names(output) = c('A', 'D', 'C1', 'C2', 'C3')
  return(output)
}

R = 100
Q1 = numeric(R)
Q2.1 = numeric(R)
Q2.2 = numeric(R)
Q2.3 = numeric(R)

for (i in 1:R)
{
  s = Three.Parallel(2, 1.5, 1, 0.5, 480)
  Q1[i] = mean(s$D - s$A) # 고객들의 arrive~departure 평균 시간
  Q2.1[i] = s$C1 # Server1은 몇명을 소화하는가. 
  Q2.2[i] = s$C2 # Server2은 몇명을 소화하는가. 
  Q2.3[i] = s$C3 # Server3은 몇명을 소화하는가. 
}


mean(Q1)
mean(Q2.1)
mean(Q2.2)
mean(Q2.3)
