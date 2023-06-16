# Poisson Process  
# 1. Homogeneous Poisson Process
  # Method1: Usual Approach
  # Method2: Fast Approach
# 2. Non-homogeneous Poisson Process
  # Method1: Usual Approach
  # Method2: Fast Approach

# 1. Homogeneous Poisson Process -----------------------------------

# Method 1
PP1 <- function(lambda, Time)
  # lambda: Poisson process rate
  # Time: Ending time
{
  # I: Index of event
  # S: Event time
  t <- I <- 0
  S <- NULL
  
  while (t <= Time)
  {
    t <- t + rexp(1, rate=lambda)
    if (t > Time) break
    I <- I + 1
    S <- rbind(S, c(I,t))
  }
  return(S)
}

pp1 <- PP1(0.5, 100)
plot(pp1, type='l', xlab='Time', ylab='N(t)')


# Method 2 (Conditioning)
PP2 <- function(lambda, Time)
  # lambda: Poisson process rate
  # Time: Ending time
{
  N <- rpois(1, (lambda*Time))
  St <- runif(N) * Time
  St <- sort(St)
  S <- cbind(1:N, St)	
  return(S)
}

pp2 = PP2(0.5, 100)
plot(pp2, type='l', xlab='Time', ylab='N(t)')



par(mfrow=c(1,2))
plot(pp1, type='l', xlab='Time', ylab='N(t)'); title('PP from usual approach')
plot(pp2, type='l', xlab='Time', ylab='N(t)'); title('PP from fast approach')



# 2. Nonomogeneous Poisson Process-----------------------------------------

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


# Method 1
NPP1 <- function(lambda, Time)
  # lambda: Maximum intensity function value in time interval [0,T]
  # T: Ending time
{
  # I: Index of event
  # S: Event time
  t <- I <- 0
  S <- NULL
  while (t <= Time)
  {
    t <- t + rexp(1,lambda)
    if (t > Time) break
    U = runif(1)
    if (U <= (lambda.t(t)/lambda))
    {
      I <- I + 1
      S <- rbind(S, c(I, t))
    } 
  }
  return(S)
}

npp1 <- NPP1(2, 480)
plot(npp1, type='l', xlab='Time', ylab='N(t)')



# Method2

NPP2 <- function(lambda, Time)
  # lambda: Poisson process rate
  # T: Ending time
{
  N <- rpois(1, (lambda*Time))
  St <- runif(N) * Time
  St <- sort(St)
  pt <- lambda.t(St) / lambda
  S <- St[runif(length(St))<= pt]
  return(cbind(S,1:length(S)))
}


npp2 <- NPP2(2, 480)
plot(npp2, type='l', xlab='Time', ylab='N(t)')


par(mforw=c(1,2))
plot(npp1, type='l', xlab='Time', ylab='N(t)'); title('NPP from usual approach')
plot(npp2, type='l', xlab='Time', ylab='N(t)'); title('NPP from fast approach')
