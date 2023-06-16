# Various Discrete Event Simulations
# 1. Inventory Model
# 2. Insurance Risk Model
# 3. Epidemic Model: SIR


###################
# Inventory Model #
###################
# Our interest:
# (1) Expected profit by some fixed time T.

IM <- function(lambda, G, s, S, r, L, h, x0, T)
  # lambda: Appearance rate of demend (PP)
  # G: Amount demended by each customer (We assume Poisson(G))
  # s: Minimum inventory level
  # S: Maximum inventory level
  # r: Product price
  # L: Time until order is delivered
  # h: Inventory holding cost
  # x0: Current inventory level
  # T: System ending time
{
  # Cost function
  cost <- function (y) (y - 100)^3 + 1000000
  
  t <- C <- H <- R <- 0
  x <- x0
  y <- 0
  tA <- rexp(1,lambda)
  tD <- Inf
  
  while (min(tA,tD) <= T)
  {
    if (tA < tD)		# Case 1
    {
      H <- H + (tA - t)*x*h
      t <- tA
      D <- rpois(1,G)
      w <- min(D,x)
      R <- R + w*r
      x <- x - w
      if (x < s && y == 0)
      {
        y <- S - x
        tD <- t + L
      }
      tA <- t + rexp(1,lambda)
    } else if (tD <= tA)	# Case 2
    {
      H <- H + (tD - t)*x*h
      t <- tD
      C <- C + cost(y)
      x <- x + y
      y <- 0
      tD <- Inf
    }
  }
  profit <- R - C - H
  return(profit)
}

# Answer for our interest:

R <- 1000
profit <- numeric(R)
for (i in 1:R)
{
  profit[i] <- IM(lambda=5, G=10, s=50, S=100, r=20000, L=0.5, h=0.5, x0=100, T = 12)
} 
mean(profit)

########################
# Insurance Risk Model #
########################

# Our interest:
# (1) Probability that the company's capital is always nonnegative up to Time T.

IRM <- function(lambda, mu, nu, F, c, a0, n0, T)
  # lambda: Rate of claim (PP)
  # mu: Time that each existing policyholder remains with the company
  # nu: Arrival rate of new policyholders
  # F: Amount of each claim
  # c: Premium rate
  # a0: Initial capital
  # n0: Initial number of policyholders
  # T: System ending time
{
  t <- 0
  a <- a0
  n <- n0
  tE <- rexp(1,(nu + n*mu + n*lambda))
  
  while (tE <= T)
  {
    a <- a + n*c*(tE - t)
    t <- tE
    P1 <- nu / (nu + n*mu + n*lambda)
    P2 <- (n*mu) / (nu + n*mu + n*lambda)
    P3 <- (n*lambda) / (nu + n*mu + n*lambda)
    J <- sample(1:3, 1, prob=c(P1,P2,P3))
    if (J == 1) n <- n + 1
    if (J == 2) n <- n - 1
    if (J == 3)
    {
      Z <- rchisq(1, F)
      if (Z > a)
      {
        I <- 0
        break
      } else a <- a - Z
    }
    tE <- t + rexp(1,(nu + n*mu + n*lambda))
  }
  if (tE > T) I <- 1
  return(I)
}

# Answer for our interest:
R <- 10
P <- numeric(R)
for (i in 1:R)
{
  P[i] <- IRM(lambda=2, mu=0.1, nu=2, F=10, c=10, a0=85000, n0=100, T=365)
}
sum(P)/R


########################
#  SIR Epidemic Model  #
########################

# Our interests:
# 1. #'s of infected and recoved persons at time t
# 2. Duration of the infectious disease

SIR <- function(S,I,mu,lambda,T)
# S: # of susceptible at t=0
# I: # of infected at t=0
# mu: Rate that a susceptiable person is infected
# lambda: Rate that an infected person is recovered
# T: Ending time  
{
  t <- 0; R <- 0
  res <- c(t,S,I,R)
  while(t <= T)
  {
    X <- rexp(1,rate=(S*I*mu + I*lambda))
    t <- t + X
    P <- (S*I*mu) / (S*I*mu + I*lambda)
    J <- sample(1:2, 1, prob=c(P,1-P))
    if (J == 1)
    {
      S <- S - 1
      I <- I + 1
    }
    if (J == 2)
    {
      I <- I - 1
      R <- R + 1
    }
    res <- rbind(res,c(t,S,I,R))
    if (I == 0) break
  }
  colnames(res) <- c('Time','S','I','R')
  return(res)
}

Y <- SIR(S=1000,I=1,mu=0.001,lambda=0.15,T=100)


plot(Y[,1],Y[,2],type='l',lwd=2,xlab='Time',ylab='# of persons')
lines(Y[,1],Y[,3],lwd=2,col='red')
lines(Y[,1],Y[,4],lwd=2,col='blue')
legend(38,600,lwd=c(2,2,2),col=c('black','red','blue'),
       legend=c('Susceptible','Infected','Recovered'))

Y[nrow(Y),]



