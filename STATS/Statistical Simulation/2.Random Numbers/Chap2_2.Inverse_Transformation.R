#####################################################
########### Inverse transformation method ###########
#####################################################

# Case1: Exponential 

exponential_inv <- function(n, lambda){
  
  U <- runif(n)
  X <- -(1/lambda)*log(1-U)
  return(X)
}

x <- exponential_inv(1000,1)

y <- rexp(1000, rate=1)

par(mfrow=c(1,2))
hist(x)
hist(y)


#########################################################################
# Case2: Bernoulli

bernoulli_inv <- function(n,p){
  
  U <- runif(n)
  X <- ifelse(U <= 1-p, 0, 1)
  return(X)
}

#########################################################################
# Case3: Binomial

 
binomial_inv <- function(gen, n, p){
 
  # gen: number of random numbers to generate
  # n: total number of trial
  # p: probability
  
  random_numbers <- runif(gen, 0, 1)
  cdf <- pbinom(0:n, n, p)
  
  binom_num <- cut(random_numbers, breaks=c(0, cdf), labels=0:n)
  binom_num <- as.numeric(as.character(binom_num))
  
  return(binom_num)
}

binomial_inv(100, 5, .2)


### Or, in a more efficient way (Professor's code)

n <- 5
p <- 0.2
Fx <- pbinom(0:n, n, p)

X <- NULL
for (i in 1:100){
  U <- runif(1)
  X <- c(X, sum(Fx < U))
}


########################################################################
# Case 4: Poisson

poisson_inv <- function(gen, lambda){
  
  random_numbers <- vector('numeric', gen)

  for (i in 1:gen){
    
    unif <- runif(1, 0, 1)
    cnt <- 0
    cdf <- dpois(0, lambda)
    next_p <- cdf
    
    while(1){
      
      if(unif <= cdf){
        random_numbers[i] <- cnt
        break
      }
      
      else{
        next_p <- (lambda/(cnt+1))*next_p
        cdf <- cdf + next_p
        cnt <- cnt+1
      }
    }
  }
  
  return(random_numbers)
}



mean(poisson_inv(1000, 2))

par(mfrow=c(1,2))
hist(poisson_inv(1000,2))
hist(rpois(1000, 2))


########################################################################
# Case 5: Geometric

geom_inv <- function(gen, p){
  
  random_numbers = vector('numeric', gen)
  
  for (i in 1:gen){
     
    unif <- runif(1, 0, 1)
    random_numbers[i] <- floor(log(unif)/log(1-p))+1
    
  }
  
  return(random_numbers)
}

geom_inv(1000, 0.5)
par(mfrow=c(1,2))
hist(geom_inv(1000, 0.5))
hist(rgeom(1000, 0.5))
mean(geom_inv(1000, 0.5))