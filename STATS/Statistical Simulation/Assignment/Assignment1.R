# 1 Consider the binomial distribution with a parameter (n=5, 0.2)

## 1-(1)

pb1_1 <- function(n, p, gen){
  
  # n: total number of trial 
  # p: 'success' probability in binomial distribution
  # gen: the number of random numbers to generate
  
  random_numbers <- runif(gen, min=0, max=1)
  cdf <- pbinom(q=0:n, size=n, prob=p)
  
  binom_rnum <- cut(random_numbers, breaks=c(0, cdf), labels=0:n)
  binom_rnum <- as.numeric(as.character(binom_rnum))
  return(binom_rnum)
}

pb1_1(5, 0.2, 100)

## pb1_2 

pb1_2 <- function(n, p, gen){

  random_sum_vector = vector('numeric', gen)
    
  for (i in 1:gen){
    
    random_numbers <- runif(n, min=0, max=1)
    random_numbers <- ifelse(random_numbers <= 1-p, 0, 1)
    rv_sum <- sum(random_numbers)
    
    random_sum_vector[i] <- rv_sum
  }
  
  return(random_sum_vector)
}

pb1_2(5, 0.2, 100)

# plot(table(pb1_2(5, 0.2, 100)), xlab='number of trials',
#      ylab='probability', main='binomial distribution')

## pb1_3

result_comparison <- matrix(NA, 2, 3)

result_comparison[1:2, 1] <- c(mean(pb1_1(5, 0.2, 100)),
                               var(pb1_1(5, 0.2, 100)))

result_comparison[1:2, 2] <- c(mean(pb1_2(5, 0.2, 100)),
                               var(pb1_2(5, 0.2, 100)))

result_comparison[1:2, 3] <- c(5*.2, 5*.2*.8)
colnames(result_comparison) <- c('Inv Trans Method',
                                 'Trans Method',
                                 'Theoretical Values')
result_comparison

###############################################################

# 2

pb2 <- function(lambda, gen){
  
  pois_vector <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    exp_gen = rexp(100, rate = lambda)
    
    if (exp_gen[1]>1) ind <- 0
    else ind = max(which((cumsum(exp_gen) <= 1))) 
    
    pois_vector[i] <- ind
  }
  
  return(pois_vector)
}

pb2(2, 100)

result_comparison2 <- matrix(NA, 2, 2)
result_comparison2[1:2, 1] <- c(mean(pb2(2, 100)), var(pb2(2, 100)))
result_comparison2[1:2, 2] <- c(2, 2)
colnames(result_comparison2) <- c('Random Number Generate', 'Theoretical')
result_comparison2


#################################################################

# 3

pb_3 <- function(gen){
  
  unif <- runif(gen, 0, 1)
  interest <- 1/sqrt(1-unif)
  
  return(interest)
}

pb_3(100)


hist(pb_3(100))

#################################################################

# 4

## 4-2

pb4 <- function(gen){
  
  random_numvec <- vector('numeric', gen)
  cnt_vec <- vector('numeric', gen)
  trial_vec <- vector('numeric', gen)
  
  total_trial <- 0
  
  for (i in 1:gen){
    
    cnt <- 0
    
    while(T)
    {
      unif <- runif(1, 0, 1)
      unif2 <- runif(1, 0, 1)
      nominator <- 6*unif2*(1-unif2)
      denominator <- 1.5 * 1
      threshold <- nominator / denominator  
      cnt <- cnt+1
      total_trial <- total_trial + 1
      
      if (unif <= threshold){
        random_numvec[i] <- unif2
        cnt_vec[i] <- cnt
        break
      }
    }
    
    if (i == gen) print(total_trial/100)
  }
  
  result <- data.frame(random_numvec, cnt_vec)
  return (result)
  
}
#$ 4-3
pb4(100)

hist(pb4(100)[,1], freq=F)
x <- seq(0, 1, 0.01)
y <- 6*x*(1-x)
lines(x, y, col='blue')


################################################################

# 5

pb5 <- function(gen){
  
  # gen: the number of random numbers to generate
  
  mu_vec <- as.matrix(c(0, 1, 2))
  sigma_mat <- matrix(c(1.0, -0.5, 0.5, -0.5, 1.0, -0.5, 0.5, -0.5, 1.0),
                      3, 3)
  
  L <- t(chol(sigma_mat))
  df <- matrix(NA, 3, gen)
  
  for (i in 1:gen){
  
    Z_vec <- as.matrix(rnorm(3, 0, 1))
    x_vec <- L %*% Z_vec + mu_vec
    df[,i] <- x_vec
  }

  pairs(t(df))
  return(t(df)) 
}

pb5(200)

