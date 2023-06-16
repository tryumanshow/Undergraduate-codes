########################################################################
#Case1: Box Muller Transformation 

Box_muller <- function(gen){

  if (gen%%2 == 0){
    cnt <- floor(gen/2) 
    indicate = 0
  }
  else {
    cnt <- floor(gen/2) + 1
    indicate = 1 
  }
  
  unif1 <- runif(cnt, 0, 1)
  unif2 <- runif(cnt, 0, 1)
  
  xs <- NULL
  
  for (i in 1:cnt){
    x1 <- sqrt(-2*log(unif2[i]))*cos(2*pi*unif1[i])
    x2 <- sqrt(-2*log(unif1[i]))*cos(2*pi*unif2[i])

    xs <- c(xs, x1, x2)
  }   
  
  if (indicate == 1) xs <- xs[1:length(xs)-1]
  
  return(xs)
}

Box_muller(100)
Box_muller(101)

hist(Box_muller(1000))
hist(Box_muller(1001))

########################################################################
# Case2: Chi-Square

chisq_random <- function(df, gen){
  
  # df: degree of freedom in chisquare (= number of Z-dist'n)
  # gen: # of chisq random numbers
  
  chisq_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    z <- Box_muller(df)
    z <- z^2
    chisq_numbers[i] <- sum(z)
  }
  
  return(chisq_numbers)
  
}

chisq_random(4, 100)

par(mfrow=c(1,2))
hist(chisq_random(4, 1000))
hist(rchisq(1000, 4))



## 정규분포의 built-in function을 사용할 거라면? 

chisq_random <- function(gen, n){
  
  chisq_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    z <- rnorm(n, 0, 1)
    chisq_numbers[i] <- sum(z^2)
  }
  
  return(chisq_numbers)
  
}


hist(chisq_random(1000, 7))
hist(rchisq(1000, 7))


########################################################################
# Case3: F-dist'n

f_random <- function(gen, m, n){
  
  f_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    temp1 <- rchisq(1, m)
    temp2 <- rchisq(1, n)
    
    f_numbers[i] <- (temp1/m) / (temp2/n)
  }
  
  return(f_numbers)
}

library(stats)
f_random(1000, 3, 2)
par(mfrow=c(1,2))
hist(f_random(1000, 3, 20))
hist(rf(1000, 3, 20))


########################################################################
# Case4: T dist'n

t_random <- function(gen, df){
  
  t_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    z <- rnorm(1, 0, 1)
    chi <- rchisq(1, df)
    
    t_numbers[i] <- z/(sqrt(chi/df))
  }
  
  return(t_numbers)
}

t_random(1000, 4)

hist(t_random(1000, 4))
hist(rt(1000, 4))


########################################################################
# Case5: Gamma dist'n

gamma_random <- function(gen, n, lambda){
  
  # gen: the number of random numbers
  # n: parameter of gamma distribution
  # lambda: parameter of gamma distribution
  
  gamma_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    temp <- rexp(n, rate=lambda)
    gamma_numbers[i] <- sum(temp)
  }
  
  return(gamma_numbers)
}

gamma_random(1000, 5, 2)

hist(gamma_random(1000, 5, 2))
hist(rgamma(1000, 5, 2))


# 아니면 Inverse Transformation이랑 섞어서 generating

gamma_random <- function(gen, n, lambda){
  
  gamma_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    U <- runif(n)
    X <- -(1/lambda)*log(1-U)
    gamma_numbers[i] <- sum(X)
  } 
  
  return(gamma_numbers)
}

gamma_random(1000, 5, 2)
hist(gamma_random(1000, 5, 2))


########################################################################
# Case6: Uniform dist'n Uniform(a,b) (Continuous Case)

unif_moved <- function(gen, a, b){
  
  unif_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    unif <- runif(1, 0, 1)
    moved_unif <- (b-a)*unif + a
    unif_numbers[i] <- moved_unif
    
  }
  return(unif_numbers)
}

unif_moved(100, 2, 4)
hist(unif_moved(10000, 2, 4))


########################################################################
# Case7: Beta distribution 

beta_random <- function(gen, r, s, lambda){
  
  beta_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    gamma_temp1 <- rgamma(1, shape=r, rate=lambda)
    gamma_temp2 <- rgamma(1, shape=s, rate=lambda)
    
    beta_numbers[i] <- gamma_temp1 / (gamma_temp1 + gamma_temp2)
  }
  
  return(beta_numbers)
 
}

beta_random(1000, 3, 5, 2)

par(mfrow=c(1,2))
hist(beta_random(1000, 3, 5, 2))
hist(rbeta(1000, 3, 5))


########################################################################
# Case8: Normal distribution 

normal_random <- function(gen, mu, sigma){
  
  normal_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    z <- rnorm(1, 0, 1)
    normal_numbers[i] <- mu + sigma * z
  }
  return(normal_numbers)
}

normal_random(1000, 2, 5)
hist(normal_random(1000, 2, 5))
hist(rnorm(1000, 2, 5))


########################################################################
# Case9: Log Normal Distribution

lognormal_random <- function(gen, mu, sigma){
  
  lognormal_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    Y <- normal_random(1, mu, sigma)
    lognormal_numbers[i] <- exp(Y)
    
  }
  
  return(lognormal_numbers)
}

hist(lognormal_random(1000, 2, 0.1))

library(stats)
hist(rlnorm(1000, 2, .1))


# 아니면 standard normal -> normal -> lognormal 순으로 갈 거라면

lognormal_random <- function(gen, mu, sigma){
  
  lognormal_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    std_z <- rnorm(1, 0, 1)
    trans_z <- mu + sigma * std_z
    exp_z <- exp(trans_z)
    
    lognormal_numbers[i] <- exp_z
  }
  
  return(lognormal_numbers)
}

hist(lognormal_random(1000, 2, 0.1))




########################################################################
# Case10: Binomial Distribution

binomial_random <- function(gen, num, p){
  
  binomial_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    ber <- ifelse(runif(num, 0, 1)<=1-p, 0, 1)
    binomial_numbers[i] <- sum(ber)
    
  }
  
  return(binomial_numbers)
}

binomial_random(1000, 5, 0.2)

par(mfrow=c(1,2))
hist(binomial_random(10000, 5, 0.2))
hist(rbinom(10000, 5, 0.2))


########################################################################
# Case11: Geometric Distribution

geom_random <- function(gen, p){
  
  geom_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    cnt <- 0
    while(1){
      unif <- runif(1, 0, 1)
      bern <- ifelse(unif <= 1-p, 0, 1)
      cnt = cnt + 1
      if (bern == 1) {
        geom_numbers[i] <- cnt
        break;
      }
    }
    
  }
  return (geom_numbers)
}

geom_random(1000, 0.5)
mean(geom_random(1000, 0.5))

hist(geom_random(1000, 0.5))
hist(rgeom(1000, 0.5))

mean(rgeom(1000, 0.5))


########################################################################
# Case12: Discrete Uniform

dis_uni_random <- function(gen, end){
  
  dis_uni_numbers <- vector('numeric', 0)
  
  for (i in 1:gen){
    unif <- runif(1, 0, 1)
    dis_uni_numbers[i] <- floor(end * unif)
  }
  
  return(dis_uni_numbers)
}

dis_uni_random(1000, 5)
hist(dis_uni_random(1000, 5))

########################################################################
# Case13: Poisson

pois_random <- function(gen, lambda){
  
  possion_vector <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    cnt <- 0
    exp_sum <- 0
    
    while(1){
      exp_random <- rexp(1, lambda)
      exp_sum <- exp_sum + exp_random 
      
      if (exp_sum > 1){
        possion_vector[i] <- cnt
        break
      }
      else{
        cnt <- cnt + 1
      }
    }
  }
  return(possion_vector)
}


pois_random(100, 2)


par(mfrow=c(1,2))
hist(pois_random(1000,2))
hist(rpois(1000, 2))


########################################################################
# Case14: Negative Binomial

negbin_random <- function(gen, r, p){
  
  negbin_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    geom_group <- rgeom(r, p)
    negbin_numbers[i] <- sum(geom_group)
  }
  
  return(negbin_numbers)
}

negbin_random(1000, 10, 0.2)

hist(negbin_random(1000, 10, .2))
hist(rnbinom(1000, 10, .2))