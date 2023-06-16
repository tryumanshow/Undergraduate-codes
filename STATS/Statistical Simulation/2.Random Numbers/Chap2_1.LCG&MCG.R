# LCG

LCG <- function(gen, init, a, c, m){
  
  # gen: the number of random numbers to generate 
  # init: initial seed to start with
  # a: 'a' part in LCG
  # c: 'c' part in LCG
  # m: mod part in LCG
  
  x <- NULL
  cnt <- 0 # The number of elements in 'x' vector
  
  for (i in 1:gen){
    
    cnt = cnt + 1
    
    temp <- (a * init + c) %% m
    x <- c(x, temp)
    init <- temp
  
    if (i > 1){
      if (x[1] / m == temp / m){
        x <- x[1:cnt-1]
        break
      }
    }
  }
  
  return(sort(x/m))
}

LCG(32, 4, 5, 4, 16)
LCG(1000, 0, 1664525, 1013904223, 2^32) # Recommended number in LCG


###############################################################
# LCG2

LCG_rep <- function(gen, init, a, c, m){

  last_result <- NULL
  
  # 일단 위의 LCG 함수로 random number 추출 iteration 한 번 돌림
  temp <- LCG(gen, init, a, c, m)
  last_result <- temp
  
  # 첫 iteration 때 이미 요구하는 개수를 넘은 경우
  if (length(last_result) >= gen){ 
    last_result <- last_result[1:gen]
  }

  # 첫 iteration 때 이미 요구하는 개수를 못 넘은 경우 계속 iteration
  else{
    while(length(last_result) < gen){
      init <- proc.time()[3]
      temp <- LCG(gen, init, a, c, m)
      last_result <- c(last_result, temp)
      last_result <- unique(last_result)
    } 
    last_result <- last_result[1:gen]
  }

  return(sort(last_result))
}

LCG_rep(32, floor(proc.time()[3]), 5, 4, 16)

LCG_rep(100, floor(proc.time()[3]), 1664525, 1013904223, 2^32)


#####################################################################
# MCG

MCG <- function(gen, init, a, m){
  
  # gen: the number of random numbers to generate 
  # init: initial seed to start with
  # a: 'a' part in LCG
  # m: mod part in LCG
  
  x <- NULL
  cnt <- 0 # The number of elements in 'x' vector
  
  for (i in 1:gen){
    
    cnt = cnt + 1
    
    temp <- a*init %% m
    x <- c(x, temp)
    init <- temp
    
    if (i > 1){
      if (x[1] / m == temp / m){
        x <- x[1:cnt-1]
        break
      }
      
      else next
    }
  }
  
  return(sort(x/m))
}

MCG(1000, 2, 7^5, 2^31-1)

###################################################################
# MCG _ Repetition

MCG_rep <- function(gen, init, a, m){
  
  last_result <- NULL
  
  # 일단 위의 LCG 함수로 random number 추출 iteration 한 번 돌림
  temp <- MCG(gen, init, a, m)
  last_result <- temp
  
  # 첫 iteration 때 이미 요구하는 개수를 넘은 경우
  if (length(last_result) >= gen){ 
    last_result <- last_result[1:gen]
  }
  
  # 첫 iteration 때 이미 요구하는 개수를 못 넘은 경우 계속 iteration
  else{
    while(length(last_result) < gen){
      init <- proc.time()[3]
      temp <- MCG(gen, init, a, m)
      last_result <- c(last_result, temp)
      last_result <- unique(last_result)
    } 
    last_result <- last_result[1:gen]
  }
  
  return(sort(last_result))
}

MCG_rep(1000, proc.time()[3], 5, 16)
hist(MCG_rep(10000, proc.time()[3], 5, 16))

# hist(MCG_rep(10000, proc.time()[3], 7^5, 2^31-1))



####################################################################



# If there's no need to consider whether same generation repeats:

# LCG
LCG <- function(n, I0, a, b, M){
  
  X <- NULL
  
  for (i in 1:n){
    Ij <- (a*I0 + b) %% M
    X <- c(X, Ij)
    I0 <- Ij
  }
  
  return(X/M)
}

LCG(32, 0, 1, 1, 16)


# MCG
MCG <- function(n, I0, a, M){
  
  X <- NULL
  
  for (i in 1:n){
    Ij <- (a*I0) %% M
    X <- c(X, Ij)
    I0 <- Ij
  }
  
  return(X/M)
}

MCG(32, 1, 2, 16)
