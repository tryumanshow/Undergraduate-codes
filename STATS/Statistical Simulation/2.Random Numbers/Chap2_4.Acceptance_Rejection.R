##########################################################################
# Case1: Discrete Version 

# X ~ p(x)
# x=1 -> p(x) = 0.1
# x=2 -> p(x) = 0.1
# x=3 -> p(x) = 0.4
# x=4 -> p(x) = 0.3
# x=5 -> p(x) = 0.1


# Proposal Dist'n 을 Discrete unif ~ (1, 5)로 하기로 결정 
# c=2로 하기로 결정


discrete_distn <- function(rv){
  
  if (rv==1) result <- 0.1
  else if (rv==2) result <- 0.1
  else if (rv==3) result <- 0.4
  else if (rv==4) result <- 0.3
  else if (rv==5) result <- 0.1
  else result <- 0
  
}


discrete_ar <- function(gen, start=1, end=5){
  
  discrete_result <- vector('numeric', gen)
  cnt_result <- vector('numeric', gen)
  
  c <- 2
  
  for (i in 1:gen){
    
    cnt <- 0 # To count total execution number
    
    while(1){
    
      unif <- runif(1, 0, 1)
      chosen_num <- floor(end * runif(1, 0, 1)) + 1
      target <- discrete_distn(chosen_num)
      threshold <- target / (c * (1/end))
        
      if (unif <= threshold){
        cnt = cnt+1
        discrete_result[i] <- chosen_num
        cnt_result[i] <- cnt
        break;
      }
      
      else cnt = cnt+1
    }
  }
  return(data.frame(discrete_result, cnt_result))
}

discrete_ar(1000, 1, 5)

result <- discrete_ar(1000, 1, 5)
hist(result[,1], probability = T)

mean(result[,2]) # C랑 상당히 유사!! 



##########################################################################
# Case2: Continuous Version 

# f(x)=20x(1-x)^3, 0<x<1 : Target Dist'n
# g(x)=1, 0<x<1 (Unif(0,1)): Proposal Dist'n 


continuous_distn <- function(gen){
  
  continuous_result <- vector('numeric', gen)
  cnt_result <- vector('numeric', gen)
  
  for (i in 1:gen){
    
    cnt = 0
    
    while(1){
    
      unif <- runif(1, 0, 1)
      proposal <- runif(1, 0, 1)
      threshold <- (64/135)*20*(proposal)*(1-proposal)^3*1
      
      if (unif <= threshold){
        cnt = cnt+1
        continuous_result[i] <- proposal
        cnt_result[i] <- cnt
        break;
      }
      
      else{
        cnt = cnt+1
      }
    }
  }
  
  return(data.frame(continuous_result, cnt_result))
  
}

continuous_distn(1000)

par(mfrow=c(1,2))
hist(continuous_distn(1000)[,1], freq=F)

x <- seq(0, 1, 0.001)
y <- 20*x*(1-x)^3
lines(x,y, col='blue')
lines(curve(20*x*(1-x)^3, 0, 1))

mean(continuous_distn(1000)[,2])
