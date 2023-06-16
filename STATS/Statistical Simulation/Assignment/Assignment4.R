#(1)

pb1 <- function(lambda, time, alpha){
  
  # lambda: Device getting shocks rate
  # time: Ending time

  t <- 0 
  A <- NULL # Arrival 
  Dt <- NULL # D(t): the value interested in
  
  while (t <= time)
  {
    t <- t + rexp(1, rate=lambda)
    if (t > time) break
    A <- c(A, t) # Arrival Time 
    Di <- rchisq(1, 2)
    Dt <- c(Dt, Di*exp(-alpha*(time-t)))
  }

  return(sum(Dt))
}

pb1_result <- NULL
for (i in 1:10000){
  pb1_result <- c(pb1_result, pb1(2, 10, 3))
}

mean(pb1_result)



#(2)

pb2 <- function(exp.mean, chisq.df, time){
  
  # time: Ending time
  t <- 0
  
  start.point <- NULL
  send.complete <- NULL
  handle.complete <- NULL
  process <- NULL
  
  while(t < time){
    
    start <- t
    tE <- rexp(1, 3*(1/exp.mean)) # 1/exp.mean + 1/exp.mean + 1/exp.mean
    t <- t + tE
    
    size <- rchisq(1, chisq.df)
    main_com <- t + rexp(1, 1/(3*size))
    
    p1 <- p2 <- p3 <- 1/3
    J <- sample(1:3, 1, prob=c(p1, p2, p3))
    process <- c(process, J)
    
    start.point <- c(start.point, start)
    send.complete <- c(send.complete, t)
    handle.complete <- c(handle.complete, main_com)
    
    t <- main_com
    
    
  }
  

  result <- as.matrix(cbind(start.point, send.complete, handle.complete, process))
  result <- result[result[,'handle.complete']<time,]
  return(result)
}

one_case <- pb2(9, 2, 1000)
head(one_case, 5)
tail(one_case, 5)

expected_time <- NULL
for(i in 1:1000){
  
  
  tb <- pb2(9, 2, 1000)
  mean_tb <- mean(tb[,3]-tb[,1])
  
  expected_time <- c(expected_time, mean_tb)

}

mean(expected_time)

