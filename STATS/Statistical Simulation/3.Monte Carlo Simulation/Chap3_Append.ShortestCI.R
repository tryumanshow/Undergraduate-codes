# 95% Confidence Interval for asymetric case

shortest_ci <- function(simulated_data){

  alpha_seq <- seq(0.001, 0.049, 0.001)
  ci <- NULL
  
  for (j in alpha_seq){
    
    ci_temp <- quantile(simulated_data, prob=c(j, (0.95+j)))
    ci <- rbind(ci, ci_temp)
  }
  
  len <- ci[,2]-ci[,1]
  shortest <- ci[which.min(len),]
  
  return(shortest)
}


# If symmetric, just use the usual normal function

quantile(data, prob=c(0.025, 1-0.025))