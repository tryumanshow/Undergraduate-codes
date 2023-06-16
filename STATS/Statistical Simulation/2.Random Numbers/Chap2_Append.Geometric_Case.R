# Geometric Special Case

# 현재, 교안의 Geometric Dist'n에 대한, 
# Inverse Transformation, Transformation Method
# 둘다 기준이 한 번 성공하기까지 총 시도한 횟수!


# 하지만, 그 기준이 한 번 성공하기까지 총 실패 횟수라고 한다면?
# 실제로 geom과 관련한 built-in function은 실패 횟수를 기준으로 정의함


# Inverse Transformation Method. 

geom_random1 <- function(gen, p){
  
  geom_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    unif <- runif(1, 0, 1)
    geom_input <- floor(log(unif)/log(1-p)-1) + 1 # 여기 -1의 차이만 있을 뿐 
    geom_numbers[i] <- geom_input
  }
  
  return(geom_numbers)
}

geom_random1(1000, 0.5)
mean(geom_random1(1000, 0.5))


# Transformation Method

geom_random2 <- function(gen, p){
  
  geom_numbers <- vector('numeric', gen)
  
  for (i in 1:gen){
    cnt = 0 # 여기가 0이냐 1이냐 차이만 있음 (counting 기준만 다르기 때문)
    while(1){
      unif <- runif(1, 0, 1)
      bern <- ifelse(unif <= 1-p, 0, 1)
      
      if (bern == 1){
        geom_numbers[i] <- cnt
        break;
      }
      
      else{
        cnt <- cnt+1
      }
    }
  }
  return(geom_numbers)
}

geom_random2(1000, 0.5)
mean(geom_random2(1000, 0.5))

par(mfrow=c(1,3))
hist(geom_random1(1000, 0.5))
hist(geom_random2(1000, 0.5))
hist(rgeom(1000, 0.5))


data.frame(mean(geom_random1(1000, 0.5)),
           mean(geom_random2(1000, 0.5)),
           mean(rgeom(1000, 0.5)))


