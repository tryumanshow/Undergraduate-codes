library(ggplot2)

# 1

year <- 2000:2008
drowing <- c(102, 102, 98, 85, 95, 96, 98, 123, 94)
films <- c(2, 2, 3, 1, 1, 2, 3, 4, 1)
data_pb1 <- data.frame(year, drowing, films)

## 1-(a) 
data_pb1$rank.drowing <- rank(drowing)
data_pb1$rank.films <- rank(films)

cor(data_pb1$rank.drowing, data_pb1$rank.films)

## 1-(b)
# n=9 , between 0.009, 0.007 => Reject null hypothesis

## 1-(c)
r.s <- cor(data_pb1$rank.drowing, data_pb1$rank.films)
critical.value <- r.s / sqrt(1/8)
critical.value
1-pnorm(critical.value)

########################################################

# 2
age <- c(3, 7, 15, 24, 85, 180, 360)
strength <- c(2500, 3200, 4300, 5300, 5900, 6700, 6900)
data_pb2 <- data.frame(age, strength)


## 2-(a)
ggplot(data=data_pb2, aes(x=age, y=strength, label=paste(age, strength))) +
  geom_point()+
  geom_text(hjust=-.1)

## 2-(b)
cor(age, strength, method='pearson')
cor(age, strength, method='spearman')
cor(age, strength, method='kendall')

### Or by manually: spearman
age.rank <- rank(age)
strength.rank <- rank(strength)

cor(age.rank, strength.rank)

### Or by manually: kendall
u <- vector('numeric', length(age))
comb <- choose(length(age), 2)
perm <- combn(length(age), 2)

for (i in 1:comb){
  index <- perm[,i]
  x.diff <- age[index][2]-age[index][1]
  y.diff <- strength[index][2]-strength[index][1]
  u[i] <- ifelse(x.diff*y.diff > 0, 1, 0)  
}

tau <- 2*sum(u)/comb - 1
tau
########################################################

# 3
data_pb3 <- matrix(c(4, 5, 1, 5, 8, 4, 1, 6, 10, 3, 3, 6), 
                   nrow=4, ncol=3, byrow=T)

rownames(data_pb3) <- c('30 or greater', '24-29', '18-23', '17 or less')
colnames(data_pb3) <- c('A', 'B', 'C')

## 3-(a)
expected.freq.table <- function(data){
  row.cnt <- nrow(data_pb3)
  col.cnt <- ncol(data_pb3)
  margin.added <- addmargins(data)
  expected.null <- matrix(NA, nrow=row.cnt, ncol=col.cnt)
  
  for (i in 1:row.cnt){
    for (j in 1:col.cnt){
      expected.null[i,j] <- margin.added[i, col.cnt+1] * 
        margin.added[row.cnt+1, j] / margin.added[row.cnt+1, col.cnt+1]
    }
  }
  return(expected.null)
}
 
exp.freq <- expected.freq.table(data_pb3)
rownames(exp.freq) <- c('30 or greater', '24-29', '18-23', '17 or less')
colnames(exp.freq) <- c('A', 'B', 'C')
exp.freq

get.chisq <- function(data){
  numerator <- (data_pb3 - expected.freq.table(data_pb3))^2
  denominator <- expected.freq.table(data_pb3)
  chisq.stats <- sum(numerator / denominator)
  
  return(chisq.stats)
}

get.chisq(data_pb3)

## 3-(b) bigger than 0.1

## 3-(c)

for.KW.transformation <- function(matrix_object){
  
  row.cnt <- nrow(matrix_object)
  col.cnt <- ncol(matrix_object)
  margins.added <- addmargins(matrix_object)
  
  rank.generated <- NULL
  
  for (i in 1:row.cnt){
    for (j in 1:col.cnt){
      if (matrix_object[i,j]==0){
        next
      } 
      else{  
        rank.generated <- c(rank.generated, rep(j, matrix_object[i,j]))
      } 
    }
  }
  
  rank.generated <- rank(rank.generated)

  transformed.to.list <- rep(list(NA), row.cnt)
  col.temp <- 0
  
  for (i in 1:row.cnt){
    if (i==1){
      transformed.to.list[[i]] <- rank.generated[1:margins.added[1,col.cnt+1]]
      col.temp <- col.temp + margins.added[1,col.cnt+1]
    }
    else{
      transformed.to.list[[i]] <- rank.generated[(col.temp+1):
                                          (col.temp+margins.added[i, col.cnt+1])]
      col.temp <- col.temp + margins.added[i,col.cnt+1]  
    }
  }
  return(transformed.to.list)
}

for.KW.transformation(data_pb3)
data_pb3.to.rank <- for.KW.transformation(data_pb3)

get.KW <- function(list_object){

  row.cnt <- length(list_object)
  n.i <- NULL
  rank.mean <- NULL
  for.variance <- NULL
  
  for (i in 1:row.cnt){
    n.i <- c(n.i, length(list_object[[i]]))
    rank.mean <- c(rank.mean, mean(list_object[[i]]))
    for.variance <- c(for.variance, list_object[[i]])
  }
  
  total.cnt <- length(for.variance)
  T.adj.KW <- sum(n.i*(rank.mean-((total.cnt+1)/2))^2)/var(for.variance)
  
  return(T.adj.KW)
  
}

get.KW(data_pb3.to.rank)
1-pchisq(get.KW(data_pb3.to.rank), df=3)


# 3-(d)

JT.stats <- function(list_object){
  
  object.cnt <- length(list_object)
  combi.two <- combn(object.cnt, 2)
  total.combi.cnt <- ncol(combi.two)
  
  T.JT <- 0
  
  Total.length <- 0
  row.cnt <- NULL
  
  for (i in 1:total.combi.cnt){
    chosen <- combi.two[,i]
    index1 <- chosen[1]
    index2 <- chosen[2]
    object1 <- list_object[[index1]]
    object2 <- list_object[[index2]]
    
    expanded_grid <- expand.grid(object1, object2)
    T.JT <- T.JT + sum(expanded_grid[,1] < expanded_grid[,2]) + 
      (1/2) * sum(expanded_grid[,1] == expanded_grid[,2]) 
  }
  
  for (i in 1:object.cnt){
    Total.length <- Total.length + length(list_object[[i]])
    row.cnt <- c(row.cnt, length(list_object[[i]]))
  }
  
  Expected.T.JT <- (Total.length^2 - sum(row.cnt^2))/4
  Var.T.JT <- (Total.length^2*(2*Total.length+3) - 
                 sum(row.cnt^2*(2*row.cnt+3)))/72
  
  Z.JT <- (T.JT - Expected.T.JT) / sqrt(Var.T.JT)
  
  return(Z.JT)
}

JT.stats(data_pb3.to.rank)
1-pnorm(JT.stats(data_pb3.to.rank))

########################################################

# 4
## 4-(a)

study1 <- matrix(c(83, 3, 72, 14), nrow=2, byrow=T)
study2 <- matrix(c(90, 3, 227, 43), nrow=2, byrow=T)
study3 <- matrix(c(129, 7, 81, 19), nrow=2, byrow=T)

matrix.list <- list(study1, study2, study3)

sep.odds <- function(list_object){
  
  stopifnot(is.list(list_object)==TRUE)
  odds_ratio <- vector('numeric', length(list_object))
  
  for (i in 1:length(list_object)){
    obj <- list_object[[i]]
    odds_ratio[i] <- (obj[1,1]*obj[2,2])/(obj[1,2]*obj[2,1])
  }
  
  return(odds_ratio)
  
}

sep.odds(matrix.list)

## 4-(b)

MH.estimate <- function(list_object){
  
  stopifnot(is.list(list_object)==TRUE)
  A <- 0
  B <- 0
  
  for (i in 1:length(list_object)){
    obj <- list_object[[i]]
    margins_added <- addmargins(obj)
    
    A <- A + obj[1,1]*obj[2,2]/margins_added[3,3]
    B <- B + obj[1,2]*obj[2,1]/margins_added[3,3]
  }
  
  result <- A/B
  return(result)
  
}

MH.estimate(matrix.list)

## 4-(c)

var.log.MH <- function(list_object){
  
  stopifnot(is.list(list_object)==TRUE)
  third.eq.numerator <- second.eq.numerator <- first.eq.numerator <- 0
  A <- B <- 0
  
  for (i in 1:length(list_object)){
    obj <- list_object[[i]]
    margins_added <- addmargins(obj)
    
    first.eq.numerator <- first.eq.numerator + 
      (obj[1,1]+obj[2,2])*(obj[1,1]*obj[2,2])/(margins_added[3,3]^2)
    
    second.eq.numerator <- second.eq.numerator + 
      ((obj[1,1]+obj[2,2])*(obj[1,2]*obj[2,1]) + 
         (obj[1,2]+obj[2,1])*(obj[1,1]*obj[2,2])) / (margins_added[3,3]^2)
    
    third.eq.numerator <- third.eq.numerator + 
      (obj[1,2]+obj[2,1])*(obj[1,2]*obj[2,1])/(margins_added[3,3]^2)
    
    A <- A + (obj[1,1]*obj[2,2])/margins_added[3,3]
    B <- B + (obj[1,2]*obj[2,1])/margins_added[3,3]
  }
  
  result <- first.eq.numerator / (2*A^2) + 
    second.eq.numerator / (2*A*B) +
    third.eq.numerator / (2*B^2)
  
  return(result)
}


var.log.MH(matrix.list)

## 4-(d)

MH <- MH.estimate(matrix.list)
Var.log.MH <- var.log.MH(matrix.list)

exp(log(MH)-1.96*sqrt(Var.log.MH))
exp(log(MH)+1.96*sqrt(Var.log.MH))

########################################################

# 5 

## 5-(a)

T.MH <- function(list_object){
  
  stopifnot(is.list(list_object)==TRUE)
  numerator.before.square <- denominator <- 0
  
  for (i in 1:length(list_object)){
    obj <- list_object[[i]]
    margins_added <- addmargins(obj)
    
    xk <- obj[1,1]
    expected.xk <- margins_added[1,3]*margins_added[3,1]/margins_added[3,3]
    var.xk <- (margins_added[1,3]*margins_added[2,3]*
      margins_added[3,1]*margins_added[3,2]) /
      ((margins_added[3,3]^2)*(margins_added[3,3]-1))
    
    numerator.before.square <- numerator.before.square + (xk-expected.xk)
    denominator <- denominator + var.xk
    
    print(xk)
    print(expected.xk)
    print(var.xk)
  }

  
  result <- (numerator.before.square)^2 / denominator
  return(result)
}

T.MH(matrix.list)

