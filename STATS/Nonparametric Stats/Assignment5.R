# install.packages('gtools')
library(gtools)
library(boot)

#1 
Mother <- c(65, 69, 65, 64, 66, 69, 64, 66, 60, 70, 66)
Daughter <- c(67, 64, 62, 64, 69, 70, 65, 66, 63, 74, 65)

## 1-(a)
# Originally, the thing that the problem demanded was Permutation, test, 
# but it was impossible to implement all 11! cases on my computer.

Daughter.length <- length(Daughter)
Daughter.order <- 1:Daughter.length
# Daughter.permutation <- permutations(n=Daughter.length,
                                     # r=Daughter.length,
                                     # v=Daughter.order)
# system.time(permutations(n=Daughter.length,
#                          r=Daughter.length,
#                          v=Daughter.order))

# So, instead, I chose random sampling with B=10000 (much less than 11!)

random.sampling.Pearson <- function(data, B){
  
  stopifnot(is.matrix(data)==TRUE)
  
  Threshold <- cor(data[,1], data[,2], method='pearson')
  Old.X <- data[,1]
  random.sampled.corr <- vector('numeric', B)
  
  for (i in 1:B){
    sample.index <- permute(1:nrow(data))
    New.Y <- data[sample.index, 2]
    random.Pearson <- cor(Old.X, New.Y, method='pearson')
    random.sampled.corr[i] <- random.Pearson
  }
  
  hist(random.sampled.corr)
  abline(v=Threshold)
  p.value <- sum(random.sampled.corr >= Threshold) / B
  return(p.value)
  
}

MD.matrix <- cbind(Mother, Daughter)
random.sampling.Pearson(MD.matrix, 10000)

## 1-(b)
MD.pearson <- function(data, B){
  
  row.num <- nrow(data)
  pearson.vector <- vector('numeric', B)
  
  for (i in 1:B){
    boot.row <- sample(row.num, replace=T)
    boot.data <- data[boot.row,]
    pearson.vector[i] <- cor(boot.data[,1], boot.data[,2],
                             method='pearson')
  }
  return(pearson.vector)
}

Boot.Pearson <- MD.pearson(MD.matrix, 10000)
Boot.Pearson
hist(Boot.Pearson)

## 1-(c)

### Using the function in 1-(b)
corr.lower <- quantile(Boot.Pearson, prob=0.025)
corr.upper <- quantile(Boot.Pearson, prob=0.975)

#### Percentile Method
corr.percentile <- c(corr.lower, corr.upper)
corr.percentile

#### Residual Method
original.corr <- cor(MD.matrix[,1], MD.matrix[,2], 
                     method='pearson')
corr.residual <- c(2*original.corr - corr.upper, 
                   2*original.corr - corr.lower)
names(corr.residual) <- sort(names(corr.residual))
corr.residual

### Using the boot function (Result: Similar)
corr.func <- function(x, d){
  cor(x[d,1], x[d,2], method='pearson')
}

boot.corr <- boot(MD.matrix, corr.func, R=1000)
boot.ci.corr <- boot.ci(boot.corr, type='all')
boot.ci.corr



## 1-(d)

c <- 2*1.96/sqrt(nrow(MD.matrix)-3)
Lower.Z.CI <- ((1+original.corr)*exp(-c)-(1-original.corr))/
  ((1+original.corr)*exp(-c)+(1-original.corr))
Upper.Z.CI <- ((1+original.corr)*exp(c)-(1-original.corr))/
  ((1+original.corr)*exp(c)+(1-original.corr))
Z.CI <- c(Lower.Z.CI, Upper.Z.CI)
Z.CI


###############################################################
 
# 2

MD.data.frame <- as.data.frame(MD.matrix)
lm.fit <- lm(Daughter ~ Mother, data=MD.data.frame)

residuals <- lm.fit$residuals
fitted.values <- lm.fit$fitted.values

beta1.residual <- function(data, d){
  
  residuals <- data[,1]
  fitted.values <- data[,2]
  rest <- data[,-c(1,2)][,1] # Mother column (Input) picking
  
  boot.residuals <- residuals[d]
  boot.fitted.values <- fitted.values + boot.residuals
  
  lm(boot.fitted.values ~ rest)$coefficients[2]
}

input.to.beta1.residual <- cbind(residuals, fitted.values, MD.matrix)

boot.beta1.residual <- boot(input.to.beta1.residual, beta1.residual, R=1000)
boot.ci(boot.beta1.residual, conf=0.9)

boot.beta1.residual
qqnorm(boot.beta1.residual$t)
qqline(boot.beta1.residual$t)

###############################################################

# 3

eosinophil <- c(55, 140, 91, 122, 111, 185, 203, 101, 
                76, 145, 95, 101, 196, 45, 299, 226, 
                65, 70, 196, 72, 121, 171, 151, 113, 
                112, 67, 276, 125, 100, 81, 122, 71, 
                158, 78, 162, 128, 96, 79, 67, 119)

get.three.at.once <- function(data, B){
  
  X.bar <- mean(data)
  boot.X.bar <- vector('numeric', B)
  
  for (i in 1:B){
    sample.index <- sample(1:length(data), replace=T)
    sampled.data <- data[sample.index]
    X.bar.cal <- mean(sampled.data)
    boot.X.bar[i] <- X.bar.cal
  }
  
  MSE <- mean((boot.X.bar - X.bar)^2)
  std.error <- sd(boot.X.bar)
  bias <- mean(boot.X.bar) - X.bar
  
  output <- c(MSE, std.error, bias)
  names(output) <- c('MSE', 'Standard error', 'Bias')
  
  return(output)
}

get.three.at.once(eosinophil, 10000)

###############################################################

# 4

## 4-(a)

for.t.pivot <- function(data, B){
  
  t.pivot <- vector('numeric', B)
  X.bar <- mean(data)
  n <- length(data)
  
  for (i in 1:B){
    sample.index <- sample(1:n, replace=T)
    sampled.data <- data[sample.index]
    t.pivot[i] <- (mean(sampled.data)-X.bar)/(sd(sampled.data)/sqrt(n))
  }
  
  t.percentiles <- quantile(t.pivot, probs=c(0.025, 0.975))
  t.pivot.ci <- c(X.bar - t.percentiles[2]*sd(data)/sqrt(n), 
                  X.bar - t.percentiles[1]*sd(data)/sqrt(n))
  
  names(t.pivot.ci) <- sort(names(t.pivot.ci))
  
  return(t.pivot.ci)
}

for.t.pivot(eosinophil, 10000)

## 4-(b)

mean.func <- function(data, d){
  mean(data[d])
}

boot.mean <- boot(eosinophil, mean.func, R=10000)
boot.ci(boot.mean)

# boot.mean
# qqnorm(boot.mean$t)
# qqline(boot.mean$t)
