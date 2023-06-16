getwd()
setwd('C://Users//½Â¿ì//Desktop//Assignment6')

library(FNN)
library(splines)
library(boot)

# 1

## 1-(a)
data1 <- read.csv('height.csv', header=T)
height <- data1$x

min_max <- c(min(height), max(height))

par(mfrow=c(2,2))
hist(height, breaks=seq(min_max[1]-1, min_max[2]+1, 1), 
     main='2h=1')
hist(height, breaks=seq(min_max[1]-1, min_max[2]+1, 2),
     main='2h=2')
hist(height, breaks=seq(min_max[1]-1, min_max[2]+1, 4),
     main='2h=4')
hist(height, breaks=seq(min_max[1]-1, min_max[2]+1, 8),
     main='2h=8')

optimal.h <- 1.75/(length(height)^(1/3))*sd(height)
optimal.h

## 1-(b)
par(mfrow=c(2,3))
gaussian1 <- density(height, kernel='gaussian', bw=1)
gaussian2 <- density(height, kenrel='gaussian', bw=3)
gaussian3 <- density(height, kenrel='gaussian', bw=6)
epanechnikov1 <- density(height, kernel='epanechnikov', bw=1)
epanechnikov2 <- density(height, kernel='epanechnikov', bw=3)
epanechnikov3 <- density(height, kernel='epanechnikov', bw=6)

plot(gaussian1, main='Gaussian kernal with h=1')
plot(gaussian2, main='Gaussian kernal with h=3')                         
plot(gaussian3, main='Gaussian kernal with h=6')
plot(epanechnikov1, main='Epanechnikov kernal with h=1')
plot(epanechnikov2, main='Epanechnikov kernal with h=3')                         
plot(epanechnikov3, main='Epanechnikov kernal with h=6')



optimal.h <- 1.06/(length(height)^(1/5))*sd(height)
optimal.h

################################################################

# 2
## 2-(a)
data2 <- read.csv('vehicle.csv', header=T)

attach(data2)
par(mfrow=c(1,1))
plot(km.per.liter, topspeed)

lm.model <- lm(topspeed~km.per.liter)
abline(lm.model)

plot(lm.model)

## 2-(b)
liter.grid <- seq(range(km.per.liter)[1], 
                  range(km.per.liter)[2], 0.1)
knn.fit <- knn.reg(train=km.per.liter, test=data.frame(liter.grid),
                   y=topspeed, k=10)

plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='k=10')
lines(liter.grid, knn.fit$pred, col='steelblue', lwd=2)


knn.fit$pred[which(liter.grid==10)]
knn.fit$pred[which(liter.grid==15)]
knn.fit$pred[which(liter.grid==20)]

## 2-(c)
loess.fit <- loess(topspeed~km.per.liter, span=0.7)
plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='Loess')
lines(liter.grid, predict(loess.fit, 
                          data.frame(km.per.liter=liter.grid)), 
      col='steelblue', lwd=2)

estimate <- predict(loess.fit, newdata=data.frame(km.per.liter=c(10, 15, 20)))
names(estimate) <- c('10', '15', '20')
estimate

# predict(loess.fit, 10)
# predict(loess.fit, 15)
# predict(loess.fit, 20)

## 2-(d)
ns.fit <- lm(topspeed ~ ns(km.per.liter, df=3))
plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='Natural Spline')
lines(liter.grid, predict(ns.fit,
                          data.frame(km.per.liter=liter.grid)), 
      col='steelblue', lwd=2)

estimate <- predict(ns.fit, newdata=data.frame(km.per.liter=c(10, 15, 20)))
names(estimate) <- c('10', '15', '20')
estimate

## 2-(e)
plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='Three lines')
lines(liter.grid, knn.fit$pred, col='steelblue', lwd=2)
lines(liter.grid, predict(loess.fit, 
                          data.frame(km.per.liter=liter.grid)), 
      col='red', lwd=2)
lines(liter.grid, predict(ns.fit,
                          data.frame(km.per.liter=liter.grid)), 
      col='green', lwd=2)
legend(18, 250, c('KNN', 'Loess', 'Natural Spline'), 
       col=c('steelblue', 'red', 'green'),
       pch=c(10, 10, 10))


## 2-(f)
test.data <- data.frame(topspeed = c(163,171,179,145,195,166,171,161,166,156),
                       km.per.liter = c(16.9,13.4,16.6,19.,8.1,15.0,13.4,16.4,15.0,23.2))

knn.prediction <- vector('numeric', nrow(test.data))
for (i in 1:length(knn.prediction)){
  knn.prediction[i] <- knn.fit$pred[which(liter.grid==
                                            test.data$km.per.liter[i])]
}
MSE.KNN <- mean((knn.prediction-test.data$topspeed)^2)


loess.prediction <- predict(loess.fit, 
                            newdata=data.frame(km.per.liter=test.data$km.per.liter))
MSE.LOESS <- mean((loess.prediction-test.data$topspeed)^2)



ns.prediction <- predict(ns.fit,
                         newdata=data.frame(km.per.liter=test.data$km.per.liter))
MSE.NS <- mean((ns.prediction-test.data$topspeed)^2)

MSE.result <- c(MSE.KNN, MSE.LOESS, MSE.NS)
names(MSE.result) <- c('KNN', 'LOESS', 'Natural Cubic Spline')
MSE.result

detach(data2)

#############################################################

# 3
## 3-(a)
data3 <- data2

Loocv <- function(data, from=1, to=8){
  
  stopifnot(ncol(data)==2 & is.data.frame(data))
  overall_cnt <- to-from+1
  overall_MSE <- vector('numeric', overall_cnt)
  
  for (i in from:to){
    poly_model <- glm(topspeed ~ poly(km.per.liter, i), data=data)
    overall_MSE[i] <- cv.glm(data, poly_model)$delta[1]
  }
  return(overall_MSE)
}

Loocv.MSE <- Loocv(data3)
plot(Loocv.MSE, main='MSE plot')

## 3-(b)

Ten.fold <- function(data, from=1, to=8, K=10, B=10){
  
  stopifnot(ncol(data)==2 & is.data.frame(data))
  
  for_table = vector('numeric', B)
  index_vector <- from:to
  overall_cnt <- to-from+1
  overall_MSE <- vector('numeric', overall_cnt)
  
  for (j in 1:B){
    for (i in from:to){
      poly_model <- glm(topspeed ~ poly(km.per.liter, i), data=data)
      overall_MSE[i] <- cv.glm(data, poly_model, K=K)$delta[1]
    }
    for_table[j] <- index_vector[which(overall_MSE==min(overall_MSE))]
  }
  return(for_table)
}

degree <- Ten.fold(data3)
table(degree)

## 3-(c)
best.fit <- glm(topspeed ~ poly(km.per.liter, 2), data=data3)

plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='Polynomial Regression with df=2')
predicted.value <- predict(best.fit, newdata=data.frame(km.per.liter = liter.grid))
lines(liter.grid, predicted.value, col='steelblue', lwd=2)

estimate <- predict(best.fit, 
                    newdata=data.frame(km.per.liter=c(10, 15, 20)))
names(estimate) <- c('10', '15', '20')
estimate

## 3-(d)
plot(km.per.liter, topspeed, pch=16, col='grey80', 
     main='Two Plots')
lines(liter.grid, predicted.value, col='steelblue', lwd=2)
lines(liter.grid, predict(ns.fit,
                          data.frame(km.per.liter=liter.grid)), 
      col='green', lwd=2)
legend(18, 250, c('Polynomial', 'Natural Spline'), 
       col=c('steelblue', 'green'),
       pch=c(10, 10))

## 3-(e)
poly.prediction <- predict(best.fit,
                           newdata=data.frame(km.per.liter=test.data$km.per.liter))
mean((poly.prediction-test.data$topspeed)^2)

mean((ns.prediction-test.data$topspeed)^2)

