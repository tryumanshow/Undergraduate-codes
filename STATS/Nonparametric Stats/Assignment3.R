library(tidyr)
library(dplyr)
library(DescTools)

# Question1

trt1 <- c(1.9, 0.9, 0.5, 0.7, 1.1)
trt2 <- c(1.7, 1.8, 0.2, 1.2, 1.5)
trt3 <- c(2.8, 2.3, 1.6, 2.0, 1.0)

data_q1 <- data.frame(trt1, trt2, trt3, stringsAsFactors = T)
# colnames(data_q1) <- c('1', '2', '3')
data_q1 <- data_q1 %>%
  gather(treatment, value)

data_q1.gathered <- data_q1


attach(data_q1)

treatment <- factor(treatment)
TukeyHSD(aov(value~treatment), 'treatment')

## 1-(a)

kruskal.test(value ~ treatment)
kruskal.test(value ~ treatment).statistic

#### Or calculate manually

length(c(trt1, trt2, trt3)) == length(unique(c(trt1, trt2, trt3))) # No ties

trt.rank <- rank(c(trt1, trt2, trt3))
data_q1.rank <- matrix(trt.rank, 3, 5, byrow=T)
rank.mean <- apply(data_q1.rank, 1, mean)
T.KW.manual <- 12/(15*16)*sum(5*(rank.mean-(15+1)/2)^2)
T.KW.manual

## 1-(b)
### chisq(0.05, 3-1) = 5.99
qchisq(0.95, 3-1) # Bigger than 4.34 (KW chi-squared)
 
## 1-(c)
treat.order <- ordered(treatment)
JT.output <- JonckheereTerpstraTest(x=value, g=treat.order, 
                                    alternative='increasing')
JT.output

#### Or calculate manually
expectation.JT <- (15^2 - 3*5^2)/4
var.JT <- (15^2*(2*15+3) - 3*(5^2*(2*5+3)))/72

pairs.trt12 <- expand.grid(trt1, trt2)
pairs.trt13 <- expand.grid(trt1, trt3)
pairs.trt23 <- expand.grid(trt2, trt3)


T.JT <- sum(pairs.trt12[,1]<pairs.trt12[,2], 
            pairs.trt13[,1]<pairs.trt13[,2],
            pairs.trt23[,1]<pairs.trt23[,2])

  
Z.score <- (T.JT - expectation.JT)/sqrt(var.JT)
Z.score

detach(data_q1)

#############################

# Question2

data_q1 <- t(data.frame(trt1, trt2, trt3, stringsAsFactors = T))

data_q1 <- cbind(data_q1, as.matrix(apply(data_q1, 1, mean)))

rank.data_q1 <- matrix(rank(c(data_q1[1,1:5],
                              data_q1[2,1:5],
                              data_q1[3,1:5])), 3, 5, byrow=T)
data_q1 <- cbind(data_q1, as.matrix(apply(rank.data_q1, 1, mean)))

colnames(data_q1)[6:7] <- c('value.mean', 'rank.mean')

## 2-(a)
obs.diff <- rank.diff <- NULL

for(i in 1:choose(3,2)){
  comb.list <- combn(3,2)
  temp1 <- abs(data_q1[,'value.mean'][comb.list[1,i]]-
                 data_q1[,'value.mean'][comb.list[2,i]])
  temp2 <- abs(data_q1[,'rank.mean'][comb.list[1,i]]-
                 data_q1[,'rank.mean'][comb.list[2,i]])
  
  obs.diff <- c(obs.diff, temp1)
  rank.diff <- c(rank.diff, temp2)
}

## 2-(b)
sample.variance <- c(0.292, 0.417, 0.468)
SSE <- 0

for (i in 1:length(sample.variance)){
  SSE <- SSE + 4*sample.variance[i]
}


SSE
MSE <- SSE /(15-3)

## 2-(c)
# : 3.77 

## 2-(d)
q.value <- 3.77*sqrt(MSE/5); q.value
obs.diff >= q.value


# attach(data_q1.gathered)
# TukeyHSD(aov(value~treatment), 'treatment')
# detach(data_q1.gathered)

q.value.rank <- 3.31*sqrt((15*16)/(12*5))
rank.diff >= q.value.rank
#############################

# Question3

trt1 <- c(100, 250, 50, 80)
trt2 <- c(112, 240, 58, 82)
data_q3 <- t(data.frame(trt1, trt2))
colnames(data_q3) <- c(1,2,3,4)
# data_q3 <- data.frame(trt1, trt2, pair=1:4) %>%
#   gather(treatment, value, -pair) 
# 
# data_q3 <- data_q3[,c('treatment', 'value', 'pair')]

## 3-(b)
critical.value <- mean(data_q3[1,]-data_q3[2,])

abs.diff <- abs(data_q3[1,]-data_q3[2,])
perm.distn <- NULL

sign_matrix <- matrix(NA, nrow=2^4, ncol=length(trt1))
sign_matrix[,1] <- rep(c(1,-1), each=8)
sign_matrix[,2] <- rep(c(1,-1), each=4)
sign_matrix[,3] <- rep(c(1,-1), each=2)
sign_matrix[,4] <- rep(c(1,-1))

for (i in 1:nrow(sign_matrix)){
  temp <- sign_matrix[i,] * abs.diff
  perm.distn <- c(perm.distn, mean(temp))
}

sum(perm.distn <= critical.value) / nrow(sign_matrix)

## 3-(c)

overall.rank <- rank(abs.diff)
d.i <- data_q3[1,] - data_q3[2,]
v.i <- ifelse(d.i > 0, 1, 0)
critical.value2 <- sum(overall.rank * v.i)

perm.signed.distn <- NULL

v.matrix <- ifelse(sign_matrix>0, 1, 0)

for (i in 1:nrow(v.matrix)){
  temp <- v.matrix[i,]*overall.rank
  perm.signed.distn <- c(perm.signed.distn, sum(temp))
}

perm.signed.distn
sum(perm.signed.distn <= critical.value2) / nrow(v.matrix)

#############################

# Question4 

# 뭔가 이상하긴 한데... rank 값들이라 그런가...

judge1 <- c(4, 3, 1, 2, 6, 5)
judge2 <- c(5, 2, 3, 1, 6, 4)
judge3 <- c(2, 5, 1, 3, 4, 6)
judge4 <- c(3, 4, 2, 1, 5, 6)

data_q4 <- data.frame(judge1, judge2, judge3, judge4)
data_q4['contestant_rank'] <- c('A', 'B', 'C', 'D', 'E', 'F')

data_q4 <- data_q4 %>%
  gather(judge, value, -contestant_rank)
data_q4 <- data_q4[,c('judge', 'contestant_rank', 'value')]
data_q4

attach(data_q4)
judge <- factor(judge)
contestant_rank <- factor(contestant_rank)

friedman.test(y=value, groups=judge, blocks=contestant_rank)
# friedman.test(value ~ judge|contestant_rank)

detach(data_q4)

### test manually 

data_q4 <- data.frame(judge1, judge2, judge3, judge4)
data_q4.temp <- t(data_q4)
data_q4.rank <- matrix(NA, 4, 6)

for (i in 1:6){
  data_q4.rank[,i] <- rank(data_q4.temp[,i])
}

denominator <- 0

for(i in 1:6){
  denominator <- denominator + var(data_q4.rank[,i])  
}

right <- apply(data_q4.rank, 1, mean)

T.ties <- 6^2 / denominator * (sum((right-3.5)^2))
pchisq(T.ties, 3)
