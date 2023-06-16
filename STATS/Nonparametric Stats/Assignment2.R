library(DescTools)
library(tidyr)
library(dplyr)

# Question1

sec1 <- c(17, 14, 15, 19, 13)
sec2 <- c(11, 16, 8, 12)

## 1-(a)
all.pairs <- expand.grid(sec1, sec2)
all.pairs.diff <- all.pairs[,1] - all.pairs[,2]
pwd <- sort(all.pairs.diff)
pwd


## 1-(b)
U <- sum(all.pairs[,1] < all.pairs[,2])
U

## 1-(c)
k_a <- 3; k_b <- 18
pwd[k_a]
pwd[k_b] ## (-1, 8]

### Same result with 1-(c) 
library(DescTools)
HodgesLehmann(x=sec1, y=sec2, conf.level=0.9, na.rm=F) 


## 1-(d)
HL.est <- median(pwd)
HL.est

##############################################################

# Question2 

section.all <- c(sec1, sec2)
section.sort <- sort(section.all)

emp.F1 <- ecdf(sec1)(section.sort)
emp.F2 <- ecdf(sec2)(section.sort)
KS.stat <- max(abs(emp.F1-emp.F2)); KS.stat

## 2-(a)


KS.simulation <- function(group1, group2, num_of_sample){
  
  choose.sec1 <- combn(length(group1), num_of_sample)
  choose.sec2 <- combn(length(group2), num_of_sample)
  
  B1 <- choose(length(group1), num_of_sample)
  B2 <- choose(length(group2), num_of_sample)
  
  Test.stat.interest <- rep(NA, B1*B2)
  iteration.index <- 0
  
  for(i in 1:B1){
    T1.ind <- choose.sec1[,i]
    temp.sec1 <- group1[T1.ind]
    for (j in 1:B2){
      iteration.index <- iteration.index + 1 
      
      T2.ind <- choose.sec2[,j]
      temp.sec2 <- group2[T2.ind]
      
      temp.sec.all <- c(temp.sec1, temp.sec2)
      temp.sec.sort <- sort(temp.sec.all)
      
      temp.emp.F1 <- ecdf(temp.sec1)(temp.sec.sort)
      temp.emp.F2 <- ecdf(temp.sec2)(temp.sec.sort)
      
      temp.interest <- max(abs(temp.emp.F1 - temp.emp.F2))
      Test.stat.interest[iteration.index] <- temp.interest
    }
  }
  return(Test.stat.interest)
}

KS.simulation(sec1, sec2, 2)
sum(KS.simulation(sec1, sec2, 2) >= KS.stat) / 
  length(KS.simulation(sec1, sec2,2)) 

## 2-(b)
KS.simulation(sec1, sec2, 3)
sum(KS.simulation(sec1, sec2, 3) >= KS.stat) / 
  length(KS.simulation(sec1, sec2,3)) 



##############################################################

# Question3

speciesA <- c(5.1, 9.4, 7.2, 8.1, 8.8)
speciesB <- c(2.5, 4.2, 6.9, 5.5, 5.3)

## 3-(a)
all.species <- c(speciesA, speciesB)
sort.species <- sort(all.species)
emp.F1 <- ecdf(speciesA)(sort.species)
emp.F2 <- ecdf(speciesB)(sort.species)


emp.F1; emp.F2
KS.stat <- max(abs(emp.F1 - emp.F2)); KS.stat

## 3-(b)
all.perms <- combn(10, 5)
B <- choose(10, 5)

test.stat <- rep(NA, B)

for(i in 1:B){
  sp1.ind <- all.perms[,i]
  new.speciesA <- sort.species[sp1.ind]
  new.speciesB <- sort.species[-sp1.ind]
  emp.tempF1 <- ecdf(new.speciesA)(sort.species)
  emp.tempF2 <- ecdf(new.speciesB)(sort.species)
  temp.KS.stat <- max(abs(emp.tempF1-emp.tempF2))
  test.stat[i] <- temp.KS.stat
}

table(test.stat)
sum(test.stat >= KS.stat) / B

#############################################################


# Question 4

getwd()
femur <- read.csv('femur.csv')

# 4-(a)
femur.stats <- femur %>% 
  group_by(class) %>%
  summarise(n=length(femur_load),
            mean=mean(femur_load), 
            var=var(femur_load))

femur.total_mean <- mean(femur[,'femur_load'])

SST <- SSE <- 0

for (i in 1:nrow(femur.stats)){
  SST <- SST + femur.stats[i, 'n']*
    (femur.stats[i, 'mean'] - femur.total_mean)^2   
  SSE <- SSE + (femur.stats[i, 'n']-1)*
    (femur.stats[i, 'var'])
}

K <- 5
F.stats.obs <- as.numeric((SST/(K-1))/(SSE/(nrow(femur)-K)))

## For the Permutation

indicator.vector <- NULL

temp.class <- femur.stats %>% pull('class')
temp.n <- femur.stats %>% pull('n')

for (i in 1:length(temp.class)){
  temp <- rep(temp.class[i], temp.n[i])
  indicator.vector <- c(indicator.vector, temp)
}


femur.load.indexing <- femur[,'femur_load']
B <- 10000 # Replication number
Fstat.dist <- rep(NA, B)

for(i in 1:B){
  
  sampled.index <- sample(indicator.vector)
  
  temp.class1 <- femur.load.indexing[sampled.index == 1]
  temp.class2 <- femur.load.indexing[sampled.index == 2]
  temp.class3 <- femur.load.indexing[sampled.index == 3]
  temp.class4 <- femur.load.indexing[sampled.index == 4]
  temp.class5 <- femur.load.indexing[sampled.index == 5]
  
  new.xbar1 <- mean(temp.class1); new.var1 <- var(temp.class1)
  new.xbar2 <- mean(temp.class2); new.var2 <- var(temp.class2)
  new.xbar3 <- mean(temp.class3); new.var3 <- var(temp.class3)
  new.xbar4 <- mean(temp.class4); new.var4 <- var(temp.class4)
  new.xbar5 <- mean(temp.class5); new.var5 <- var(temp.class5)
  
  new.SST <- sum(temp.n * (c(new.xbar1, new.xbar2, 
                             new.xbar3, new.xbar4, 
                             new.xbar5) - femur.total_mean)^2)
  new.SSE <- sum((temp.n-1) * c(new.var1, new.var2, 
                                new.var3, new.var4, 
                                new.var5))

  
  new.Fstat <- (new.SST/4)/(new.SSE/(nrow(femur)-5))
  Fstat.dist[i] <- new.Fstat
}

Fstat.dist


sum(Fstat.dist >= F.stats.obs) / B

# 4-(b)
## Here exists the tie. 
# -> Need to consider the Adjustted KW statistics.

length(femur[,'femur_load']) == 
  length(unique(femur[, 'femur_load'])) 

Class <- femur[,1]
n.vec <- table(Class)
N <- sum(n.vec)
R <- rank(femur[,'femur_load'], 
          ties.method='average')

Rbar.vec <- aggregate(R, list(Class), mean)[,2]

Adjusted.KW.stats <- 1/(var(R)) * 
  sum(n.vec * (Rbar.vec - (N+1)/2)^2)

Adjusted.KW.stats

chi.sqr.approx.pvalue <- 1-pchisq(Adjusted.KW.stats, df=4)
chi.sqr.approx.pvalue
