library(ggplot2)

# Question2 (a)

trt1 <- c(10, 15, 50)
trt2 <- c(12, 17, 19)
trt.comb <- c(trt1, trt2)

stats <- mean(trt1) - mean(trt2)

all.permutation <- combn(6, 3)

B <- choose(6, 3)
test.stat <- rep(NA, B)

for (i in 1:B){
  
  T1.ind <- all.permutation[,i]
  new.Y1 <- trt.comb[T1.ind]
  new.Y2 <- trt.comb[-T1.ind]
  test.stat[i] <- mean(new.Y1) - mean(new.Y2)
}

sum(test.stat >= stats) / B


ggplot(as.data.frame(test.stat), aes(x=test.stat))+
  geom_dotplot(method='histodot',binwidth=1.5)+
  geom_vline(xintercept=stats, color='blue')
  
# Question2 (b)

stats2 <- sum(trt1) - sum(trt2)
test.stat2 <- rep(NA, B)


for (i in 1:B){
  T1.ind <- all.permutation[,i]
  new.Y1 <- trt.comb[T1.ind]
  new.Y2 <- trt.comb[-T1.ind]
  test.stat2[i] <- sum(new.Y1) - sum(new.Y2)
}

sum(test.stat2 >= stats2) / B


#############################################

# Question3 (a)

speciesA <- c(5.1, 9.4, 7.2, 8.1, 8.8)
speciesB <- c(2.5, 4.2, 6.9, 5.5, 5.3)
class <- c(rep(1, 5), rep(2, 5))

species <- c(speciesA, speciesB)
species.rank <- rank(species)


species.rank1 <- species.rank[class==1]
species.rank2 <- species.rank[class==2]


Wilcoxon.obs <- mean(species.rank1) - 
  mean(species.rank2)



all.permutation2 <- combn(10,5)
B2 <- choose(10, 5)

test.stats3 <- rep(NA, B2)


for (i in 1:B2){
  T1.ind <- all.permutation2[,i]
  rank1 <- species.rank[T1.ind]
  rank2 <- species.rank[-T1.ind]
  interest <- mean(rank1) - mean(rank2)
  test.stats3[i] <- interest
}

sum(test.stats3 >= Wilcoxon.obs)/B2


# Question3 (b)

mean.rank <- mean(species.rank)
var.rank <- mean(species.rank^2) - mean(species.rank)^2

wilcox.mean <- 5*mean.rank
wilcox.var <- (5*5*var.rank)/9

z.score <- (sum(species.rank1) - wilcox.mean) / sqrt(wilcox.var)

1-pnorm(z.score)
