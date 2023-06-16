# Ex1

MD <- function(lambdaA, lambda1, lambda2, T){
  
  t <- N1 <- N2 <- W1 <- W2 <- PF1 <- PF2 <- 0
  tE <- rexp(1, (lambdaA+lambda1+lambda2))
  W <- rchisq(1, df=5)
  TW <- sum(W)
  np <- 1
  
  P1 <- lambdaA / (lambdaA + lambda1 + lambda2)
  P2 <- lambda1 / (lambdaA + lambda1 + lambda2)
  P3 <- lambda2 / (lambdaA + lambda1 + lambda2)
  
  while(tE <= T){
    t <- tE
    J <- sample(1:3, 1, prob=c(P1, P2, P3))
    
    if (J==1){
      W <- c(W, rchisq(1, df=5))
      TW <- sum(W)
      np <- length(W)
    }
    if (J==2){
      if (TW > 500){
        CW <- cumsum(W)
        L <- which(CW == max(CW[CW<=500]))
        N1 <- N1 + L
        W1 <- W1 + CW[L]
        W <- W[-(1:L)]
        PF1 <- PF1 + CW[L]*2 - 500
        TW <- sum(W)
        np <- length(W)
      }
      if (TW <= 500){
        N1 <- N1 + length(W)
        W1 <- W1 + TW
        PF1 <- PF1 + TW*2 - 500
        W <- NULL
        TW <- np <- 0
      }
    }
    if (J==3){
      if (TW > 1000){
        CW <- cumsum(W)
        L <- which(CW == max(CW[CW <= 1000]))
        N2 <- N2 + L
        W2 <- W2 + CW[L]
        W <- W[-(1:L)]
        PF2 <- PF2 + CW[L]*2 - 700
        TW <- sum(W)
        np <- length(W)
      }
      if(TW <- 1000)
      {
          N2 <- N2 + length(W)
          W2 <- W2 + TW
          PF2 <- PF2 + TW*2 - 700
          W <- NULL
          TW <- np <- 0
      }
    }
    tE <- t + rexp(1, (lambdaA + lambda1 + lambda2))
  }
  result <- c(N1, W1, N2, W2, PF1, PF2)
  return(result)
}

R <- 100
res <- NULL
for (i in 1:R){
  s <- MD(lambdaA=5, lambda1=.05, lambda2=0.01, T=500)
  res <- rbind(res, s)
}

apply(res, 2, mean)










# Ex2

bacteria <- function(b0, a0, lam1, lam2, lam3, T){
  
  t <- 0
  B <- b0
  A <- a0
  tE <- rexp(1, (B*lam1 + A*B*lam2 + A*lam3))
  S <- c(t, B, A)
  while(tE <= T){
    t <- tE
    P1 <- (B*lam1)/(B*lam1 + A*B*lam2 + A*lam3)
    P2 <- (A*B*lam2)/(B*lam1 + A*B*lam2 + A*lam3)
    P3 <- (A*lam3)/(B*lam1 + A*B*lam2 + A*lam3)
    J <- sample(1:3, 1, prob=c(P1, P2, P3))
    if (J==1) B <- B+1
    if (J==2){
      B <- B-1
      A <- A+1
    }
    if (J==3) A <- A-1
    S <- rbind(S, c(t, B, A))
    if (A==0 & B==0) break
    tE <- t + rexp(1, (B*lam1+A*B*lam2 + A*lam3))
  }
  return(S)
}

y <- bacteria(b0=50, a0=100, lam1=1, lam2=0.005, 
              lam3=0.6, T=30)

plot(y[,1],y[,2],type='l',ylim=c(0, signif(max(y[,2:3]),digits=2)), 
     col='blue', xlab='time', ylab='Population')
lines(y[,1], y[,3], col='red')