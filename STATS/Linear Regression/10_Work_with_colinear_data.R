# Install Packages
if ("HH" %in% installed.packages()) {
  print("'remotes' package is already installed.")
} else (
  install.packages("HH")
)

library("HH")


current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
edu_path <- file.path(current_dir, 'dataset/edu_multi.csv')
edu <- read.csv(edu_path, header=TRUE)

Y <- edu[,1]
X <- edu[,2:4]
TX <- scale(X, center=TRUE, scale=TRUE)
TY <- scale(Y, center=TRUE, scale=TRUE)

# Solution 1: remove specific X variable OR variable selection

# Solution 2: PCR

# Spectral Decomposition
eig <- eigen(t(TX) %*% TX)
P <- eig$vectors
L <- eig$values

vector_str <- paste(L/sum(L), collapse = " ")
print(sprintf('Importance of each principal component: %s', vector_str))

C <- TX %*% P # Principal Component
result1 <- lm(TY ~ C - 1)  # 절편 고려 x
summary(result1)
vif(result1)

# PCR only with the first PC
result2 <- lm(TY ~ C[, 1] - 1)
summary(result2)

# Solution 3: Ridge Regression
library('MASS')
lm.ridge(ACHV ~ . , data=edu, lambda=0.37)
