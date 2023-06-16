##################################################

# Install Packages
if ("randtests" %in% installed.packages()) {
  print("'remotes' package is already installed.")
} else (
  install.packages("randtests")
)

if ("lmtest" %in% installed.packages()) {
  print("'remotes' package is already installed.")
} else (
  install.packages("lmtest")
)

library(randtests)
library(lmtest)

##################################################


# Read data
current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
bacteria_path <- file.path(current_dir, 'dataset/bacteria.csv')
bacteria <- read.csv(bacteria_path, header=TRUE)

# sign?
result1 <- lm(nt ~ t, data=bacteria)
r <- rstandard(result1)
sign(r)

# Run test
runs.test(r, 'two.sided', threshold=0)  # Residuals are not random.

# Durbin-Watson Test
dwtest(bacteria$nt ~ bacteria$t)  # Auto-correlation exists

##################################################



