current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
turkey_path <- file.path(current_dir, 'dataset/Turkey.csv')

turkey <- read.csv(turkey_path, header=TRUE)

turkey$Origin = factor(turkey$Origin)

# Only categorical
fit1 = lm(Y ~ X + Origin, turkey)
summary(fit1)

# What if interaction?
fit2 <- lm(Y ~ X + Origin + Origin*X, turkey)
summary(fit2)
