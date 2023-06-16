current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
art1_path <- file.path(current_dir, 'dataset/artificial1.csv')
art1_data <- read.csv(art1_path, header=TRUE)

result1 <- lm(Y~X, data=art1_data)
summary(result1)

# WLS
# art1_data$TY <- art1_data$Y / art1_data$X
# art1_data$TX <- 1 / art1_data$X
# result2 <- lm(TY~TX, data=art1_data)

# summary(result2)
# plot(result2)

result2 <- lm(Y~X, data=art1_data, weight=1/X^2)
summary(result2)

layout(matrix(1:4, 2, 2))
plot(result2)
