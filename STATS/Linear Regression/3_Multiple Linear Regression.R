######################################################

# Simple linear regression example

x <- c(0.19, 0.15, 0.57, 0.4, 0.7, 0.67, 0.63, 0.47, 0.75, 0.6,
       0.78, 0.81, 0.78, 0.69, 1.3, 1.05, 1.52, 1.06, 1.74, 1.62)
y <- c(3.8, 5.9, 14.1, 10.4, 14.6, 14.5, 15.1, 11.9, 15.5, 9.3, 
       15.6, 20.8, 14.6, 16.6, 25.6, 20.9, 29.9, 19.6, 31.3, 32.7)

plot(x, y)

# regression line
result <- lm(y ~ x)
abline(result, col='blue')

summary(result)

######################################################

# Multiple regression on MPG dataset w/ ANOVA

current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
mpg_path <- file.path(current_dir, 'dataset/MPG.csv')

mpg <- read.csv(mpg_path, header=TRUE)

mpg_colnames <- colnames(mpg)
mpg_colnames <- c('MPG', mpg_colnames[2:3])
colnames(mpg) <- mpg_colnames

model <- lm(MPG ~ . - MPG, data = mpg)

# anova_result <- anova(model)
# print(anova_result)

result <- summary(model)
print(result)

# predict ?
new_x <- data.frame(Weight=10, Odometer=30)
predict.lm(model, new_x, interval='confidence') # for mean response
predict.lm(model, new_x, interval='predict') # individual response
