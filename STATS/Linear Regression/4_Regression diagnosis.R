current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
mpg_path <- file.path(current_dir, 'dataset/MPG.csv')

mpg <- read.csv(mpg_path, header=TRUE)
plot(mpg)

mpg_fit <- lm(MPG ~ Weight + Odometer, mpg)
# residiuals
mpg_fit$residuals 
# yhat
mpg_fit$fitted 

# standardized residual
rstudent(mpg_fit)
# Leverage values ( Threshold: 0.4 )
hatvalues(mpg_fit)
# Cook's distance
cooks.distance(mpg_fit)

plot(mpg_fit, which=5) # Residual vs Leverage (w/ Cook's distance)
