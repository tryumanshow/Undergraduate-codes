current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
bacteria_path <- file.path(current_dir, 'dataset/bacteria.csv')

bacteria <- read.csv(bacteria_path, header=TRUE)

# Make function
draw_plots <- function(fitted_line, original_data){
  
  rstd <- rstandard(fitted_line)
  
  par(mfrow = c(1, 2))
  
  plot(fitted_line$fitted, rstd, main='yhat vs standardized residual', 
       xlab='yhat', ylab='standardized residual') 
  plot(original_data$t, rstd, main='x vs standardized residual', 
       xlab='t', ylab='standardized residual')
  
  par(mfrow = c(1,1))
  
  print(summary(fitted_line))
}

# Original
plot(bacteria$t, bacteria$nt)
result1 <- lm(nt~t, data=bacteria)
draw_plots(result1, bacteria)

# Transformation
plot(bacteria$t, log(bacteria$nt))  # Looks linear
result2 <- lm(log(nt) ~ t, data=bacteria)
draw_plots(result2, bacteria)

##################################################

# Example with artificial data

art1_path <- file.path(current_dir, 'dataset/artificial1.csv')
art1_data <- read.csv(art1_path, header=TRUE)

# Original
result1 <- lm(Y~X, data=art1_data)
par(mfrow = c(1, 2))
plot(art1_data$X, art1_data$Y)
plot(result1$fitted, rstandard(result1))
par(mfrow = c(1, 1))

layout(matrix(1:4, 2, 2))
plot(result1)


# Transform Y
art1_data$newY <- log(art1_data$Y)
result2 <- lm(newY ~ X, data=art1_data)
plot(result2)


##################################################

# Example with artificial data2

# Original
art2_path <- file.path(current_dir, 'dataset/artificial2.csv')
art2_data <- read.csv(art2_path, header=TRUE)

result2_1 <- lm(Y~X, data=art2_data)

layout(matrix(1:4, 2, 2))
plot(result2_1)

# Transform Y
art2_data$newY <- log(art2_data$Y)
result2_2 <- lm(newY ~ X, data=art2_data)
plot(result2_2)
