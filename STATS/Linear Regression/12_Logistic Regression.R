# Read data
current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
admit_path <- file.path(current_dir, 'dataset/admit.csv')
admit <- read.csv(admit_path, header=T)

# Labeling for each rank
admit$rank2 <- ifelse(admit$rank == 2, 1, 0)
admit$rank3 <- ifelse(admit$rank == 3, 1, 0)
admit$rank4 <- ifelse(admit$rank == 4, 1, 0)

# fit <- glm(admit ~ gre + gpa + rank2 + rank3 + rank4, data=admit, 
#           family='binomial')

fit <- glm(admit ~ gre + gpa + factor(rank), data=admit, 
    family='binomial')
summary(fit)


y_pred <- ifelse(fit$fitted.values>0.5, 1, 0)
tb <- table(admit$admit, 
          y_pred, 
          dnn=c('Observed', 'Predicted'))

# Accuracy
sum(diag(tb)) / sum(tb)


# Add new data
new <- data.frame(gre=500, gpa=3.25, rank=1)
predict(fit, newdata=new, type='link')  # estimated log-odds

# P(admit=1) and C.I
pred <- predict(fit, newdata=new, type='response', se.fit=TRUE)
pred$fit
pred$se.fit
c(pred$fit - 1.96*pred$se.fit, pred$fit + 1.96*pred$se.fit)
