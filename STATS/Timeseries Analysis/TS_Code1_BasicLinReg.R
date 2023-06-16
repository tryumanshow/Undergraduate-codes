# Basic Linear Regression

bp <- c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)
weight <- c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
age <- c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)

par(mfrow=c(1,2))
plot(weight, bp)
title('BP vs Weight')
abline(lm(bp~weight), col='red')
plot(age, bp)
title('Age vs Weight')
abline(lm(bp~age), col='red')


# fit linear model
fit <- lm(bp ~ weight + age)
summary(fit)


resi <- fit$residuals
par(mfrow=c(2,2))
plot(resi)
title('residual plot')
plot(weight, resi)
title('residual vs weight')
plot(age, resi) # Not really clear, but someone might say 'It has quadratic relationship!'
title('residual vs age')
qqnorm(resi)
qqline(resi)