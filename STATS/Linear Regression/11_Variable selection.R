# Read data
current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
adv_path <- file.path(current_dir, 'dataset/advertising.txt')
adv <- read.table(adv_path, header=T)

# Full model
fit1 <- lm(St ~ ., data = adv)
anova(fit1)

# Reduced model
fit2 <- lm(St ~ At + Pt + Et, data = adv)
anova(fit2)

# Hypothesis testing
# At1, Pt1 필요 x
anova(fit2, fit1)

fit3 <- lm(St ~ Pt + Et, data = adv)

# Criterion

## Mellow's Cp
full_mse <- sum(fit1$residuals ** 2) / df.residual(fit1)
row_cnt <- nrow(adv)

cp_model1 <- sum(fit1$residuals ** 2) / full_mse - (row_cnt - 2 * length(fit1$coefficients))
cp_model2 <- sum(fit2$residuals ** 2) / full_mse - (row_cnt - 2 * length(fit2$coefficients))
cp_model3 <- sum(fit3$residuals ** 2) / full_mse - (row_cnt - 2 * length(fit3$coefficients))

2 * length(fit1$coefficients) - (length(fit1$coefficients - 1))
2 * length(fit2$coefficients) - (length(fit1$coefficients - 1))
2 * length(fit3$coefficients) - (length(fit1$coefficients - 1))

## AIC
c(AIC(fit1), AIC(fit2), AIC(fit3))

## BIC
c(BIC(fit1), BIC(fit2), BIC(fit3))


# Variable Selection 
null <- lm(St ~ 1, data = adv)
full <- lm(St ~ ., data = adv)

## Forward selection( Result: St ~ At + Pt + Et )
step(null, scope = list(lower = null, 
                        upper = full), 
     direction = "forward")

## Backward elimination ( Result: St ~ At + Pt + Et )
step(full, scope = list(lower = null, 
                       upper = full), 
     direction='backward')  

## Stepwise  ( Result: St ~ At + Pt + Et )
step(null, scope = list(lower = null, 
                        upper = full), 
     direction='both')
