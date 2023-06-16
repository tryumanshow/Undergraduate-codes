
# Install Packages
if ("HH" %in% installed.packages()) {
  print("'remotes' package is already installed.")
} else (
  install.packages("HH")
)

library("HH")
 

##################################################

# See VIF example 1

# Read data
current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
edu_path <- file.path(current_dir, 'dataset/edu_multi.csv')
edu <- read.csv(edu_path, header=TRUE)

fit <- lm(ACHV ~ ., data=edu)
summary(fit)

plot(edu)

# See VIF
vif(fit)

##################################################

current_dir <- getwd()
current_dir <- file.path(current_dir, 'Desktop/undergraduate_github/Stats/Linear Regression')
adv_path <- file.path(current_dir, 'dataset/advertising.txt')
adv <- read.table(adv_path, header=T)
fit <- lm(St ~ At + Pt + Et + At.1 + Pt.1, data=adv)
summary(fit)
vif(fit)

plot(adv)

fit2 <- lm(St ~ At + Pt + Et, data=adv)
summary(fit2)
vif(fit2)
