# Michael Voecks
# APPM 4580

# Load WorldBankC02 dataset from fields library
library("fields")
data(WorldBankCO2)

# Produce pairwise scatterplot of data
pairs(WorldBankCO2)
WorldBankCO2 = data.frame(WorldBankCO2)

# log scale the data to get a better model
transformed <- log(WorldBankCO2)
pairs(transformed)

# Fit a linear model to the dataset, use _________ to choose the most relevant features
library(leaps)
best.sub <- regsubsets(CO2.cap~., data=transformed)
which.min(summary(best.sub)$bic) # find the # of features that fit the model best

# After running the above, it was found the best model had all features
fit <- lm(CO2.cap~., data=WorldBankCO2)
logfit <- lm(CO2.cap~., data=transformed)

# Problem B part 2
inter <- matrix(1, 100, 1)
x1 <- seq(1,100)
x2 <- x1
b0 <- 0
b1 <- 2
b2 <- 1
error <- rnorm(100, 0, 10)
X <- cbind(inter, x1, x2)
y = inter + b0 + b1*x1 + b2*x2 + error

fit <- lm(y~x1+x2)
summary(fit)

qr.coef(qr(X), y)
