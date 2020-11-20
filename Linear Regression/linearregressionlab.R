library(MASS)
library(ISLR)

# lstat -> percent of households with low socioeconomic status
# medv -> median house value
lmfit = lm(medv~lstat, data = Boston)

# summary of the model
summary(lmfit)

# confidence intervals for coefficients
confint(lmfit)

# produce confidence/prediction intervals for given values
# confidence intervals --> 95% of intervals will contain the true value of f(X), or the true average
predict(lmfit, data.frame(lstat=c(5,10,15)), interval="confidence")
# prediction intervals --> 95% of intervals will contain the true value of Y for this lstat value
predict(lmfit, data.frame(lstat=c(5,10,15)), interval="prediction")

plot(Boston$lstat, Boston$medv)
abline(lmfit)

# diagnostic plots
par(mfrow=c(2,2))
plot(lmfit)

# residual plots -> useful for determining non-linearity
plot(predict(lmfit), residuals(lmfit))
plot(predict(lmfit), rstudent(lmfit))

# leverage plots
plot(hatvalues(lmfit))
which.max(hatvalues(lmfit))

# Multiple Linear Regression
# Two predictors
lmfit = lm(medv~age+lstat, data=Boston)
summary(lmfit)

# All predictors
lmfit = lm(medv~., data=Boston)
summary(lmfit)
