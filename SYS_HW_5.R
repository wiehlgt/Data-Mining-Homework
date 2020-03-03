# Gavin Wiehl (gtw4vx)
#8
#a)
set.seed(1)
x = rnorm(100)
e = rnorm(100)

#b)
b0 = 4
b1 = 2.2
b2 = 7.1
b3 = -3.5
y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + e
#c)
library(leaps)
dataframe <- data.frame(y=y,x=x)
regfit.full=regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe, nvmax = 10)
reg.summary = summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab = "Number of Variables",ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)
points(4,reg.summary$adjr[4],col='red',cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(4,reg.summary$cp[4],col='red',cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(3,reg.summary$bic[3],col="red",cex=2,pch=20)

# using adjusted R-squared, we use the 4 variable model
# using CP, we use the 4 variable model
# using BIC, we use the 3 variable model

coef(regfit.full, which.max(reg.summary$adjr2))
# best coefficients are 4.072 for intercept, 2.587 for b1, 6.945 for b2, -3.942 for b3, and 0.08 for b4
# the best model includes x,x^2,x^3, and x^5


#d)
#Forward selection
regfit.fwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe, nvmax = 10, method = "forward")
reg.summary.fwd <- summary(regfit.fwd)
par(mfrow = c(2, 2))
plot(reg.summary.fwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
which.min(reg.summary.fwd$cp)
points(4,reg.summary.fwd$cp[4],col='red',cex=2,pch=20)

plot(reg.summary.fwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(reg.summary.fwd$bic)
points(3,reg.summary.fwd$bic[3],col='red',cex=2,pch=20)

plot(reg.summary.fwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
which.max(reg.summary.fwd$adjr2)
points(4,reg.summary.fwd$adjr2[4],col='red',cex=2,pch=20)

# using adjusted R-squared, we use the 4 variable model
# using CP, we use the 4 variable model
# using BIC, we use the 3 variable model

coef(regfit.fwd, which.max(reg.summary.fwd$adjr2))
# best coefficients are 4.072 for intercept, 2.587 for b1, 6.945 for b2, -3.942 for b3, and 0.08 for b4
# the best model includes x,x^2,x^3, and x^5
#Backward selection

regfit.bwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe, nvmax = 10, method = "backward")
reg.summary.bwd <- summary(regfit.bwd)
par(mfrow = c(2, 2))
plot(reg.summary.bwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
which.min(reg.summary.bwd$cp)
points(4,reg.summary.bwd$cp[4],col='red',cex=2,pch=20)

plot(reg.summary.bwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(reg.summary.bwd$bic)
points(3,reg.summary.bwd$bic[3],col='red',cex=2,pch=20)

plot(reg.summary.bwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
which.max(reg.summary.bwd$adjr2)
points(4,reg.summary.bwd$adjr2[4],col='red',cex=2,pch=20)

# using adjusted R-squared, we use the 4 variable model
# using CP, we use the 4 variable model
# using BIC, we use the 3 variable model

coef(regfit.bwd, which.max(reg.summary.bwd$adjr2))
# best coefficients are 4.079 for intercept, 2.43 for b1, 6.93 for b2, -3.68 for b3, and 0.0012 for b4
# the best model includes x,x^2,x^3, and x^9
# best subset selection and forward selection gives us the models with x,x^2,x^3, and x^5
# while backwards selection gives us the models with x,x^2,x^3, and x^9

#e)
set.seed(1)
library(glmnet)
x.matrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe)[, -1]
cv.lasso <- cv.glmnet(x.matrix, y, alpha = 1)
plot(cv.lasso)

top.lambda <- cv.lasso$lambda.min
top.lambda

fit.lasso <- glmnet(x.matrix, y, alpha = 1)
predict(fit.lasso, s = top.lambda, type = "coefficients")[1:11, ]

# We see that the lasso method gives us a model with x, x^2, x^3, x^5, and x^10
# x^10 coefficient looks quite small

#f)
b7 <- 3
y <- b0 + b7 * x^7 + e
dataframe <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 2))

plot(reg.summary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
which.min(reg.summary$cp)
points(2,reg.summary$cp[2],col='red',cex=2,pch=20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(1,reg.summary$bic[1],col='red',cex=2,pch=20)

plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
which.max(reg.summary$adjr2)
points(4,reg.summary$adjr2[4],col='red',cex=2,pch=20)

# We find that CP gives us a model with 2 variables
# BIC gives us a 1 variable model
# Adjr2 gives us a 4 variable model

coef(regfit.full, 1)
# The one variable model has an intercept of 3.95 and b7 as 3.0 
coef(regfit.full, 2)
# the two variable model has a intercept of 4.07, b2 as -0.14, b7 as 3.0
coef(regfit.full, 4)
# the four variable model has a 4.07, b1 as 0.29, b2 as -0.161, b3 as -0.25, b7 as 3.0

# Now for the lasso regression
x.matrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataframe)[, -1]
cv.lasso <- cv.glmnet(x.matrix, y, alpha = 1)
bestlam <- cv.lasso$lambda.min
bestlam
fit.lasso <- glmnet(x.matrix, y, alpha = 1)
predict(fit.lasso, s = bestlam, type = "coefficients")[1:11, ]
# Lasso picks the one variable model with the intercept as 4.32 and b7 as 2.9. b7 is close but the intercept is off.

#9
#a)
library(ISLR)
attach(College)
set.seed(1)
dim(College)
train = sample(1:777, 777 / 2)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]
#b)
lm.fit <- lm(Apps ~ ., data = College.train)
lm.pred <- predict(lm.fit, College.test)
mean((lm.pred - College.test$Apps)^2)
# The test MSE for the linear model is 1135758
#c)
matrix.train <- model.matrix(Apps ~ ., data = College.train)
matrix.test <- model.matrix(Apps ~ ., data = College.test)
grid<-10^seq(10,-2,length=100)
ridge.fit <- glmnet(matrix.train, College.train$Apps, alpha = 0, lambda = grid, thresh=1e-12)
ridge.cv <- cv.glmnet(matrix.train, College.train$Apps, alpha = 0, lambda = grid, thresh=1e-12)
ridge.lambda <- ridge.cv$lambda.min
ridge.lambda

ridge.predict <- predict(ridge.fit, s = ridge.lambda, newx = matrix.test)
mean((ridge.predict - College.test$Apps)^2)
# the best lambda is 0.01, and the test MSE for ridge regression is 1135714
#d)
lasso.fit <- glmnet(matrix.train, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
lasso.cv <- cv.glmnet(matrix.train, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
lasso.lambda <- lasso.cv$lambda.min
lasso.lambda
lasso.pred <- predict(lasso.fit, s = lasso.lambda, newx = matrix.test)
mean((lasso.pred - College.test$Apps)^2)
# The best lambda is also 0.01 and the test MSE for lasso regression is 1135659
#e)
library(pls)
pcr.fit <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, College.test, ncomp = 10)
mean((pcr.pred - College.test$Apps)^2)
# the test MSE for PCR is 1723100
#f)
pls.fit <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, College.test, ncomp = 10)
mean((pls.pred - College.test$Apps)^2)
# The test MSE for PLS is 1131661
#g)
# According to the test MSE's the PLS and Lasso regression performed the worst, while the Linear model and PCR performed the best.
# However, none of these MSE's are great.




