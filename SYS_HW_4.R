#5
#a)
set.seed(1)
attach(Default)
glm.fit <- glm(default~income+balance,data=Default,family = "binomial")
summary(glm.fit)
#b)
train=sample(10000,5000)
default.test<-default[-train]

glm.fit <- glm(default~income+balance,data=Default,family = "binomial",subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, newdata=Default[-train,],type = 'response')
glm.pred <- rep("No",length(default.test))
glm.pred[glm.prob>0.5] <- "Yes"
mean(glm.pred != default.test) # we have a 2.54 percent error rate

#c)
#1:
set.seed(2)
train=sample(10000,5000)
default.test<-default[-train]

glm.fit <- glm(default~income+balance,data=Default,family = "binomial",subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, newdata=Default[-train,],type = 'response')
glm.pred <- rep("No",length(default.test))
glm.pred[glm.prob>0.5] <- "Yes"
mean(glm.pred != default.test) # we have a 2.38 percent error rate

#2:
set.seed(3)
train=sample(10000,5000)
default.test<-default[-train]

glm.fit <- glm(default~income+balance,data=Default,family = "binomial",subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, newdata=Default[-train,],type = 'response')
glm.pred <- rep("No",length(default.test))
glm.pred[glm.prob>0.5] <- "Yes"
mean(glm.pred != default.test) # we have a 2.64 percent error rate

#3:
set.seed(4)
train=sample(10000,5000)
default.test<-default[-train]

glm.fit <- glm(default~income+balance,data=Default,family = "binomial",subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, newdata=Default[-train,],type = 'response')
glm.pred <- rep("No",length(default.test))
glm.pred[glm.prob>0.5] <- "Yes"
mean(glm.pred != default.test) # we have a 2.56 percent error rate

#There appears to be a noticable variablitiy between the percent error rates when the training/test sets are sampled differently
#d)
set.seed(1)
train=sample(10000,5000)
default.test<-default[-train]

glm.fit <- glm(default~income+balance+student,data=Default,family = "binomial",subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, newdata=Default[-train,],type = 'response')
glm.pred <- rep("No",length(default.test))
glm.pred[glm.prob>0.5] <- "Yes"
mean(glm.pred != default.test) # we have a 2.6 percent error rate
#The inclusion of the student variable does not appear to improve the test error rate

#6
#a)
library(boot)
set.seed(1)
glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)
# glm estimates the standard errors of B_1 and B_2 as 4.985e-06 and 2.274e-04 respectively

boot.fn=function(data,index){
  return(coef(glm(default ~ income + balance, data = data, family = "binomial", subset=index)))
}
boot(Default, boot.fn, 1000)
# bootstrap estimates the standard errors of B_1 and B_2 as 4.866284e-06 and 2.298949e-04 respectively, not too far off

#7
#a)
detach(Default)
attach(Weekly)

glm.fit1 <- glm(Direction~Lag1+Lag2,data=Weekly,family = "binomial")

#b)
glm.fit2 <- glm(Direction~Lag1+Lag2,data=Weekly[-1,],family = "binomial")

#c)
glm.prob <- predict(glm.fit1,Weekly[1,],type = "response")
glm.pred <- rep("Down",1)
glm.pred[glm.prob>0.5]="Up"
(Direction[1]==glm.pred[1]) # Incorrect. The logit model classified observation 1 as Up, when it is actually down

#d)
set.seed(1)
dim(Weekly)
error <- rep(0, 1089)
for (i in 1:1089) {
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
  glm.pred <- predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
  test.up <- Weekly[i, ]$Direction == "Up"
  if (glm.pred != test.up)
    error[i] <- 1
}
error
#e)
mean(error) # the estimate test error rate for LOOCV is 44.99 percent, which is quite high

#8
#a)
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

#b)
plot(y~x) # there appears to be a curved relationship, perhaps -x^2

#c)
#1
library(boot)
set.seed(1)
data <- data.frame(x, y)
glm.fit1 <- glm(y ~ x)
cv.glm(data, glm.fit1)$delta # error rate of 7.288162

#2
glm.fit2 <- glm(y ~ poly(x, 2))
cv.glm(data, glm.fit2)$delta # error rate of 0.9374236

#3
glm.fit3 <- glm(y ~ poly(x, 3))
cv.glm(data, glm.fit3)$delta # error rate of 0.9566218

#4
glm.fit4 <- glm(y ~ poly(x, 4))
cv.glm(data, glm.fit4)$delta # error rate of 0.9539049

#d)
set.seed(111)
glm.fit1 <- glm(y ~ x)
cv.glm(data, glm.fit1)$delta # error rate of 7.288162

#2
glm.fit2 <- glm(y ~ poly(x, 2))
cv.glm(data, glm.fit2)$delta # error rate of 0.9374236

#3
glm.fit3 <- glm(y ~ poly(x, 3))
cv.glm(data, glm.fit3)$delta # error rate of 0.9566218

#4
glm.fit4 <- glm(y ~ poly(x, 4))
cv.glm(data, glm.fit4)$delta # error rate of 0.9539049

# error rate remains the same, since we are run through every observation with LOOVC there is no difference between test/training samples.

#e)
# the quadratic model has the smallest error rate, this is what I expected, since the scatterplot looked quadratic.

#f)
summary(glm.fit1)
summary(glm.fit2)
summary(glm.fit3)
summary(glm.fit4)
# the linear and quadratic terms appear to be statistically significant due to their small p-values. The 3rd and 4th order polynomials are not statistically significant
# These results are consistent with the cross validated results, the quadradic model performed the best.

