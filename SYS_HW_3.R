# SYS HW 3
# Gavin Wiehl (gtw4vx)
library(ISLR)
attach(Weekly)
set.seed(1)
#10
#a)
names(Weekly)
dim(Weekly)
contrasts(Direction)
pairs(Weekly)
summary(Weekly)
cor(Weekly[,-9]) # there does not appear to be much correlation between lag variables
# only volume and year appear to be correlated, with volume increasing as the years increase.

#b)
glm.fits<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family="binomial")
summary(glm.fits)
# Only the variable Lag2 appears to be statistically significant, with a p-value of 0.029.

#c)
glm.prob<-predict(glm.fits,type="response")
glm.pred<-rep("Down",1089)
glm.pred[glm.prob>.5]="Up" # predicts up when logit probability is greater than .5
table(glm.pred,Direction) # confusion table
mean(glm.pred==Direction) # model success rate. 56.1 percent of the models predictions are correct.

#d)
train<-(Year<2009) # create training data from year 1990 to 2008
Weekly.test<-Weekly[!train,]
dim(Weekly.test) # look at the number of items in test set
Direction.test<-Direction[!train] # create a test Direction array

glm.fits<-glm(Direction~Lag2,data=Weekly,family="binomial",subset=train) # train a logit model on training set
glm.prob<-predict(glm.fits,Weekly.test,type="response") # predict the test set with logit model
glm.pred<-rep("Down",104)
glm.pred[glm.prob>.5]="Up"
table(glm.pred,Direction.test) # confusion matrix of lag2 logit model with test set.
mean(glm.pred==Direction.test) # model success rate of 62.5 percent.

#e)
library(MASS)
lda.fit<-lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
lda.pred<-predict(lda.fit,Weekly.test)
lda.class=lda.pred$class
table(lda.class,Direction.test)
mean(lda.class==Direction.test) # model success rate of 62.5 percent

#f)
qda.fit<-qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.class<-predict(qda.fit,Weekly.test)$class
table(qda.class,Direction.test)
mean(qda.class==Direction.test) # model success rate of 58.65 percent

#g)
library(class)
train.X<-matrix(Lag2[train])
test.X<-matrix(Lag2[!train])
train.Direction<-Direction[train]

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.test)
mean(knn.pred==Direction.test) # model success rate of 50 percent

#i)
# logit regression number 2
glm.fits<-glm(Direction~Lag1*Lag2,data=Weekly,family="binomial",subset=train) # train a logit model on training set with lag1 and lag2
glm.fits
summary(glm.fits)
glm.prob<-predict(glm.fits,Weekly.test,type="response") # predict the test set with logit model
glm.pred<-rep("Down",104)
glm.pred[glm.prob>.5]="Up"
table(glm.pred,Direction.test) # confusion matrix of lag2 logit model with test set.
mean(glm.pred==Direction.test) # model success rate of 57.69 percent.

# knn where k=3
train.X<-matrix(Lag2[train])
test.X<-matrix(Lag2[!train])
train.Direction<-Direction[train]

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.test)
mean(knn.pred==Direction.test) # model success rate of 54.8 percent with k=3.

detach(Weekly)
#11
#a)
attach(Auto)
dim(Auto)
mpg01<-rep(0,392)
mpg01[mpg>median(mpg)]=1
Auto<-data.frame(Auto,mpg01)
names(Auto)

#b)
pairs(Auto) # there appears to be a relationship between displacement, horsepower,
# weight, and acceleration based on the scatterplots
boxplot(displacement~mpg01)
boxplot(horsepower~mpg01)
boxplot(weight~mpg01)
boxplot(acceleration~mpg01)
# out of those variables, displacement and weight seem to have the largest relationship on mpg01
# while acceleration has the smallest

#c)
set.seed(1)
train<-sample(392,size=294) # get 294 random integers between 0 to 392
Auto.train<-Auto[train,] # training data is 3/4 of full auto data
Auto.test<-Auto[-train,] # test data is 1/4 of full auto data

#d)
lda.fit<-lda(mpg01~displacement+horsepower+weight+acceleration,data=Auto,subset=train)
lda.prob<-predict(lda.fit,Auto.test)
lda.class<-lda.prob$class
table(lda.class,Auto.test$mpg01)
mean(lda.class==Auto.test$mpg01) # success rate is 86.73 percent

#e)
qda.fit<-qda(mpg01~displacement+horsepower+weight+acceleration,data=Auto,subset=train)
qda.class<-predict(qda.fit,Auto.test)$class
table(qda.class,Auto.test$mpg01)
mean(qda.class==Auto.test$mpg01) # success rate is 85.71 percent

#f)
glm.fit<-glm(mpg01~displacement+horsepower+weight+acceleration,data=Auto,subset=train,family="binomial")
summary(glm.fit)
glm.prob<-predict(glm.fit,Auto.test,type="response")
glm.pred<-rep(0,98)
glm.pred[glm.prob>.5]=1
table(glm.pred,Auto.test$mpg01)
mean(glm.pred==Auto.test$mpg01) # success rate 87.76 percent

#g)
set.seed(1)
#KNN with K=1
train.X<-cbind(displacement,horsepower,weight,acceleration)[train,]
test.X<-cbind(displacement,horsepower,weight,acceleration)[-train,]
train.mpg01<-Auto$mpg01[train]

knn.pred<-knn(train.X,test.X,train.mpg01,k=1)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01) # success rate 85.71 percent

#KNN with K=3
knn.pred<-knn(train.X,test.X,train.mpg01,k=3)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01) # success rate 87.76 percent

#KNN with K=7
knn.pred<-knn(train.X,test.X,train.mpg01,k=7)
table(knn.pred,Auto.test$mpg01)
mean(knn.pred==Auto.test$mpg01) # success rate 86.73 percent

