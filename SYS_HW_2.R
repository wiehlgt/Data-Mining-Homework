# SYS6018 HW2
# Gavin Wiehl
# gtw4vx



# 9
#a)
auto <- read.csv("Auto.csv",sep=",",header=TRUE)
pairs(auto)

#b)
auto2<-auto[,-9] # creates new dataframe without names variable
sapply(auto2, is.numeric) # check to see if other variables are numeric
auto2$horsepower <- as.numeric(auto2$horsepower) # changes horsepower to numeric
sapply(auto2, is.numeric)
cor(auto2) # correlations between variables
#c)
auto.lm <- lm(mpg~.,data=auto2) # regression with all variables except name
summary(auto.lm)
#d)
plot(auto.lm) #The residual vs fitted plot does not look too concerning, though with the left hand beginning side we see the residuals 
# consistently above the origin line. This can potentially suggest some non-linearity, and there is some non-constant variance
#e)
attach(auto2)
auto.inter <- lm(mpg~weight*horsepower*acceleration,data=auto2) # regression with weight, horsepower, and acceleration. Inlcudes interaction 
# between all three terms
summary(auto.inter)

#f)
#log transformation 
auto.dis<-lm(mpg~displacement,data=auto2) # regression using displacement only
auto.wht<-lm(mpg~weight,data=auto2) # regression using weight only
plot(auto.dis) # residual plots show regression with displacement only produced non-constant variance
plot(auto.wht) # residual plots show regression with weight only produced non-constant variance

auto.dislog<-lm(mpg~log(displacement),data=auto2) # regression using log(displacement) only
auto.whtlog<-lm(mpg~log(weight),data=auto2) # regression using log(weight) only
plot(auto.dislog) # residual plots show log(displacement) regression with constant variance
plot(auto.whtlog) # residual plots show log(weight) regression with constant variance

auto.diswht<- lm(mpg~displacement+weight,data=auto2) # regression with displacement and weight
auto.log<-lm(mpg~log(displacement)+log(weight),data=auto2) # regression with log(displacement) and log(weight)
summary(auto.diswht) #summary of regression with displacement and weight
summary(auto.log) # summary of regression with log(displacement) and log(weight)

# square root transformation
auto.dissqrt<-lm(mpg~sqrt(displacement),data=auto2) # regression using log(displacement) only
auto.whtsqrt<-lm(mpg~sqrt(weight),data=auto2)

plot(auto.dissqrt) # residual plots show sqrt(displacement) regression with constant variance
plot(auto.whtsqrt) # residual plots show sqrt(weight) regression with constant variance

summary(auto.dis)
summary(auto.wht)
summary(auto.dissqrt) #summary of regression with sqrt(displacement)
summary(auto.whtsqrt) # summary of regression with  sqrt(weight)

# squared transformation
auto.dissqr<-lm(mpg~(displacement^2),data=auto2) # regression using displacement^2 only
auto.whtsqr<-lm(mpg~(weight^2),data=auto2) # regression using weight^2 only
plot(auto.dissqr) # residual plots show displacement^2 regression 
plot(auto.whtsqr) # residual plots show weight^2 regression 

detach(auto2)
#14
#a)
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm (100)
#b)
cor(x1,x2)
plot(x1,x2)
#c)
y.lm<-lm(y~x1+x2)
summary(y.lm)
#d
y.x1<-lm(y~x1) # regression with x1 only
summary(y.x1)
#e
y.x2<-lm(y~x2) # regression with x2 only
summary(y.x2)
plot(y~x2)
abline(y.x2)
#g)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

y.lm<-lm(y~x1+x2)
summary(y.lm)

y.x1<-lm(y~x1) # regression with x1 only
summary(y.x1)

y.x2<-lm(y~x2) # regression with x2 only
summary(y.x2)

plot(y.lm)
plot(y.x1)
plot(y.x2)

plot(y~x1)
plot(y~x2)

#15
#a)
library(MASS)
#a)
crim.zn<-lm(crim~zn,data=Boston)
crim.indus<-lm(crim~indus,data=Boston)
crim.chas<-lm(crim~chas,data=Boston)
crim.nox<-lm(crim~nox,data=Boston)
crim.rm<-lm(crim~rm,data=Boston)
crim.age<-lm(crim~age,data=Boston)
crim.dis<-lm(crim~dis,data=Boston)
crim.rad<-lm(crim~rad,data=Boston)
crim.tax<-lm(crim~tax,data=Boston)
crim.ptratio<-lm(crim~ptratio,data=Boston)
crim.black<-lm(crim~black,data=Boston)
crim.lstat<-lm(crim~lstat,data=Boston)
crim.medv<-lm(crim~medv,data=Boston)

summary(crim.zn)
summary(crim.indus)
summary(crim.chas)
summary(crim.nox)
summary(crim.rm)
summary(crim.age)
summary(crim.dis)
summary(crim.rad)
summary(crim.tax)
summary(crim.ptratio)
summary(crim.black)
summary(crim.lstat)
summary(crim.medv)

plot(Boston$crim~Boston$chas)
#b)
Boston.lm<-lm(crim~.,data=Boston)
summary(Boston.lm)
#c)
uni_coeff <- c(coefficients(crim.zn)[2], coefficients(crim.indus)[2],coefficients(crim.chas)[2],coefficients(crim.nox)[2],
               coefficients(crim.rm)[2],coefficients(crim.age)[2],coefficients(crim.dis)[2],coefficients(crim.rad)[2],
               coefficients(crim.tax)[2],coefficients(crim.ptratio)[2],coefficients(crim.black)[2],coefficients(crim.lstat)[2],
               coefficients(crim.medv)[2]) # all the coefficients of the univariate linear regression
multi_coeff <- c(coefficients(Boston.lm)[2:14]) # all the coefficients of the multivariate linear regression
plot(multi_coeff~uni_coeff,xlab='univariate regression coefficients',ylab='multivariate regression coefficients')

#d)
# univariate regression using the form y~x+x^2+x^3
crim.zn<-lm(crim~zn+(zn^2)+(zn^3),data=Boston)
crim.indus<-lm(crim~indus+(indus^2)+(indus^3),data=Boston)
crim.chas<-lm(crim~chas+(chas^2)+(chas^3),data=Boston)
crim.nox<-lm(crim~nox+(nox^2)+(nox^3),data=Boston)
crim.rm<-lm(crim~rm+(rm^2)+(rm^3),data=Boston)
crim.age<-lm(crim~age+(age^2)+(age^3),data=Boston)
crim.dis<-lm(crim~dis+(dis^2)+(dis^3),data=Boston)
crim.rad<-lm(crim~rad+(rad^2)+(rad^3),data=Boston)
crim.tax<-lm(crim~tax+(tax^2)+(tax^3),data=Boston)
crim.ptratio<-lm(crim~ptratio+(ptratio^2)+(ptratio^3),data=Boston)
crim.black<-lm(crim~black+(black^2)+(black^3),data=Boston)
crim.lstat<-lm(crim~lstat+(lstat^2)+(lstat^3),data=Boston)
crim.medv<-lm(crim~medv+(medv^2)+(medv^3),data=Boston)

summary(crim.zn)
summary(crim.indus)
summary(crim.chas)
summary(crim.nox)
summary(crim.rm)
summary(crim.age)
summary(crim.dis)
summary(crim.rad)
summary(crim.tax)
summary(crim.ptratio)
summary(crim.black)
summary(crim.lstat)
summary(crim.medv)

