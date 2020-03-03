# Gavin Wiehl
# SYS 6018 - HW1
# gtw4vx

#8
# a)
college <- read.table("College.csv",sep=",",header=TRUE) # read in college csv
# b)
fix(college) # look at the college dataframe
rownames(college)=college[,1] # set rownames to college names
fix(college) # look at the college dataframe again
college=college[,-1] # remove the first row
fix(college) # once again look at the dataframe
# c)
summary(college) # see a summary of the data
pairs(college[,1:10]) # a look at the scatterplots of the first ten predictors
plot(college$Outstate~college$Private,xlab="Private",ylab="Out of state tuition") # a boxplot of out of state tuition vs private
Elite=rep("No",nrow(college)) # creating a new column called elite, 'Yes' if top 10 percent of high school grads attend college at greater than 50%
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college<-data.frame(college,Elite)
summary(college) # there are 27 elite colleges
plot(college$Outstate~Elite)
par(mfrow=c(2,2))
hist(college$Apps,xlab="College Applications",breaks=10,main="Histogram of College Applications") # a histogram of the number of college applications
hist(college$F.Undergrad,xlab="Full Time Undergrad",breaks=5,main="Histogram of Full Time Undergraduates") # a histogram of the number of full time undergraduates
hist(college$PhD,xlab="PhD's",breaks=10,main="Histogram of faculty PhD's") # a histogram of the number of faculty phd's
hist(college$Grad.Rate,xlab="Graduation Rate",breaks=20,main="Histogram of graduation rates") # a histogram of the graduation rates

## Summary:
# a look at the scatterplot pairs shows numerous predictors with strong correlations, mostly positive
# and some linear. For instance, the number of applications and the number of applications accepted show
# a positive linear relationship. The variables Top25perc and Top10perc also have a positive relationship
# Enroll and F.Undergrad also have a positive linear relationship. Many of these variables have some relationship
# to each other. Looking at the box plot of Out of state tuition vs Private/Non-private, there also appears to be 
# a relationship. Private colleges generally have higher out of state tuitions than do non-private colleges.
# We also see that most of our colleges cannot be considered elite based on our criteria, which is that over 
# 50 percent of students enrolled graduated in the top 10 percent of their class. Looking at our 
# histograms, most of the colleges receive less than 5000 applications, and most have less than 5000 enrolled.
# They also show that most colleges have 60-80 PhD's as faculty, and that the graduation rates, which look pretty
# normally distributed hover around 45 percent to 80 percent.

#10
#a)
library(MASS)
?Boston
fix(Boston)
# there are 506 rows and 14 columns. rows represent different Boston suburbs, and the columns represent different statistics
# about the suburbs, like crime rate, pupil to teacher ratio, median value of homes, etc.
#b)
par(mfrow=c(1,2))
pairs(Boston)
library(corrplot)
corrplot(cor(Boston),method="number")
# pairwise scatterplots show a few correlations between the variables, like lstat (percentage of lower status of the population) 
# and medv (median value of owner-occupied homes), and dis (weighted distances to five Boston employment centres) and nox (nitric oxides concentration).
#c) A few variables look correlated with crim, including lstat and rm (	average number of rooms per dwelling). Both of these relationships are positive
# and look somewhat linear.
#d)
summary(Boston)
par(mfrow=c(1,3))
plot(Boston$crim)
plot(Boston$tax)
plot(Boston$ptratio)
# The range of crime rates is 0.00632 to 88.9762. The mean 3.614. Most suburbs have low crime, however a few have very high crime rates.
# The range of property tax rates starts at 187.0 to 711.0. The mean is 408.2. There are a wide variance of tax rates, the suburbs are scattered between 200 and 500.
# The range of pupil to teacher ratio starts at 12.60 to 22.0. The mean is 18.46. The pupil-teacher ratio is also scattered with most suburbs between 14 to 21 pupil to teacher ratio.
#e)
Boston$chas<-as.factor(Boston$chas)
is.factor(Boston$chas)
summary(Boston)
# There are 35 suburbs that bound the Charles river.
#f)
# The median pupil-teacher ratio is 19.05
#g)
Boston[which(Boston$medv == min(Boston$medv)),] # picks the index of the min medv, then chooses Boston at that indices.
summary(Boston)
# There are two suburbs that have a minimum median home value of 5000, at index 399 and 406 respectively. They have similar or the same values for indus(18.1), nox(0.693), rm(5.453 and 5.683), age(100), 
# rad(24),tax(100), and ptratio(20.1). They both have higher than average crime rates, high indus(proportion of non-retail business acres per town), high rad(index of accessibility to radial highways), high pupil to teacher ratio, and high lstat. 
#h)
length(which(Boston$rm>7))
length(which(Boston$rm>8))
Boston[which(Boston$rm>7),] 
Boston[which(Boston$rm>8),]
summary(Boston)
summary(Boston[which(Boston$rm>7),])
summary(Boston[which(Boston$rm>8),])
# There are 64 suburbs that have an average room per dwelling above 7. Those suburbs have less than average crime rates, indus, age, rad, tax, and lstat.
# There are 13 suburbs that have an average room per dwelling above 8. Those suburbs have less than average crime rates, indus, dis, rad, tax, ptratio, and lstat.