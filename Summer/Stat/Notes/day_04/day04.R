# STAT 6430
# Day 4 -- R
# 
# Today we continue with simple linear regression, including
# other transformations and cross-validation.
#
# Here's the brain weight data again:
#
bh.train = read.csv("brainhead-train.csv", header = TRUE)
plot(bh.train,pch=20,cex=.2)
ggplot(bh.train,aes(bh.train$HeadSize))+geom_histogram()
ggplot(bh.train,aes(bh.train$BrainWeight))+geom_histogram()
ggplot(bh.train,aes(x=HeadSize,y=BrainWeight))+  geom_point(shape=1)
#
# We previously tried a linear and and quadratic polynomial
# for models.  Here they are again, with MSE's:
#
brain.lm1 <- lm(BrainWeight ~ HeadSize, data=bh.train)
mse1 <- mean(brain.lm1$residuals^2)
mse1
brain.lm2 <- lm(BrainWeight ~ poly(HeadSize,2), data=bh.train)
mse2 <- mean(brain.lm2$residuals^2)
mse2
#
# Team Question
#  1. Do higher degree polynomials do better?
#
summary(brain.lm1)
summary(brain.lm2)
brain.lm3 <- lm(BrainWeight ~ poly(HeadSize,3), data=bh.train)
mse3 <- mean(brain.lm3$residuals^2)
mse3
summary(brain.lm3)
brain.lm4 <- lm(BrainWeight ~ poly(HeadSize,4), data=bh.train)
mse4 <- mean(brain.lm4$residuals^2)
mse4
summary(brain.lm4)


# Here's a different data set, related to the price of 
# diamonds (c = carets, p = price($)):
#
d04b <- read.csv("d04b.csv",header=T)
plot(d04b,pch=20,cex=.2)

# Team Questions:
# 1. Find a model (linear or polynomial) that seems to give
#    the best fit to the data.
# 2. Find a model for the subset of diamonds the weight less 
#    than one caret.  Does this change the model?
#

ggplot(d04b,aes(x=c,y=p))+geom_point(shape=1)+geom_smooth(method=lm)
ggplot(d04b,aes(x=c,y=log(p)))+geom_point(shape=1)+geom_smooth(method=lm)

simple_model <- lm(p~c,data = d04b)
log_model <- lm(log(p)~c,data= d04b)

mse_simple<-mean(simple_model$residuals^2)
mse_simple

mse_log < -mean(exp(log_model$residuals)^2)
mse_log

summary(simple_model)
summary(log_model)


#
## Transformations
#
# Besides polynomials, it is also common to try taking the 
# logarithm of either variable to improve the model.  Here's
# a plot of the diamond data, using log(p) in place of p:

plot(log(p)~c, data=d04b,pch=20,cex=.2)
d04b.lmlog <- lm(log(p)~c, data=d04b)
summary(d04b.lmlog)
mean(d04b.lmlog$residuals^2)

# Team Questions:
# 1. Try out models fitted to the diamond data that use log
#    transformations on one or both variables.  Are any of 
#    these better than the earlier models?

d04c <- read.csv("d04c.csv",header=T)
plot(d04c,pch=20,cex=.2)

# Team Questions:
# 1. For the set d03c, find a model that seems to best fit
#    the data.
# 2. Select at random 100 of the records in d03c, and use 
#    that subset to repeat #1.  Does it make a difference?

#
## Predictions
#
# There is a "predict" function in R that will automatically
# take the results from lm together with input variables and
# produce predictions.

d04c.lm3 <- lm(y~poly(x,3), data=d04c) # Cubic model 
xvals <- c(-5, 3, 16) # A list of 3 x-values
predict(d04c.lm3, newdata = data.frame(x=xvals)) # outputs corresponding y-values

# We can use predict to generate a plot for a nonlinear model:
#
plot(d04c,pch=20,cex=.2)
xvals <- seq(from=-20, to=20,by=.1) # A sequence of x-values
yvals <- predict(d04c.lm3,newdata=data.frame(x=xvals)) # The y-values
lines(seq(from=-20, to=20,by=.1),yvals,col='red', type='b',cex=.1)
  # "lines" connects the dots, to give the appearance of a curve

#
## Cross-validation
#
# The purpose of cross-validation is to assess the quality of a 
# model on data that was not used to create the model, with the
# goal of avoiding model quality being self-fulfilling.

## Team Questions
# 1. From the data set d04c, select a random subset of 100 records
#    and name the new data frame "d04c.train".  Name the remaining
#    records "d04c.test".
# 2. Determine which model seems to best fit the d04c.train data.
# 3. Use the model to predict the y-values corresponding to each of 
#    the x-values in the set d04c.test.
# 4. Determine the MSE for d04c.test.

# The next data set is set up in a manner similar to future homework
# exercises.  There are two sets of data: "d04d-train.csv" which contains
# x and y-values to uses to develop a model, and "d04d-predict.csv" which
# contains only x-values.  Once you have a model developed, you will use
# it to predict the y-values corresponding to the x-values in the 
# "predict" file.

## Team questions:
# 1. Upload "d04d-train.csv" into R.
# 2. Develop a model for predicting y from x.  Cross-validate!
# 3. Upload "d04d-predict.csv" into R, and use your model to
#    predict the corresponding y-values.
# 4. Upload "d04d-correct.csv" into R.  This file contains the
#    x-values from "d04d-predict.csv" along with the correct
#    y-values.  Use these y-values to compute the MSE for 
#    your predicted values.




