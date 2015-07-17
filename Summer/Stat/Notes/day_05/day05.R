# STAT 6430
# Day 5 -- R
# 
# Today is a hodge-podge -- we'll see how far we get.
#
## Log transformations and MSE's
#
# Let's start with the diamond data set:
d05b <- read.csv("d05b.csv",header=T)
plot(d05b,pch=20,cex=.2)
ggplot(d05b,aes(x=c,y=p))+geom_point()+geom_smooth(method=lm)
# The linear and quadratic fits, so that we have the MSE's for
# comparison:
d05b.lm1 <- lm(p ~ c, data=d05b)
mse1 <- mean(d05b.lm1$residuals^2)
mse1
d05b.lm2 <- lm(p ~ poly(c,2), data=d05b)
mse2 <- mean(d05b.lm2$residuals^2)
mse2
#
# Now suppose we use a log transformation, applying log() to
# the price p:
d05b.lmlogy <- lm(log(p) ~ c, data=d05b)
mean(d05b.lmlogy$residuals^2)

#
# We can't use the residuals from d05b.lmlog because they are
# generated from the model log(p)~c, and we want the difference
# between the predicted and actual y-values, not the predicted and
# actual log(y)-values.
# Here's the actual y-values, from the data:
actual.y <- d05b$p
#
# And here's the predicted y-values:
predict.y <- exp(d05b.lmlogy$fitted.values) # exp() reverses the log
#
# Now we can compute the MSE:
mselogy <- mean((actual.y - predict.y)^2)
mselogy # Not so great!

# Here's a plot of the model with the data:
plot(d05b,pch=20,cex=.2)
xvals <- seq(from=0.15, to=1.05,by=.0025) # A sequence of x-values
yvals <- predict(d05b.lmlogy,newdata=data.frame(c=xvals)) # The y-values
lines(xvals,exp(yvals),col='red', type='l',cex=.1)

# Team Questions:
# 1. For the set d05c, find a model that seems to best fit
#    the data.
# 2. Select at random 100 of the records in d05c, and use 
#    that subset to repeat #1.  Does it make a difference?
d05c <- read.csv("d05c.csv")

ggplot(d05c,aes(x=x,y=y))+geom_point()
ggplot(d05c,aes(x=x,y=y))+geom_point()+geom_smooth(method=lm)

simple_lm<-lm(y~x,data=d05c)
summary(simple_lm)
anova(simple_lm)

plot(simple_lm)

quad_lm<-lm(y~poly(x,2),data=d05c)
summary(quad_lm) # The quadaratic parameter is insignificant and therefore doesn't count
anova(quad_lm)

lin_log_lm<-lm()

sub <- sample(1:200, size=100,replace=F)
training<-d05c[sub,]
test    <-d05c[-sub,]

ggplot(data=training,aes(x=x,y=y))+geom_point()

training_lm <- lm(data=training,y~x)
summary(training_lm)

training_lm_quad <- lm(data=training,y~poly(x,2))
summary(training_lm_quad)
#
## Cross-validation
#
# The purpose of cross-validation is to assess the quality of a 
# model on data that was not used to create the model, with the
# goal of avoiding model quality being self-fulfilling.

## Team Questions
# 1. From the data set d05c, select a random subset of 100 records
#    and name the new data frame "d05c.train".  Name the remaining
#    records "d05c.test".
# 2. Determine which model seems to best fit the d05c.train data.
# 3. Use the model to predict the y-values corresponding to each of 
#    the x-values in the set d04c.test.
# 4. Determine the MSE for d04c.test.

# The next data set is set up in a manner similar to future homework
# exercises.  There are two sets of data: "d05d-train.csv" which contains
# x and y-values to uses to develop a model, and "d05d-predict.csv" which
# contains only x-values.  Once you have a model developed, you will use
# it to predict the y-values corresponding to the x-values in the 
# "predict" file.

## Team questions:
# 1. Upload "d05d-train.csv" into R.
# 2. Develop a model for predicting y from x.  Cross-validate!
# 3. Upload "d05d-predict.csv" into R, and use your model to
#    predict the corresponding y-values.
# 4. Upload "d05d-correct.csv" into R.  This file contains the
#    x-values from "d05d-predict.csv" along with the correct
#    y-values.  Use these y-values to compute the MSE for 
#    your predicted values.

#
## Loops
#

# With a "for" loop, each term in the specified sequence will be 
# used for one pass through the loop.  Any sequence can be specified.
#
for (j in 1:5){
  print(j)
}
#
for (j in 5:1){
  print(j)
  print(j^3)
}
#
for (j in seq(2,10,by=2)){
  print(j)
}
#
for (j in c(7,-3,9,0)){
  print(j)
}
#
# A "while" loop will execute for as long as a specified condition
# is met.
#
i <- 5
factorial <- 1
while (i > 1) {
  factorial <- i*factorial
  print(factorial)
  i <- i-1
}

ran <- runif(1, min=0, max=1) # Random number in [0,1]
print(ran)
while(ran > 0.2) {
  ran <- runif(1, min=0, max=1)
  print(ran)
}

x <- runif(2, min=0, max=1) 
print(x)
while(x[1] > 0.4 | x[2] > 0.4) {
  x <- runif(2, min=0, max=1) 
  print(x)
}

#
## Functions
#

fact <- function(i) {   # A function to compute factorials
  if(i<0)  return (0)
  if(i<=1) return (1)
           return (i*fact(i-1))
}
fact(5)
x <- fact(6)
x

demo1 <- function(x,y) {
  w <- x+y
  return(c(w, x*y))
}
z <- demo1(3,8)
z
z[1]
w  # w is not visible outside the function

#
# Quick introduction to If/Then statements
#

small.number <- function(x) {
  if (x <= 100) {
    return("small")  # Or print("small") if not assigning
  } 
}
small.number(45.8)
small.number(108)

small.or.big.number <- function(x) {
  if (x <= 100) {
    return("small")
  } else {
    return("big")
  }
}
small.or.big.number(45.8)
small.or.big.number(108)

#
# Nesting if/then/else statements
#
number.size <- function(x){
  if (x < 10) {
    print("Small",quote=FALSE)  # Quotes removed from small
  } else {
    if (x >= 10 & x <= 30){
      print("Medium")
    } else {
      return("Large")
    }
  }
}
#
number.size(16)
number.size(-10)
number.size(35)

#
# Team Questions
# 1. Write a function that will take an integer n as input.
#    The function should generate n random points in the square
#    [-1,1]x[-1,1], and return the proportion of those points
#    that land in the unit circle x^2 + y^2 <= 1.
# 2. Evaluate your function for n = 50,000.
#

my_func = function(n) {
  xy <- runif(n,-1,1)
  if(xy[0]^2+xy[1]^2<=1) return
}
