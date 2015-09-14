# Team Assignment 2

setwd("~/MSDS/Fall/Stat_6021/Team_Assign/team2")
# Possibly handy code from Team Assignment 1:
x <- read.table("x-values.txt")[,1]

# Generate corresponding y-values according to the 
# model y ~ 25 + 4x + e, where e~N(0,var=12^2)
true_mean <- 0
true_var  <- 12^2

y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

## Team Assignment 2
#
#  Please submit one set of answers per team. All answers can be submitted
#  in an annotated R file.

## Question 1: This question continues with the model from Team Assignment 1,
#   which used values of x from "x-values.txt" and generated y values using
#   the model y ~ 25 + 4*x + e, where e~N(0,var=12^2).  Carry out each requested
#   simulation 1000 times.
#   (a) Simulate MS_Res, and find a 95% confidence interval for sigma^2 by 
#       finding the 2.5th and 97.5th percentiles to give the lower and upper
#       confidence limits.
Sxx <- sum((x-mean(x))^2)
sigma.sq.array<-rep(0,1000)
t_test <- req(0,1000)
for(i in 1:1000) {
  y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  
  lm.temp <- lm(y~x)
  lm.sum<-summary(lm.temp)
  sigma.sq<-sum((lm.sum$residuals)^2)/98
  sigma.sq.array[i] <- sigma.sq
  MSres <- sqrt(sigma.sq/Sxx)
  
  t_test_0<-(lm.temp$coefficients[2] - 4)/MSres
  if(t_test_0 > 1.984467 | t_test_0 < (-1.984467)) {
    t_test[i] <- 1
  } else {
    t_test[i] <- 0
  }
  
}
quantile(sigma.sq.array, c(.025,.975))
#   (b) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4.  Test
#       at a 5% significance level each time, and determine the proportion of 
#       times that the null hypothesis is rejected, implying that beta_1 not= 4.

## Question 2: For this problem generate a single data set from the model for
#   Question 1.  Here we're going to implement a simple form of the bootstrap
#   approximation method.  Repeat (a) and (b) 1000 times:
#   (a) From your data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- that's OK and expected.
#   (b) Use your sample to generate a regression equation, and then save
#       the values of hat(beta_0) and hat(beta_1).
#   (c) Find a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the
#       confidence intervals contain the true parameter values?

## Question 3: Import the data set "mult-xvalues.csv" which contains 100 sets of 
#   data for the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2), then 
#       pair up the y-values with corresponding rows from the mult-xvalues.csv
#       data.
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values are explanatory variables.
#   (c) Determine how many of the explanatory variables are significant at the 5%
#       level.
#   (d) Determine the proportion of significant variables in the 100 simulations,
#       then compare with the expected theoretical value.