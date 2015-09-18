# Homework 4
#
## Question 1: The objective of this exercise is to
#   use simulation to demonstrate the effects of
#   multicollinearity on the variance of regression
#   coefficients, and to observe how this influences
#   the accuracy of predictions.
#   For this question you will need two files,
#   "hw04.csv" and "hw04predict.csv".  Import
#   both into R.
#   (a) Repeat the following 1000 times:
#    (1) Select a random sample of 100 observations
#        from hw04.csv.
#    (2) Fit a linear model to the 100 observations,
#        using all 4 variables, and save the values 
#        of the estimated coefficients in separate
#        vectors.
#    (3) Use your linear model to predict the y-values
#        associated with the variables in "hw04predict.csv"
#        then compute the MSE.  Save this value in a
#        vector.
#    (4) Compute the standard deviation for the vectors
#        containing the coefficients, and the mean for 
#        the vector containing the MSE's.  Record those
#        values.
#   (b) Choose a suitable variable to remove from the
#       model, then repeat (1)-(4) from part (a).
#       How do the results in (4) from each part compare?
#       Explain what you observe in each case.

setwd("~/MSDS/Fall/Stat_6021/Homework/hw4")

dat <- read.csv("hw04.csv", header = TRUE)
pred <- read.csv("hw04predict.csv", header = TRUE)

library("car")

max <- 1000

beta_0 <- rep(0,max)
beta_1 <- rep(0,max)
beta_2 <- rep(0,max)
beta_3 <- rep(0,max)
beta_4 <- rep(0,max)
mse    <- rep(0,max)

for(i in 1:max) {
  # build model
  data_sample <- dat[sample(nrow(dat),100),]
  sample.lm <- lm(y~.-x3,data=data_sample)
  
  # save coeffs
  beta_0[i] <- sample.lm$coefficients[1]
  beta_1[i] <- sample.lm$coefficients[2]
  beta_2[i] <- sample.lm$coefficients[3]
  beta_3[i] <- sample.lm$coefficients[4]
  beta_4[i] <- sample.lm$coefficients[5]
  
  predicted_values <- predict.lm(sample.lm,pred)
  mse[i] <- sum((pred$y-predicted_values)^2)/(200-4)
}

partial<-c(mean(mse),sd(beta_0),sd(beta_1),sd(beta_2),sd(beta_3),sd(beta_4))

vif(lm(y~.-x3,data=dat))
