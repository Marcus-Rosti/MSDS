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
full.mse    <- rep(0,max)
partial.mse    <- rep(0,max)

for (i in 1:max) {
  # build model
  data_sample <- dat[sample(nrow(dat),100),]
  sample.lm <- lm(y ~ .,data = data_sample)

  # save coeffs
  beta_0[i] <- sample.lm$coefficients[1]
  beta_1[i] <- sample.lm$coefficients[2]
  beta_2[i] <- sample.lm$coefficients[3]
  beta_3[i] <- sample.lm$coefficients[4]
  beta_4[i] <- sample.lm$coefficients[5]

  predicted_values <- predict.lm(sample.lm,pred)
  full.mse[i] <- sum((pred$y - predicted_values) ^ 2) / 100
}

full <-
  c(mean(full.mse),sd(beta_0),sd(beta_1),sd(beta_2),sd(beta_3),sd(beta_4))

for (i in 1:max) {
  # build model
  data_sample <- dat[sample(nrow(dat),100),]
  sample.lm <- lm(y ~ . - x3,data = data_sample)

  # save coeffs
  beta_0[i] <- sample.lm$coefficients[1]
  beta_1[i] <- sample.lm$coefficients[2]
  beta_2[i] <- sample.lm$coefficients[3]
  beta_3[i] <- sample.lm$coefficients[4]
  beta_4[i] <- sample.lm$coefficients[5]

  predicted_values <- predict.lm(sample.lm,pred)
  partial.mse[i] <- sum((pred$y - predicted_values) ^ 2) / 100
}

partial <-
  c(mean(partial.mse),sd(beta_0),sd(beta_1),sd(beta_2),sd(beta_3),sd(beta_4))

full
partial

vif(lm(y ~ . - x3,data = dat))

# As far as I can tell x3 = x1 + x2 + (some noise).  The r^2 is too high for it not to be the case.  And the vif disappears
#   when we remove x3.  Therefore it should be removed from the model since it inflates the covariance


############################################################################################################
# Book programming questions
############################################################################################################
# reading in data
library(gdata)

chem <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B8.XLS")
pres <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B9.XLS")
life <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B16.XLS")
medi <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B17.XLS")
############################################################################################################
#
# Question 3.12
#
############################################################################################################
### a
chem.lm <- lm(y ~ .,data = chem)

### b
anova(chem.lm) # anova shows significance of the regressors, at alpha = .05
summary(chem.lm) # aR^2 = 0.842, so they explain much of the variance in y as well

### c
summary(chem.lm)
# t_beta_1 = 6.64
# t_beta_2 = 8.82
qt((1 - 0.05/2),33)
# t_critical = 2.03
# they're both greater than the t-stat so they both b_1 and b_2 are significant predictors

### d
summary(chem.lm)$r.squared     # 0.842
summary(chem.lm)$adj.r.squared # 0.832
# Over 80% of the variation is explained by the predictors. Shows our model has a fairly high explanatory power

summary(lm(y ~ x2,data = chem))$r.squared     # 0.468
summary(lm(y ~ x2,data = chem))$adj.r.squared # 0.452
# The model with two predictors explains the response about twice as well

### e
confint(chem.lm,level = 0.95)[3,]
#   2.5 % 97.5 %
#  0.0886 0.1292

confint(lm(y ~ x2,data = chem),level = 0.95)[2,]
#   2.5 % 97.5 %
#  0.0614 0.1340
# The interval is narrower with the extra regressor. It's because the variance is smaller do to the added regressor

############################################################################################################
#
# Question 3.13
#
############################################################################################################

### a
pres.lm <- lm(y ~ . , data = pres)

### b
summary(pres.lm)
# F-statistic: 31.9 on 4 and 57 DF,  p-value: 5.82e-14
# F - 31.9 means that at least one of the parameters is nonzero

### c
summary(pres.lm)
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   5.8945     4.3251    1.36   0.1783
# x1           -0.4779     0.3400   -1.41   0.1653
# x2            0.1827     0.0172   10.63  3.8e-15 ***
# x3           35.4028    11.0996    3.19   0.0023 **
# x4            5.8439     2.9098    2.01   0.0494 *

# The two variables, x1 and x4, fail the t test for significance at 0.05
#    Therefore I conclude that their values are zero and should be removed from the model

### d
summary(pres.lm)$r.squared     # 0.691
summary(pres.lm)$adj.r.squared # 0.67

pres.lm.partial <- lm(y ~ x2 + x3,data = pres)

summary(pres.lm.partial)$r.squared     # 0.666
summary(pres.lm.partial)$adj.r.squared # 0.655

# so even though the predictors are insiginficant they add explanatory power to the model
#   maybe in that case it is not best to drop them.  Further analysis is required

### e
confint(pres.lm,level = 0.99)[3,]
#  0.5 % 99.5 %
#  0.137  0.229

confint(pres.lm.partial,level = 0.99)[2,]
#  0.5 % 99.5 %
#  0.138  0.231

# The interval in the partial model expands suggesting that we've added some variance to our model

############################################################################################################
#
# Question 3.16
#
############################################################################################################

names(life)
### a
male.lm   <- lm(LifeExpMale   ~ People.per.TV + People.per.Dr, data = life)
female.lm <- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = life)

### b
summary(male.lm)   # F-statistic: 12.5 on 2 and 35 DF,  p-value: 7.86e-05
summary(female.lm) # F-statistic: 14.1 on 2 and 35 DF,  p-value: 3.28e-05
# They're both significant however, the male model is slightly less significant

### c
summary(male.lm)   # beta_1: -2.34, beta_2: -2.07
# at .05 significance level, we reget neither however they are so very nearly zero
summary(female.lm) # beta_1: -2.36, beta_2: -2.31
# at .05 significance level, we reget neither however they are so very nearly zero

### d
summary(male.lm)$r.squared       # 0.417
summary(male.lm)$adj.r.squared   # 0.384
summary(female.lm)$r.squared     # 0.446
summary(female.lm)$adj.r.squared #  0.414
# I mean, they're not great but the r^2s for both are still pretty good

### e
confint(male.lm,level = 0.95)[3,]
#     2.5 %    97.5 %
# -9.47e-04 -1.01e-05
confint(female.lm,level = 0.95)[3,]
#     2.5 %    97.5 %
# -7.67e-04 -5.01e-05

############################################################################################################
#
# Question 3.17
#
############################################################################################################

medi.lm <- lm(Satisfaction ~ Age + Severity + Anxiety,data = medi)
medi.lm.partial <- lm(Satisfaction ~ Age + Severity ,data = medi)

summary(medi.lm) # Anxiety looks insignificant
anova(medi.lm)

anova(medi.lm.partial,medi.lm) # Based on this I say we can drop Anxiety from the model

summary(medi.lm.partial) # the adjust r^2 is slightly higher 0.792 vs 0.789

# Simple linear regression surveys the interaction between to variables
# Multiple linear regression deals with the interaction of several variable to response
# In that way multiple has much more complexity and worries about the interaction between regressors
# Something that's significant in simple may not be in the pressence of other regressors
# Always look at the effects of adding and removing regressors.

