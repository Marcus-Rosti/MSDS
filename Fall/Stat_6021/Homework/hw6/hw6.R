setwd("~/MSDS/Fall/Stat_6021/Homework/hw6")

library(MASS)
library(gdata)
library(leaps)

# Homework 6
#  5.2, 5.4, 5.5, 5.7, 5.9, 5.10
# This file provides sample code for performing the Box-Cox
# analysis.
#
# Some sample data and a plot:
sampdat <- read.csv("hw06-sample.csv", header = TRUE)
plot(sampdat, pch = 20, cex = .2)

samp.lm <- lm(y ~ x, data = sampdat)
summary(samp.lm)
abline(samp.lm)

# We want to choice of lambda that maximizes the log-likelihood.
bc <- boxcox(y ~ x, data = sampdat, plotit = T, interp = T)
bc   # Lists log-likelihoods and corresponding lambdas.

maxlambda <- bc$x[bc$y == max(bc$y)]
maxlambda
# Note: Generally choose a sensible lambda in the 95% interval.

#################################################################################
#
# Problem 5.2
#
#################################################################################
pres <- read.xls("data-prob-5-2.XLS")
### a
plot(pres$vapor ~ pres$temp) # it looks extremely quadratic

### b
pres.lm <- lm(vapor ~ temp, data = pres)
abline(pres.lm)
# linear regression is inapporpriate for this model without a transformation

### c
logvap <- log(pres$vapor)
invtemp <- -1 / pres$temp
pres.lm.trans <- lm(logvap ~ invtemp)
plot(logvap ~ invtemp)
abline(pres.lm.trans)
# it's essentially a perfect fit now

#################################################################################
#
# Problem 5.4
#
#################################################################################
fivefour <- read.xls("data-prob-5-4.XLS")

plot(y ~ x, data = fivefour) # it looks like OLS will be a valid model
fivefour.lm <- lm(y ~ x, data = fivefour)
summary(fivefour.lm) # the r^2 value is incredibly high, and each parameter is significant
anova(fivefour.lm) # f-stat says our reg is significant

#################################################################################
#
# Problem 5.5
#
#################################################################################
manu <- read.xls("data-prob-5-5.XLS")

plot(defects ~ weeks, data = manu)

### a
manu.lm <- lm(defects ~ weeks, data = manu)
abline(manu.lm) # fits pretty well but it looks like there's a trend in the residual plot

### b
logdefects <- log(manu$defects)
plot(logdefects ~ weeks, data = manu)
manu.lm.trans <-
  lm(logdefects ~ weeks, data = manu) # the trend in the residual line is gone.
abline(manu.lm.trans)                 # This is a much better fit overall
summary(manu.lm.trans)

#################################################################################
#
# Problem 5.7
#
#################################################################################
# y = percent conversion
meth <- read.xls("data-table-B20.xls",header = TRUE,method = "csv")

plot(meth) # x2 has a strong correlation but is slightly quadratic
summary(regsubsets(X.y ~ ., data = meth)) # x2 and x3 appear to give good results

cor(meth) # x1 also has a pretty high correlation.  also, x3 and x2 look autocorrelated

# I'm going to use x1, x2 and x3 because they have the best traits based on regsubs

meth.lm <- lm(X.y ~ x_1. + X.x_2. + X.x_3., data = meth)
plot(meth.lm) # there's definitely a trend in the residual line
x2 <- meth$X.x_2. ^ 2
x3 <- sqrt(meth$X.x_3.)
meth.lm.trans <- lm(X.y ~ x_1. + x2 + x3, data = meth)
order(hatvalues(meth.lm.trans))
plot(meth.lm.trans)

# There just seems to be a lot of points that drag the regression line.
# I'm not familiar enough with this data to recommend removing the violating points.
# There's a subset of the data that behaves will but it's hard to say if those are
#   violating points, or reasons to say it's a poor choice for regression.

#################################################################################
#
# Problem 5.9
#
#################################################################################
clat <- read.xls("data-table-B8.xls")

### a
plot(clat)
plot(lm(y ~ .,data = clat))
#  x2 obviously needs to be transformed.

### b
# y ~ x2 looks pretty logarithmic
plot(clat$y ~ log(clat$x2)) # oh yeah much better
clat$x2 <- log(clat$x2)

cor(clat)
summary(lm(y ~ x2 + x1,data = clat))
summary(lm(y ~ x2 + as.factor(x1),data = clat))
plot(y ~ x2 + as.factor(x1),data = clat)

clat.lm <- lm(y ~ as.factor(x1) + x2, data = clat)

#################################################################################
#
# Problem 5.10
#
#################################################################################
kins <- read.xls("data-table-B9.xls")

### a
plot(kins)

# This looks like a very inapporpriate problem for linear regression
# Most of the variables x1-3 are factors, and y~x4 appears uncorrelated
cor(kins)
# x2 has a very strong correlation to y
summary(lm(data = kins, y ~ x2))
anova(lm(data = kins, y ~ x2),lm(data = kins, y ~ .))

# it looks like the only variable that means anything is x2!
summary(lm(data = kins, y ~ x2))
summary(lm(data = kins, y ~ x2 ^ 2))
summary(lm(data = kins, log(y) ~ x2))
# no model appears to preform better than just the model that includes x2
bc <- boxcox(y ~ x2, data = kins, plotit = T, interp = T)
bc   # even box cox says our linear model performs best
# I'd recommend no transformations