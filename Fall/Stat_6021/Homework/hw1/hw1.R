setwd('/Users/RustyRosti/MSDS/Fall/Stat_6021/Homework/hw1')
library(gdata)

# source files
NFL   <- read.xls('data-table-B1.XLS')
solar <- read.xls('data-table-B2.XLS')
MPG   <- read.xls('data-table-B3.XLS')

######################################################
### Problem 1 ###
# a
nfl_model <- lm(y ~ x8, data = NFL)
summary(nfl_model)

# b
nfl_anova <- anova(nfl_model)
nfl_anova

# c
confint(nfl_model,"x8",level=.95)

# d
summary(nfl_model)$r.squared

# e
newdata = data.frame(x8=2000) 

## the hard way
yhat      <- predict(nfl_model,newdata)
tval      <- qt(1-.05/2,28-2);
Sxx       <- sum(NFL$x8^2)-sum(NFL$x8)^2/28;
MSRss     <- sum(nfl_model$residuals^2)/26;
variation <- tval*sqrt(MSRss*((1/28)+(2000-mean(NFL$x8))^2/Sxx));

### upperbound
yhat + variation
### lowerbound
yhat - variation

## the easy way
predict(nfl_model, newdata, interval="confidence", level=.95) 

######################################################
### Problem 2 ###
newdata = data.frame(x8=1800) 
predict(nfl_model, newdata, interval="prediction", level=.90) 

######################################################
### Problem 3 ###
# a
solar_model <- lm(y ~ x4, data = solar)
summary(solar_model)

# b
anova(solar_model)

# c
confint(solar_model,"x4",level=.99)

# d
summary(solar_model)$r.squared

# e
newdata = data.frame(x4=16.5) 
predict(solar_model, newdata, interval="confidence", level=.95) 

######################################################
### Problem 4 ###
# a
mpg_model <- lm(y~x1, data = MPG)
summary(mpg_model)

# b
anova(mpg_model)

# c 
summary(mpg_model)$r.squared

# d
newdata = data.frame(x1=275) 
predict(mpg_model, newdata, interval="confidence", level=.95) 

# e
predict(mpg_model, newdata, interval="prediction", level=.95) 

# f
# The "confidence" interval refers to the mean response at x1=275.  That is,
#   it is relating to the mean of all subsets around x1=275
# The "prediciton" interval refers to the spread of data for all subsets about
#   the point x1=275. That is 95% of all data points of subsets will fall
#   between those two bounds

######################################################
### Problem 5 ###
# a
mpg_model_2 <- lm(y~x10, data = MPG)
summary(mpg_model_2)

# b
anova(mpg_model_2)

# c 
summary(mpg_model_2)$r.squared

# ?d?
# I would say that x1 explains the variation better since it has a higher r^2 value
#   since these are both models of one variable
