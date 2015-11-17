# Homework 8
#
# This file has some sample code and data for looking at polynomial
# regression models.  Further down is sample code that incorporates
# categorical data into a regression model.
#
# Polynomial Regression -- Some sample data and a plot:
d08 <- read.csv("hw08.csv", header = TRUE)
plot(d08, pch=20, cex=.2)

d08a.lm <- lm(y~., data=d08)
summary(d08a.lm)

# Here we add higher powers of each variable
d08b.lm <- lm(y~poly(x1,3)+poly(x2,3)+poly(x3,3), data=d08)
summary(d08b.lm)
vif(d08b.lm)

# We can add individual variables to the data frame
d08$x22 <- d08$x2^2  # Adds x2^2 to the data frame d08
d08c.lm <- lm(y~., data=d08)
summary(d08c.lm)
plot(d08, pch=20, cex=.2)
vif(d08c.lm)

d08$x32 <- d08$x3^2  # Adds x3^2 to the data frame d08
d08d.lm <- lm(y~., data=d08)
summary(d08d.lm)
plot(d08, pch=20, cex=.2)
vif(d08d.lm)
# Maybe just x1, x3, and x2^2 should be in the model
d08e.lm <- lm(y~x1+x3+x22, data=d08)
summary(d08e.lm)

# Let's try the partial F-test
anova(d08e.lm, d08d.lm)

# Maybe too much removed?  Let's put x3^2 back in
d08f.lm <- lm(y~x1+x3+x22+x32, data=d08)
summary(d08f.lm)

# Now x3 isn't significant; Remove?
d08g.lm <- lm(y~x1+x22+x32, data=d08)
summary(d08g.lm)

# Let's compare with all variables included
d08d.lm <- lm(y~., data=d08)  # Just to be sure we have all vars.
anova(d08g.lm, d08d.lm)

## Regression with indicator variables
#
# "insinv.csv"  contains data from 20 insurance companies
#  y = time (in months) to adopt an innovation
#  x1 = size of firm
#  x2 = firm type (0 = mutual, 1 = stock)
ins <- read.csv("insinv.csv", header = TRUE)
plot(ins, pch=20, cex=.2)
plot(y~x1, data=ins, pch=20, cex=.5, col=c("red","blue")[x2+1])

# Let's run separate regressions for each type of company
ins0.lm <- lm(y~x1,data=ins[1:10,])
summary(ins0.lm)
ins1.lm <- lm(y~x1,data=ins[11:20,])
summary(ins1.lm)

# From the output, it appears that the slopes are different but
# the y-intercepts might be the same.  Let's run the regression
# with the data pooled.
ins.lm <- lm(y~x1+x2,data=ins)
summary(ins.lm)

# Since beta2 is significant, there is a genuine difference
# between the y-intercept for each type of company.
# To compare the slopes, we add an interaction term:
ins$x12 <- ins$x1*ins$x2
ins2.lm <- lm(y~x1+x2+x12,data=ins)
summary(ins2.lm)

# You get the same result with the code below, which automatically
# includes the interaction term:
ins2.lm <- lm(y~x1*x2,data=ins)
summary(ins2.lm)

# With categorical data expressed as numbers, you need to tell R
# that your data is categorical with the "factor" command.
ins$x2 <- factor(ins$x2)
ins.lm <- lm(y~x1+x2,data=ins)
summary(ins.lm)

# Then you can create your own interaction terms:
ins$x1*ins$x2

# But R can handle this issue automatically.
ins2.lm <- lm(y~x1*x2,data=ins)
summary(ins2.lm)

# "soap.csv" data contains data from a factory producing bars of soap
#  y = amount of scrap
#  x1 = speed of line
#  x2 = line number (0 or 1)
soap <- read.csv("soap.csv", header = TRUE)
plot(soap, pch=20, cex=.2)
plot(y~x1, data=soap, pch=20, cex=.5, col=c("red","blue")[x2+1])

soap.lm <- lm(y~., data=soap)
summary(soap.lm)

# Add the interaction term
soap.lm <- lm(y~x1*x2, data=soap)
summary(soap.lm)

######################################################################################################
#
# Homework problems
#
######################################################################################################
library(gdata)
library(ggplot2)

setwd("~/MSDS/Fall/Stat_6021/Homework/hw8")
######################################################################################################
#
# 8.3
#8.3 Consider the delivery time data in Example 3.1. In Section 4.2.5 noted that these observations were collected in four cities, San Diego, Boston, Austin, and Minneapolis.
#a. Develop a model that relates delivery time y to cases x1, distance x2, and the city in which the delivery was made. Estimate the parameters of the model.
#b. Is there an indication that delivery site is an important variable?
#c. Analyze the residuals from this model. What conclusions can you draw regarding model adequacy?
######################################################################################################
city <- read.xls("data-ex-3-1.xls")
city$City <- as.factor(c(rep("San Diego",7),rep("Boston",10),rep("Austin",6),"Minneapolis","Minneapolis"))

city.lm <- lm(Delivery.Time..y~.-Observation,data=city)

summary(city.lm)


## b
# there's some indication that boston is significnatlly different however the other ones are the same

## c
plot(city.lm)
# There's a big trend in the residuals. Looks somewhat quadradtic.  however the individual loots are drawn,
# that trend disappears.

######################################################################################################
#
# 8.5
#
######################################################################################################
auto <- read.xls("data-table-B3.xls")
# Plot that
ggplot(auto,aes(y=y,x=x10,color=as.factor(x11))) + geom_point()

auto.lm <- lm(y~x10+x11, data=auto)
summary(auto.lm)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
#  (Intercept) 39.1919052  2.5570509  15.327 1.92e-15 ***
#  x10         -0.0047484  0.0009544  -4.975 2.72e-05 ***
#  x11         -2.6958431  1.9805597  -1.361    0.184
# # # # # #
# It looks like the difference in intercept is insignificant or meaningless.
# sinces the x11 coefficient doesn't add value in this simple case, I can assume,
# That they are the same.

auto.lm.interaction <- lm(y~x10*x11, data=auto)
summary(auto.lm.interaction)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
#  (Intercept)  58.108420   5.077985  11.443 4.53e-12 ***
#  x10          -0.012517   0.002055  -6.090 1.44e-06 ***
#  x11         -26.724910   6.107349  -4.376 0.000152 ***
#  x10:x11       0.009035   0.002217   4.076 0.000342 ***
# # # # # #
# Here we see that the slop of the line is very significant.  And on top of that,
# the difference in intercept becomes significant.  This would imply that the two
# catagories have significantly different models.

######################################################################################################
#
# 8.6
#
######################################################################################################
nfl <- read.xls("data-table-B1.xls")
ggplot(nfl,aes(y=y,x=x8,color=as.factor(sign(x5)))) + geom_point()
ggplot(nfl,aes(y=y,x=x7,color=as.factor(sign(x5)))) + geom_point()


nfl.lm <- lm(y~x8+x7+as.factor(sign(x5)), data=nfl)
summary(nfl.lm)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)
# (Intercept)          19.813559   9.688921   2.045  0.05247 .
# x8                   -0.006337   0.001719  -3.685  0.00122 **
# x7                   -0.006825   0.118841  -0.057  0.95470
# as.factor(sign(x5))0 -0.460504   2.466185  -0.187  0.85351
# as.factor(sign(x5))1  1.872527   0.962129   1.946  0.06394 .
# # # # # #
# The difference in factors looks muddy.  This would in indicate that
# there isn't as large a difference in these points as we think. Based
# on the t values I would throw out those variables as they do not look
# useful to us.
