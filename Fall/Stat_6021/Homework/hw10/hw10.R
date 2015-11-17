################################################################################
#
# Homework problems
# 13.1, 13.2, 13.5, 13.25
#
################################################################################
library(gdata)
setwd("~/MSDS/Fall/Stat_6021/Homework/hw10")

################################################################################
#
# 13.1
#
################################################################################
p131 <- read.xls("../../linear_regression_5e_data_sets/Chapter 13/Problems/data-prob-13-1.XLS")

# a
p131$y <- as.factor(p131$y)
p131.lm <- glm(y~x,data=p131,family="binomial")
summary(p131.lm)

# b
1-pchisq(14.254,1,lower.tail = FALSE) # .9998403
# Therefore we fail to reject the null hypothesis at alpha = .05 that logistic regression is wrong

# c
exp(-0.017705)-1 # -1.75%
# For every unit increase in speed, you are 1.75% less like to be hit.

# d
p131.lm.quad <- glm(y~poly(x,2),data=p131,family="binomial")
summary(p131.lm.quad)
anova(p131.lm,p131.lm.quad,test = "Chisq")
# p = .9889 -- Therefore we fail to reject that the large model fits the data better

################################################################################
#
# 13.2
#
################################################################################
p132 <- read.xls("../../linear_regression_5e_data_sets/Chapter 13/Problems/data-prob-13-2.XLS")
# a
p132$y <- as.factor(p132$y)
p132.lm <- glm(y~x,data=p132,family="binomial")
summary(p132.lm)

# b
anova(p132.lm)
1-pchisq(5.0906,1,lower.tail = FALSE) # 0.9759441
# Therefore we fail to reject the null hypothesis at alpha = .05 that logistic regression is wrong

# c
exp(1000*0.0002009)-1 # 22.2 %
# For every $1000 increase in income, you are 22% more likely to own a home

# d
p132.lm.quad <- glm(y~poly(x,2),data=p132,family="binomial")
summary(p132.lm.quad)
anova(p132.lm,p132.lm.quad,test = "Chisq")
# p = 0.2924 -- Therefore we fail to reject that the large model fits the data better

################################################################################
#
# 13.5
#
################################################################################
p135 <- read.csv("data-prob-13-5.csv")

# a
p135.lm <- glm(y ~ . ,data=p135, family=binomial)

# b
summary(p135.lm)
1 - pchisq(27.726 - 21.082,19-17,lower.tail = FALSE) # 0.9639194
# Therefore we fail to reject the null that logistic regression is inapporpriate

# c
summary(p135.lm)
exp(1000 * 7.382e-05)-1 # 7.38 % increase in the probability of buying a vehicle for every $1000 increase in income
exp(0.9879)-1 # 168 % increase in the probability of buying a vehicle for every unit increase in age

# d
1/(1+exp(-(-7.047e+00+7.382e-05*45000+9.879e-01*5))) # 77.10766

# e
p135.lm.inter <- glm(y ~ . + x1*x2 ,data=p135, family=binomial)

anova(p135.lm,p135.lm.inter,test="Chisq")
# We reject the null hypothesis that larger model is insignificant

# f
summary(p135.lm,test="Chisq")
# beta_1_pval = 0.247, so we fail to reject the null that beta_1 = 0
# beta_2_pval = 0.061, so at alpha .05 we fail to reject the null that beta_2 = 0

# g
confint(p135.lm)
#> confint(p135.lm)
#Waiting for profiling to be done...
#                      2.5 %       97.5 %
#x1            -4.361540e-05 0.0002184223
#x2             1.544228e-01 2.2872127855

################################################################################
#
# 13.25
#
################################################################################
p1325 <- read.xls("../../linear_regression_5e_data_sets/Chapter 13/Problems/data-prob-13-25.XLS")
library(ggplot2)

# a
ggplot(p1325, aes(x=Temperature.at.Launch, y=At.Least.One.O.ring.Failure)) + geom_point() +
  stat_smooth(method="glm", family="binomial", se=TRUE)

p1325.lm <- glm(At.Least.One.O.ring.Failure~.,data=p1325,family=binomial)

# b
summary(p1325.lm)
exp(-0.17132) - 1 # -15.74 % . so for every unit increase in temperature, it is 15% less likely to have at least one failure

# c
1/(1+exp(-(10.87535-0.17132*50))) # 0.9096484 so 90% chance of failure

# d
1/(1+exp(-(10.87535-0.17132*75))) # 0.1219974 so 12.19% chance of failure

# e
1/(1+exp(-(10.87535-0.17132*31))) # .99618 so 99.6% chance of failure... you uh shouldn't launch the rocket

# f
anova(p1325.lm)
1 - pchisq(28.975 - 23.030,23-22,lower.tail = FALSE) # 0.9852409
# So it's a very good line! I would say this is an apporpriate use of logistic regression

# g
p1325.lm.quad <- glm(At.Least.One.O.ring.Failure~poly(Temperature.at.Launch,2),data=p1325,family=binomial)
summary(p1325.lm.quad)
anova(p1325.lm,p1325.lm.quad,test="Chisq") # 0.5064
# We fail to reject the null, so the additional parameters are insignificant to the model
