# Marcus Rosti
# mer3ef
# Stat 6021
# Homework 3

setwd("~/MSDS/Fall/Stat_6021/Homework/hw3")
library(gdata)

# 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.8, 3.11

nfl <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B1.XLS")
mpg <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B3.XLS")
chem <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B5.XLS")
pnut <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B7.XLS")

################################################################################################
# Problem 3.1
################################################################################################

# a
nfl.lm <- lm(y ~ x2 + x7 + x8, data = nfl)

# b
anova(nfl.lm)

# c
t_x2 <- nfl.lm$coefficients[2] / 0.000695 # 5.18
t_x7 <- nfl.lm$coefficients[3] / 0.088233 # 2.2
t_x8 <- nfl.lm$coefficients[4] / 0.001277 # -3.77
# at .05, they're all significant

# d
sum <- summary(nfl.lm)
nfl.r.sq <- sum$r.squared # .7863
nfl.adj.r.sq <- sum$adj.r.squared # .7596

# e
nfl.lm.partial <- lm(y ~ x2 + x8,data = nfl)
# H_0: x7 contributes nothing to the model
anova(nfl.lm.partial,nfl.lm) # we reject h_0, therefore x7 must contribute something

################################################################################################
# Problem 3.2
################################################################################################

cor(nfl$y, nfl.lm$fitted.values) ^ 2 # .7863

################################################################################################
# Problem 3.3
################################################################################################

# a
confint(nfl.lm)[3,]
#       2.5 %     97.5 %
#  0.01185532 0.37606510

# b
predict(nfl.lm, newdata = data.frame(x2 = 2300, x7 = 56, x8 = 2100), interval = "confidence")
#        fit      lwr      upr
# 1 7.216424 6.436203 7.996645

################################################################################################
# Problem 3.4
################################################################################################

# a
nfl.lm.3.4 <- lm(y ~ x7 + x8, data = nfl)

# b
sum.3.4 <- summary(nfl.lm.3.4)
nfl.3.4.r.sq <- sum.3.4$r.squared # .5477
nfl.3.4.adj.r.sq <- sum.3.4$adj.r.squared # .5114
# They are both significantly less than

# c
confint(nfl.lm.3.4)[3,]
#        2.5 %       97.5 %
# -0.010156368 -0.002916818

# d
# Since the values of each variable's t stat is based on the presence of the other predictors,
#   I'd say that there is some interaction between 7 and 8 that isn't exposed when x2 is in the model.
#   It also gives us a higher predictive power

################################################################################################
# Problem 3.5
################################################################################################

# a
mpg.lm <- lm(y ~ x1 + x6, data = mpg)

# b
anova(mpg.lm)

# c
sum.3.5 <- summary(mpg.lm)
sum.3.5$r.squared # 0.7872928
sum.3.5$adj.r.squared # 0.7726233
# R^2 is higher by definition and r^2 is only marginally higher, not enought to make a conclusion

# d
confint(mpg.lm)[2,]
#       2.5 %      97.5 %
# -0.06569892 -0.04059641

# e
mpg.lm.beta.1.t <- mpg.lm$coefficients[2] / 0.006137 # -8.66
mpg.lm.beta.2.t <- mpg.lm$coefficients[3] / 0.670277 #  1.43

# f
predict(mpg.lm, newdata = data.frame(x1 = 275,x6 = 2), interval = "confidence")
#        fit      lwr      upr
# 1 20.18739 18.87221 21.50257

# g
predict(mpg.lm, newdata = data.frame(x1 = 275,x6 = 2), interval = "prediction")
#        fit     lwr      upr
# 1 20.18739 13.8867 26.48808

################################################################################################
# Problem 3.6
################################################################################################

#        fit      lwr      upr
# 1 20.69879 14.34147 27.05611

# x6 actually makes our model worse
#   x6 detracts from the predictive power of our model

################################################################################################
# Problem 3.8
################################################################################################

# a
chem.lm <- lm(y ~ x6 + x7, data = chem)

# b
sum.3.8 <- summary(chem.lm)
anova(chem.lm)
sum.3.8$r.squared # 0.699644
sum.3.8$adj.r.squared # 0.6746144
# we regect the null for the f test, therefore the regression terms must be significant

# c
sum.3.8
# beta_6 tstat 6.742
# beta_7 tstat 2.247
# At alpha of .05 both of these statistics are significant,
#   meaning they cannot be removed from the model

# d
confint(chem.lm)[2,] # beta_6
#     2.5 %     97.5 %
# 0.01285196 0.02419204

confint(chem.lm)[3,] # beta_7
#     2.5 %    97.5 %
# 0.1782076 4.1932983

# e
chem.lm.partial <- lm(y ~ x6,data = chem)
sum.3.8.partial <- summary(chem.lm.partial)
sum.3.8.partial$r.squared # 0.6364504
sum.3.8.partial$adj.r.squared # 0.6219084

# R^2 is lower in both cases, thus adding to the idea that x7
#   adds predictive and explanatory power to our model
# I'm more satisfied with the full model

# f
confint(chem.lm.partial)[2,] # beta_6
#      2.5 %     97.5 %
# 0.01335688 0.02543261
# this interval is larger

# g
mse = function(model) {
  return(mean(model$residuals ^ 2))
}

mse(chem.lm) # 87.54945
mse(chem.lm.partial) # 105.9695
# Again, the partial model has a higher mse and lower predictive power

################################################################################################
# Problem 3.11
################################################################################################

# a
pnut.lm <- lm(y ~ ., data = pnut)

# b
sum.3.11 <- summary(pnut.lm)
anova(pnut.lm)
sum.3.11$r.squared # 0.9372286
sum.3.11$adj.r.squared # 0.9058429

# by the ftest, at least one of the predictors is significant

# c
sum.3.11
# at a significance level of .05,
#  Beta_1 is barely significant
#  Beta_3 and Beta_4 are insignificant

# d
pnut.lm.partial <- lm(y ~ x2 + x5, data = pnut)

sum.3.11.partial <- summary(pnut.lm.partial)
anova(pnut.lm.partial)
sum.3.11.partial$r.squared # 0.9149136
sum.3.11.partial$adj.r.squared # 0.9018234

sum.3.11$r.squared # 0.9372286
sum.3.11$adj.r.squared # 0.9058429
# Based on this our model is a pretty close match to the data,
#   But the degrees of freedom are quite low.  Which affects
#   Adjusted r^2.  The partial model is only slightly worse

anova(pnut.lm.partial,pnut.lm)
# we fail to regect the null, therefore the extra vars add nothing to the model

# e
confint(pnut.lm)[3,]
#     2.5 %    97.5 %
# 0.1537804 0.4105053

confint(pnut.lm.partial)[2,]
#     2.5 %    97.5 %
# 0.1550559 0.4092298

# the second model is only slightly better than the full model
