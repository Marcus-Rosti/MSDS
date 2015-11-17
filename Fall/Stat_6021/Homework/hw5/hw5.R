# Marcus Rosti
# mer3ef
# hw5
# 4.4, 4.5, 4.7, 4.9, 4.23, 4.24, 4.25

#################################################################################
#
# Data Sets and boilerplate
#
#################################################################################

library(gdata)

setwd("~/MSDS/Fall/Stat_6021/Homework/hw5")

mpg <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B3.XLS")
hou <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B4.XLS")
p21 <-
  read.xls(
    "~/MSDS/Fall/Stat_6021/linear_regression_5e_data_sets/Chapter 2/Problems/data-prob-2-10.XLS"
  )
ozn <-
  read.xls(
    "~/MSDS/Fall/Stat_6021/linear_regression_5e_data_sets/Chapter 2/Problems/data-prob-2-13.XLS"
  )

wsj <-
  read.xls(
    "~/MSDS/Fall/Stat_6021/linear_regression_5e_data_sets/Chapter 2/Problems/data-prob-2-18.XLS"
  )

air <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B-15.xls")

lyf <-
  read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B-16.xls")
#################################################################################
#
# 4.4
#
#################################################################################

mpg.lm <- lm(y ~ x1 + x6, data = mpg)
mpg.lm.sum <- summary(mpg.lm)

### a

mpg.stdres = rstandard(mpg.lm)

# Ooo that looks interesting
# It seems like there's some underlying system that's making the residuals
#   follow some sort of trending on the upper middle of the plot
# It's probably due to x6 being catagorical
qqnorm(mpg.stdres)

qqline(mpg.stdres)

### b
plot(mpg.lm$fitted.values,mpg.lm.sum$residuals)
# There looks to be a slight lack of heteroskedasticity

### c
avPlots(mpg.lm)
# Each variable adds explanatory power to the model

### d
rstandard(mpg.lm) # Studentized - These mitigate the effect of heteroskedasticity
plot(rstandard(mpg.lm))

rstudent(mpg.lm) # R-Studentized - Show the effect of removing x_i from the model
plot(rstudent(mpg.lm))

#################################################################################
#
# 4.5
#
#################################################################################

hou.lm <- lm(y ~ ., data = hou)

hou.lm.sum <- summary(hou.lm)

### a

hou.lm.stdres <- rstandard(hou.lm)

qqnorm(hou.lm.stdres)

qqline(hou.lm.stdres)

# Other than in the tails, the residuals are fairly normal

### b
plot(hou.lm$fitted.values,hou.lm.sum$residuals)
# There are so few variables here that it's hard to tell if there's a meaningful trend

### c
avPlots(hou.lm)
# It looks like x1 and x2 provide the most increase given the presense of the other variables
#   x5 and x9 as well although less so than the others.  x3, x6, x7 and x8 seem to add nothing

### d
studres(hou.lm) # Studentized - These mitigate the effect of heteroskedasticity
plot(rstandard(hou.lm))

rstudent(hou.lm) # R-Studentized - Show the effect of removing x_i from the model
plot(rstudent(hou.lm))

#################################################################################
#
# 4.7
#
#################################################################################

p21.lm <- lm(sys.bp ~ weight ,data = p21)

p21.lm.sum <- summary(p21.lm)

### a

p21.lm.stdres <- rstandard(p21.lm)

qqnorm(p21.lm.stdres)

qqline(p21.lm.stdres)

# This looks absolutely non normal.  There's a huge trend in these values that's violated

### b

plot(p21.lm$fitted.values,p21.lm.sum$residuals)

# These residuals show extensive heteroskedasticity

### c
plot(ts(p21)) # Plot it as a time series
# It looks like blood pressure and weight are well correlated
#   But it's not linear

#################################################################################
#
# 4.9
#
#################################################################################

ozn.lm <- lm(days ~ index ,data = ozn)

ozn.lm.sum <- summary(ozn.lm)

### a

ozn.lm.stdres <- rstandard(ozn.lm)

qqnorm(ozn.lm.stdres)

qqline(ozn.lm.stdres)

# This looks very normal

### b

plot(ozn.lm$fitted.values,ozn.lm.sum$residuals)

# Can't really tell anything here. It looks fairly regular

### c
plot(ts(ozn)) # Plot it as a time series
# Doesn't show really much correlation that I can detect

#################################################################################
#
# 4.23
#
#################################################################################

wsj.lm <-
  lm(Returned.Impressions.per.week..millions. ~ Amount.Spent..Millions. ,data = wsj)

wsj.lm.sum <- summary(wsj.lm)

### a

wsj.lm.stdres <- rstandard(wsj.lm)

qqnorm(wsj.lm.stdres)

qqline(wsj.lm.stdres)

# Fairly normal however the values on the right side of the graph appear to be outliers

### b

plot(wsj.lm$fitted.values,wsj.lm.sum$residuals)

# This looks really bad.  All the fitted values are clustered to left hand side
#   I would say this violates the regression assumptions

#################################################################################
#
# 4.24
#
#################################################################################

air.lm <- lm(MORT ~ . - City, data = air)

air.lm.sum <- summary(air.lm)

### a

air.lm.stdres <- rstandard(air.lm)

qqnorm(air.lm.stdres)

qqline(air.lm.stdres)

# The tails look non normal but near the mean it does look

### b

plot(air.lm$fitted.values,air.lm.sum$residuals)

# There's a little bit of heteroskedasticity but not enough to violate the
#   assumptions of the regression line


#################################################################################
#
# 4.25
#
#################################################################################

lyf.lm <- lm(LifeExp ~ People.per.TV + People.per.Dr, data = lyf)

lyf.lm.sum <- summary(lyf.lm)

lyf.fem.lm <- lm(LifeExpMale   ~ People.per.TV + People.per.Dr, data = lyf)
lyf.mal.lm <- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = lyf)

lyf.fem.lm.sum <- summary(lyf.fem.lm)
lyf.mal.lm.sum <- summary(lyf.mal.lm)


### a
lyf.lm.stdres <- rstandard(lyf.lm)
lyf.fem.lm.stdres <- rstandard(lyf.fem.lm)
lyf.mal.lm.stdres <- rstandard(lyf.mal.lm)

qqnorm(lyf.lm.stdres)
qqline(lyf.lm.stdres)

qqnorm(lyf.fem.lm.stdres)
qqline(lyf.fem.lm.stdres)

qqnorm(lyf.mal.lm.stdres)
qqline(lyf.mal.lm.stdres)

# There's one big outlier that looks like it would have a lot of leverage on the regression line
# They all seem nearly the same

### b
plot(lyf.fem.lm$fitted.values,lyf.fem.lm.sum$residuals)
plot(lyf.mal.lm$fitted.values,lyf.mal.lm.sum$residuals)

# There's a few points that really stick out as outliers in both models



