# Marcus Rosti

setwd("~/MSDS/Fall/Stat_6021/Homework/hw7")
library(gdata)
library(car)

# Homework 7
#
# This file provides sample code for computing Cook's D, DFBETAS,
# and DFFITS.  Included are guidelines for using the values to
# identify possible influential points.
#
# Some sample data and a plot:
bf <- read.csv("bodyfat.csv", header = TRUE)
plot(bf, pch = 20, cex = .2)

bf.lm <- lm(fat ~ ., data = bf)

## Cook's distance, with one value per observation
cd <- cooks.distance(bf.lm)
cd

# Guideline: Treat as influential any observation with
# Cook's distance > 1
(1:length(cd))[cd > 1]  # Observations with Cook's > 1

## DFBETAS, we get an entire set of coefficients for each observation
db <- dfbeta(bf.lm)
db

# Guideline: If db_{j,i} > 2/sqrt(n), then consider observation
# i as influential on coefficient j.
db > 2 / sqrt(20)

## DFFITS, we get one value per observation
df <- dffits(bf.lm)
df

# Guideline: If db_i > 2*sqrt(p/n), then consider observation
# i as influential
(1:length(df))[df > 1]

################################################################################
# 6.12, 6.13, 6.14, 6.15
################################################################################

################################################################################
#
# 6.12
#
################################################################################

wine <- read.xls("data-table-B11.XLS")
plot(wine)
wine.lm <- lm(Quality ~ . , data = wine)
summary(wine.lm)

db <- dfbeta(wine.lm)
db > 2 / sqrt(nrow(wine))

df <- dffits(wine.lm)
(1:length(df))[abs(df) > 2 * sqrt((ncol(wine) - 1) / nrow(wine))]

cd <- cooks.distance(wine.lm)
(1:length(cd))[abs(cd) > 1]

boxplot(wine$Clarity)
plot(Quality ~ Clarity, data = wine)
summary(lm(Quality ~ Clarity, data = wine))

# It looks like the only outliers come from Clarity; however, it seems that clarity has nothing to do with quality,
#   Therefore I would recommend not using clarity as a predictor.

################################################################################
#
# 6.13
#
################################################################################
heat <- read.xls("data-table-B12.XLS")
plot(heat)
heat.lm <- lm(pitch ~ . , data = heat)
summary(heat.lm)

db <- dfbeta(heat.lm)
abs(db) > 2 / sqrt(nrow(heat))

df <- dffits(heat.lm)
(1:length(df))[abs(df) > 2 * sqrt((ncol(heat) - 1) / nrow(heat))]

cd <- cooks.distance(heat.lm)
(1:length(cd))[abs(cd) > 1]

# So it looks like 28, 29 and 32 seem to be influential points.  They also seem to be influential points in
#   the individual parameters.  Therefore it maybe worth it to remove them in terms of our inference model.
#   Again, however since the dataset is so small, dropping them might not be worth it in terms of DF

################################################################################
#
# 6.14
#
################################################################################
jet <- read.xls("data-table-B13.XLS")
plot(jet)
# there's a ton of colinearity
jet.lm <- lm(y ~ . , data = jet)
plot(jet.lm)
summary(jet.lm)
vif(jet.lm)

db <- dfbeta(jet.lm)
abs(db) > 2 / sqrt(nrow(jet))

df <- dffits(jet.lm)
(1:length(df))[abs(df) > 2 * sqrt((ncol(jet) - 1) / nrow(jet))]

cd <- cooks.distance(jet.lm)
(1:length(cd))[abs(cd) > 1]

# 11 and 20 stick out in 11 and 20. Again the degrees of freedom are pretty small.
# It may be neglegent to remove them.  I might just say remove 20 and leave the others

################################################################################
#
# 6.15
#
################################################################################
elec <- read.xls("data-table-B14.XLS")
plot(jet)
# there's a ton of colinearity
elec.lm <- lm(y ~ x1 + x2 + x3 + x4 , data = elec)
plot(elec.lm)
summary(elec.lm)
vif(elec.lm)

db <- dfbeta(elec.lm)
abs(db) > 2 / sqrt(nrow(elec))

df <- dffits(elec.lm)
(1:length(df))[abs(df) > 2 * sqrt((ncol(elec) - 1) / nrow(elec))]

cd <- cooks.distance(elec.lm)
(1:length(cd))[abs(cd) > 1]

# This one is even more frustrating, 2 and 4 clearly stick out, but the other points
# appear to vary randomly.  They'll stick out in one aspect but not the next. So I
# would recommend dropping 2 and 4 but leave the rest.