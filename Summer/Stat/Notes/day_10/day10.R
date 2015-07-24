# STAT 6430
# Day 10 -- R
#
## Team prediction competition
#

mse <- function(sm) { 
  mse <- mean(sm$residuals^2)
  return(mse)
}

setwd("~/MSDS/Summer/Stat/Notes/day_10")
library(ggplot2)
library(leaps)

# The data set "fire-train.csv" contains 300 records for forest fires.
# The response variable is "area" which gives the area burned by each
# fire.  There are 12 explanatory variables.  (Variables are listed
# below.)

fire.tr = read.csv("fire-train.csv", header = TRUE)

# You objective is to develop a linear model to predict area based on
# the other information provided.  Once you have your preferred model,
# use it to predict the areas of the 150 fires with variables given
# in the file "fire-predict.csv."

fire.pred = read.csv("fire-predict.csv", header = TRUE)

fire.tr.stripped <- fire.tr[c(-7,-195),]
plot(lm(data=fire.tr.stripped,area~.))
summary(lm(data=fire.tr.stripped,area~.-FFMC-wind-rain-RH))
summary(regsubsets(data=fire.tr.stripped,area~.-FFMC-wind-RH))
subset <-regsubsets(data=fire.tr.stripped,area~.)
plot(subset,scale="adjr2")
subset$rss
mse(lm(data=fire.tr.stripped,area~.))

install.packages("pls")
library(pls)

train <- sample(1:nrow(fire.tr.stripped),nrow(fire.tr.stripped)/2)

################################################################################################

pcr.fit<-pcr(data=fire.tr.stripped,area~.,validation="CV",subset=train)
validationplot(pcr.fit,val.type = "MSEP")
mse(pcr.fit)
pcr.pred<-predict(pcr.fit,fire.tr.stripped[-train,],ncomp=7)
mean((pcr.pred-fire.tr.stripped[-train,]$area)^2)

################################################################################################

plsr.fit<-plsr(data=fire.tr.stripped,area~.,validation="CV",subset=train)
validationplot(plsr.fit,val.type = "MSEP")
mse(plsr.fit)
plsr.pred<-predict(plsr.fit,fire.tr.stripped[-train,],ncomp=7)
mean((plsr.pred-fire.tr.stripped[-train,]$area)^2)

################################################################################################

regfit.best <- regsubsets(area~.,data=fire.tr.stripped)
regfit.sum <- summary(regfit.best)
which.max(regfit.sum$adjr2)
which.min(regfit.sum$cp)

regfit.best[8]

# If your predictions are in a vector "preds" then the code below
# will export them to the file "teampreds.csv" which should then
# be uploaded into the appropriate place in the Collab "Assignments."

write.table(preds, file = "teampreds.csv", row.names=F, col.names=F, sep=",")

# Good luck!!

## Variables
# X - x-axis spatial coordinate: 1 to 9
# Y - y-axis spatial coordinate: 2 to 9
# month - month: "jan" to "dec" 
# day - day of the week: "mon" to "sun"
# FFMC - FFMC index: 18.7 to 96.20
# DMC - DMC index: 1.1 to 291.3 
# DC - DC index: 7.9 to 860.6 
# ISI - ISI index: 0.0 to 56.10
# temp - temperature in Celsius: 2.2 to 33.30
# RH - relative humidity in %: 15.0 to 100
# wind - wind speed: 0.40 to 9.40 
# rain - outside rain: 0.0 to 6.4 
# area - burned area of forest (in ha): 0.00 to 1090.84 

################################################################################################

best.lm <- lm(area~)

