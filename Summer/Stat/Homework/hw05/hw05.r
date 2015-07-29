# Marcus Rosti
# mer3ef
# Stat 6430
# Homework 5

library(ggplot2)
library(leaps)
library(stringr)

mse <- function(model) {
  return (mean((model$residual)^2))
}

setwd("~/MSDS/Summer/Stat/Homework/hw05")

train <- read.csv("hw05p01train.csv")
pred <- read.csv("hw05p01predict.csv")

plot(train) 
# It looks like to me that disp, hp, wt are all highly autocorrelated
# That might just be adding a little too much noise to the model for my taste!


train_subs <- regsubsets(mpg~.,data=train)

train_subs_sum <- summary(train_subs)

summary(train_subs)

plot(train_subs_sum$adjr2)
plot(train_subs_sum$bic)
plot(train_subs_sum$rss)
plot(train_subs_sum$cp)

# seems like the model with wt and year has the best set of 

model <- lm(mpg~wt+year,data = train)

predvect <- predict(model,newdata=pred)

write.table(predvect, file = "hw05p01mypredictions.csv", row.names=F, col.names=F, sep=",")

##############################################################################################################
#
# Part 2
#
##############################################################################################################
# hmmm to many errors
comscore <- read.csv("comscore.csv")
# aha! that looks better // I removed the uses rows 
comscore2 <- read.csv("comscore2.csv")

# The sum of the top 100
sum(comscore2$Average.Daily.Visitors..000.[!(is.na(comscore2$X))])*1000

# [G] sum
sum(comscore2$Total.Unique.Visitors..000.[comscore2$X.1=="[G]"])*1000

# Canadian referencs
sum(comscore2$Total.Unique.Visitors..000.[str_detect(comscore2$X.2,"Canada")])*1000

# Find  the  sum  of  the  total  pages  viewed  (column  L)  for  all  entries  with  an  "[S]"  in  the  second
# column that have a figure reported for total pages viewed.
sum(comscore2$Total.Pages.Viewed..MM.[comscore2$X.1=="[S]"&!(is.na(comscore2$Total.Pages.Viewed..MM.))])*1000


