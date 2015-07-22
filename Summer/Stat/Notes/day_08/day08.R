# STAT 6430
# Day 8 -- R
#
# More multiple regression!
#

# Team Questions:
# 1. Upload the file "cruise01.csv" into R.  This data set contains 
#    records from 158 cruise ships, including crew size (the response
#    variable for this discussion) and six quantitative variables.
# 2. Make a scatter plot matrix of the data.
# 3. Use the methods described above to which linear linear models
#    seem to best fit this data.
# 4. Test your models with cross validation: Select 58 random records,
#    and develop each model based on this subset.  Then use your models to
#    compute the MSE's when predicting for the remaining 100 records.

cruise01 = read.csv("cruise01.csv", header = TRUE)
View(cruise01)  

plot(cruise01,pch=20,cex=.2)

library(leaps)
cruise_all<-regsubsets(crew~.,data=cruise01)
summary(cruise_all)
summary(cruise_all)$rsq    # R^2
summary(cruise_all)$adjr2  # adjusted R^2
summary(cruise_all)$cp     # Mallow's C_p (smaller is better)
summary(cruise_all)$bic    # Bayes Information Criterion (negative OK)

set<-sample(1:158,size=58)
trainer <- cruise01[set, ]
test <- cruise01[-set, ]

cruise_lm<-lm(crew~passengers+length+tonnage+cabins,data=trainer)
cruise_lm_full<-lm(crew~.,data=trainer)
preds<-predict(cruise_lm,newdata=test)


mean((data.frame(preds)$preds-test$crew)^2)

# Let's look at a new data set "insurance01.csv" that
# contains data on the time (months) until the first
# claim, the size ($) of the claim, and the type of claim.
# Note that type is a categorical variable.

ins01 = read.csv("insurance01.csv", header = TRUE)
View(ins01)  

plot(ins01,pch=20,cex=.2)

# Here's what we get from the lm function, treating "time"
# as the response variable.

ins01.lm <- lm(time~., data=ins01)
summary(ins01.lm)

# R automatically recognizes strings as categorical variables, and
# the lm function treats them as such, creating the appropriate
# dummy variables.

# Here's the insurance data again, this time with "type" coded using
# 1, 2, and 3:

ins02 = read.csv("insurance02.csv", header = TRUE)
View(ins02)  

# Here's what we get this time from the lm function:

ins02.lm <- lm(time~., data=ins02)
summary(ins02.lm)

# It's not the same because the 0-1-2 coding of "type"
# makes R think this is a quantitative variable when it is
# not.  This can be changed with the "factor" function.

ins02$type <- factor(ins02$type)
ins02.lm <- lm(time~., data=ins02)
summary(ins02.lm)
summary(ins01.lm)

# Next load the data set "cruise02.csv" which has two more
# columns of data than cruise01.

cruise02 <- read.csv("cruise02.csv",header = TRUE)
cruise2_all<-regsubsets(crew~.,data=cruise02)
summary(lm(crew~.-name,data=cruise02))

# Team Questions
# 1. Does it make sense to consider both the new variables in the
#    model?  (Based on your judgement, not computation.)
# 2. If it seems to make sense to include one or more of the new
#    variables, then try them out in a regression model.  Do you
#    end up with a result different than you got without the 
#    new data?

# Team Questions
# 1. Upload the data "hitters.csv" into R.
# 2. Develop your preferred linear model with "Salary" as the response
#    variable.


