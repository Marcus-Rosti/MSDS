# STAT 6430
# Day 9 -- R
#
## Logistic regression
#
setwd("~/MSDS/Summer/Stat/Notes/day_09")

# Let's start with some data on success at a programming task.
# x = months of professional experience, y = 1 if the individual was
# successful in completing task.

prog = read.csv("programming.csv", header = TRUE)

library(ggplot2)
ggplot(data=prog,aes(x=months,y=success))+geom_point()+stat_smooth(method="glm", family="binomial", se=F)

# "glm" can be used for various types of models, including
# linear models covered by lm.  Here we request logistic
# regression with the option family = "binomial".

prog.lg <- glm(success ~ months, data = prog, family = "binomial")
summary(prog.lg)



# The "predict" function can be used to predict probabilities
# of success for input values.

fit1<-predict(prog.lg, type="response") # Predicted probs for orig. data
fit1

# Here's a plot of the probabilities against the number of 
# months of experience

plot(as.vector(fit1)~prog$months,pch=20,cex=.2)

# We can convert the probabilities into predictions of success
# or failure for each test-taker by setting success=1 if the
# prediction probability >= 0.5

preds <- rep(0,25)  # Initialize all to success=0 (pessimistic)
preds[fit1 >= 0.5] <- 1 # Change those with prob >= .5 to 1
preds
prog$success

# Here's a table comparing actual success to predicted:

table(preds,prog$success)
19/25 # Correct prediction rate, a typical measure of prediction success

# Let's look at another example, this time involving health data
# related to mosquito-borne illness:

mos = read.csv("mosquito.csv", header = TRUE)
# age = age of person
# soc1 = 1 if middle class
# soc2 = 1 if lower class
# soc1 = soc2 = 0 if upper class; no need to declare factor
# sect = sector in town (of two)
# disease = patient disease status

mos.lg1 <- glm(disease~., data = mos, family = "binomial")
summary(mos.lg1)

# Logistic regression does not have as many diagnostic tools as
# linear regression, but we can use the p-values for some guidance.
# Let's try a reduced model that leaves out class:

mos.lg2 <- glm(disease~age+sect, data = mos, family = "binomial")
summary(mos.lg2)

# The remaining variables appear to be significant.
# Let's compare the correct prediction rate for each model, to
# see if there is any difference:

probs1<-predict(mos.lg1, type="response") 
preds <- rep(0,98)  # Initialize all to disease=0 
preds[probs1 >= 0.5] <- 1 # Change those with prob >= .5 to 1
table(preds,mos$disease)
(58+12)/98

probs2<-predict(mos.lg2, type="response") 
preds <- rep(0,98)  # Initialize all to disease=0 
preds[probs2 >= 0.5] <- 1 # Change those with prob >= .5 to 1
table(preds,mos$disease)
(59+10)/98

# The reduced model is slightly less successful, but since we're
# testing the predictions with the data used to create the model,
# maybe we have some circular reasoning.  We can try some cross
# validation.

# Before moving on to the next Team Questions, we pause for some
# sample code to predict against specified data instead of the
# exact set used to create the model.

s <- sample(1:98, 20, replace=FALSE)
mos.sub <- mos[s,]
probs.sub <- predict(mos.lg1, newdata=mos.sub, type="response")
preds <- rep(0,20)  # Initialize all to disease=0 
preds[probs.sub >= 0.5] <- 1 # Change those with prob >= .5 to 1
table(preds,mos$disease[s])

## Team Questions
# 1. Select a random sample of 49 records from the "mosquito" data,
#    and create a full model (all variables) based on this sample.
# 2. Cross validate: Use your model to predict the disease status 
#    of the 49 records that were not included in your sample, then 
#    create a table summarizing the comparison with the actual
#    disease status, along with a calculation of the "correct" rate.
# 3. Repeat 1 & 2 using the reduced model that omits the "class" 
#    categorical variable.

s <- sample(1:98,49,replace=F)
mos.sub <- mos[s,]
mos.lg.sub <- glm(disease~., data = mos.sub, family = "binomial")
summary(mos.lg.sub)
probs.sub <- predict(mos.lg.sub, newdata=mos.sub, type="response")
preds <- rep(0,49)  # Initialize all to disease=0 
preds[probs.sub >= 0.5] <- 1 # Change those with prob >= .5 to 1
table(preds,mos$disease[s])
(30+4)/49
# Next let's consider a new training data set "heart-train.csv".  The
# reponse variable is "chd" which indicates if coronary heart disease
# is present.  There are 8 explanatory variables, all quantitative.

heart_train = read.csv("heart-train.csv", header = TRUE)

## Team questions
# 1. Use cross validation to help select a logistic regression model
#    for predicting the status of chd.  Determine the rate of 
#    correct prediction for your held out data.
# 2. Once you have decided on your model, determine how well it
#    does in predicting the status of chd for the data in 
#    "heart-test.csv".

heart_glm <- glm(data=heart_train,chd~.,family="binomial")
summary(heart_glm)
plot(heart_glm)
anova(heart_glm)

heart_glm_reduced <- glm(data=heart_train,chd~.-adiposity-obesity-alcohol,family="binomial")
summary(heart_glm_reduced)
plot(heart_glm_reduced)
install.packages("bestglm")
library(bestglm)
library(leaps)
reg_sub<-bestglm(Xy=heart_train,chd~.,IC = "BIC",family=binomial)
summary(reg_sub)
reg_sub$BestModels
# If time permits:

## Team Questions
# 1. Upload the data "hitters.csv" into R.
# 2. Develop your preferred linear model with "Salary" as the response
#    variable.


