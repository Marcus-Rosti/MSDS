# Marcus Rosti
# mer3ef
# Homework 4
# Stat 6430

library(ggplot2)

# Set working directory
setwd("~/MSDS/Summer/Stat/Homework/hw04")
#read in both data sets
train<-read.csv("hw04p01train.csv")
test<-read.csv("hw04p01predict.csv")


#plot the data to explore 
ggplot(train,aes(x=disp,y=wt))+geom_point()
ggplot(train,aes(x=disp,y=wt))+geom_point()+geom_smooth(method=lm)
ggplot(train,aes(x=sqrt(disp),y=wt))+geom_point()+geom_smooth(method=lm)

# it looks like the sqrt of x is the better transformation
# there appears to be less trend in the risidual plot
# Also, computing is cheap, let's do both and compare along the way
model_simple <- lm(wt~disp,data=train)
model_sqrt   <- lm(wt~sqrt(disp),data=train)

# They both look pretty strong, the r^2 is better for the sqrt model though
summary(model_simple)
summary(model_sqrt)

# The mse for sqrt is definitely better
mean(model_simple$residuals^2)
mean(model_sqrt$residuals^2)

# predict the two 
preds_simple <- predict(model_simple, newdata = data.frame(disp = sqrt(test$disp)))
preds_sqrt   <- predict(model_sqrt, newdata = data.frame(disp = sqrt(test$disp)))

# Write to file!
write.table(preds_sqrt, file = "hw04p01mypredictions.csv", row.names=F, col.names=F, sep=",")
 
