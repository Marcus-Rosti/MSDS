setwd("~/Desktop/Lin_Reg_Test/")
datasrc <- read.csv(file = "exam.csv")

library(car)
library(leaps)

datasrc$x2 <- as.factor(datasrc$x2)
datasrc$x5 <- as.factor(datasrc$x5)

# build a full model
lin<-lm(y~ . , data=datasrc)
summary(lin)

#lots of covariance
vif(lin)


dreg<-regsubsets(y~.,data=datasrc)
sum.reg<-summary(reg)
plot(sum.reg$adjr2)
plot(sum.reg$bic)
plot(sum.reg$cp)
plot(sum.reg$rss)

master <- lm(y ~ x1+x3+x4+x6,data=datasrc)
plot(master)
summary(master)
anova(master,lin)
