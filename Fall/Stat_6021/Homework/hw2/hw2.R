# Marcus Rosti
# mer3ef
# Homework 2

setwd('/Users/RustyRosti/MSDS/Fall/Stat_6021/Homework/hw2')
library(gdata)
library(ggplot2)

printf <- function(...) cat(sprintf(...))

#2.20
fuel <- read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B18.xls")
lm.fuel <- lm(y.~X.x_6.,data=fuel)
sum<-summary(lm.fuel)
# t = beta_1 / se(beta_1)
t_val <- lm.fuel$coefficients[2]/sum$coefficients[4]
printf("Null: Beta_1 == 0")
if(qt(1-.025,15)<t_val){
  printf("\tRegect the Null\n")
  printf("\tInitial boiling point has an impact on fuel economy\n")
} else {
  printf("\tFail to regect the null\n")
  printf("\tInitial boiling point does not have an impact on fuel economy\n")
}

#2.21
# sulfer content has a negative impact on taste
wines <- read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B19.xls")
lm.wines <- lm(y~X.x_3.,data=wines)
qplot(hatvalues(lm.wines))
# point 28 has a leverage of about 1 so I removed it
wines_clean <- wines[-28,]
lm.wines <- lm(y~X.x_3.,data=wines_clean)
ggplot(wines_clean,aes(x=X.x_3.,y=y))+geom_point()+geom_smooth(method=lm)
sum<-summary(lm.wines)

t_val <- lm.wines$coefficients[2]/sum$coefficients[4]
printf("Null: Beta_1 == 0")
if(qt(.05,30)>t_val){
  printf("\tRegect the Null\n")
  printf("\tSulfar has a negative impact on taste\n")
} else {
  printf("\tFail to regect the null\n")
  printf("\tSulfar does not have negative impact on taste\n")
}

#2.22
# Oxygen to methanol controls the converstion process
methanol <- read.xls("../../linear_regression_5e_data_sets/Appendices/data-table-B20.xls")
lm.methanol <- lm(X.y~X.x_5.,data=methanol)

sum<-summary(lm.methanol)

t_val <- lm.methanol$coefficients[2]/sum$coefficients[4]
printf("Null: Beta_1 == 0")
if(qt(1-.025,17)<t_val) {
  printf("\tRegect the Null\n")
  printf("\tOxygen to methanol ratio has an impact on the conversation process\n")
} else {
  printf("\tFail to regect the null\n")
  printf("\tOxygen to methanol ratio has no impact on the conversation process\n")
}


#2.30
steam <- read.xls("../../linear_regression_5e_data_sets/Chapter 2/Problems/data-prob-2-12.XLS")
attach(steam)
# a
cor(usage,temp)
cov(usage,temp)/(sqrt(var(usage))*sqrt(var(temp)))
# b
steam.cov <- cor(usage,temp)
t_0<-steam.cov*sqrt(12-2)/sqrt(1-steam.cov^2)
if(t_0>qt(1-(.05/2),12-2)) {
  printf("\nRegect the null\n")
  printf("\tThere is not enough evidence to say rho == 0\n")
}

# c
cor.test(steam$temp,steam$usage)

# d

detach(steam)
