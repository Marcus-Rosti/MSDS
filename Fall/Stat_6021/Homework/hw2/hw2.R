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
t_val <- lm.fuel$coefficients[2]/sum$coefficients[4] # 2.72
printf("Null: Beta_1 == 0")
# t_test = 2.13145 < 2.72 = t_0
if(qt(1-.025,15)<t_val){
  # this is the correct answer
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
# t_test = -2.505383 > -2.042272 = t_0
if(qt(.025,30)>t_val){
  # this is the correct answer
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
# t_test = 2.109816 (NOT less than!!) 0.4648991 = t_0
if(qt(1-.025,17)<t_val) {
  printf("\tRegect the Null\n")
  printf("\tOxygen to methanol ratio has an impact on the conversation process\n")
} else {
  # this is the correct answer
  printf("\tFail to regect the null\n")
  printf("\tOxygen to methanol ratio has no impact on the conversation process\n")
}


#2.30
steam <- read.xls("../../linear_regression_5e_data_sets/Chapter 2/Problems/data-prob-2-12.XLS")
attach(steam)
# a
cor(usage,temp)
cov(usage,temp)/(sqrt(var(usage))*sqrt(var(temp)))
# they're both 0.9999326
# b
steam.cov <- cor(usage,temp)
t_0<-steam.cov*sqrt(12-2)/sqrt(1-steam.cov^2)
if(t_0>qt(1-(.05/2),12-2)) {
  # I didn't put the other one because this is the correct answer
  # Sorry for the laziness
  printf("\nRegect the null\n")
  printf("\tThere is not enough evidence to say rho == 0\n")
}

# c
r <- cor(usage,temp)
mu_z<-1/2*log((1+r)/(1-r)) # 5.14
sigma_2_z<-(12-3)^(1/2) # 3
 
test <- 1/2 * log((1+.5)/(1-.5)) # 0.5493
z_0 <- (mu_z-test)*sigma_2_z # 13.79796 ~ This is very large

if(z_0>1.96) {
  # the correct answer
  printf("\nRegect the null\n")
  printf("\tThere is not enough evidence to say rho == .5\n")
} else {
  printf("\nFail toegect the null\n")
  printf("\tThere is enough evidence to say rho == .5\n")
}

# d
# basically the same procedure as above except we the scale by tanh
r<- cor(usage,temp)
sigma<-sqrt(9)^(-1)
lower<-tanh(1/2*log((1+r)/(1-r))-2.56*sigma)
upper<-tanh(1/2*log((1+r)/(1-r))+2.56*sigma)
printf("\n(%1.7f,%1.7f)\n",lower,upper)
# (0.9996284,0.9999878)


detach(steam)
