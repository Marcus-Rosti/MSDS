# STAT 6430
# Day 6 -- R
# 
# Today is a hodge-podge -- we'll see how far we get.
#
#
## Cross-validation
#
# The purpose of cross-validation is to assess the quality of a 
# model on data that was not used to create the model, with the
# goal of avoiding model quality being self-fulfilling.

## Team Questions
# 1. From the data set d06c, select a random subset of 100 records
#    and name the new data frame "d06c.train".  Name the remaining
#    records "d06c.test".
# 2. Determine which model seems to best fit the d06c.train data.
# 3. Use the model to predict the y-values corresponding to each of 
#    the x-values in the set d06c.test.
# 4. Determine the MSE for d06c.test.

# The next data set is set up in a manner similar to future homework
# exercises.  There are two sets of data: "d06d-train.csv" which contains
# x and y-values to uses to develop a model, and "d06d-predict.csv" which
# contains only x-values.  Once you have a model developed, you will use
# it to predict the y-values corresponding to the x-values in the 
# "predict" file.

## Team questions:
# 1. Upload "d06d-train.csv" into R.
# 2. Develop a model for predicting y from x.  Cross-validate!
# 3. Upload "d06d-predict.csv" into R, and use your model to
#    predict the corresponding y-values.
# 4. Upload "d06d-correct.csv" into R.  This file contains the
#    x-values from "d06d-predict.csv" along with the correct
#    y-values.  Use these y-values to compute the MSE for 
#    your predicted values.

training_set <- read.csv("d06d-train.csv")

ggplot(data=training_set,aes(x=x,y=y))+geom_point()
ggplot(data=training_set,aes(x=x^2,y=y))+geom_point()

lin_model  <- lm(y~x,data = training_set)
quad_model <- lm(y~poly(x,2),data=training_set)
cubic_model <- lm(y ~ poly(x,3),data = training_set)

summary(lin_model)
summary(quad_model)
summary(cubic_model)

mean(lin_model$residuals^2)
mean(quad_model$residuals^2)
mean(cubic_model$residuals^2)

#Cross-validation
test_set <- read.csv("d06d-predict.csv") 
test_set["y"] <- predict(quad_model,newdata=test_set)

correct_set <- read.csv("d06d-correct.csv")

ggplot(data=test_set,aes(x=x,y=y)) + geom_point()
ggplot(data=correct_set,aes(x=x,y=y)) + geom_point()

sum((test_set$y - correct_set$y)^2)
ggplot(data=lin_model,aes(x=lin_model$fitted.values,y=lin_model$residuals))+geom_point()+geom_smooth(method=lm)
ggplot(data=quad_model,aes(x=quad_model$fitted.values,y=quad_model$residuals))+geom_point()+geom_smooth(method=lm)

#
## Introduction to vectorization
#
# Below is an example of the typical inefficient code that I like 
# to write.
#
jeff.sum.of.squares <- function(n) {
  ss <- 0
  for (j in 1:n) {
    ss <- ss + j^2
  }
  return(ss)
}
jeff.sum.of.squares(100)
#
# Time required to sum the first 10^7 terms.
system.time(jeff.sum.of.squares(10^7))

# Here is a function that does the same thing, but defined in a manner 
# that exploits that faster built-in R functions. 
fast.sum.of.squares <- function(n) {
  ss <- sum((1:n)^2)
  return(ss)
}
#
# Time required to sum the first 10^7 terms.
system.time(fast.sum.of.squares(10^7))

# 
# Another slow function
#
slow.geom.sum <- function(x,n) {
  geom.sum <- 0
  for (j in 1:n) {
    geom.sum <- geom.sum + x^j
  }
  return(geom.sum)
}

fast.geom.sum <- function(x,n) {
  if(0<x&x<1) return(1/x)
  return(sum(x^(0:n)))
}

system.time(slow.geom.sum(2.5,10^7))
system.time(fast.geom.sum(2.5,10^7))
system.time(slow.geom.sum(.5,10^7))
system.time(fast.geom.sum(.5,10^7))


#
# Team Questions
# 1. Write a function fast.geom.sum that vectorizes the above code.
# 2. Use the system.time function to determine how much faster your
#    function is (elapsed time) when compared to slow.geom.sum when
#    applied to x = 0.8 and n = 10^7.

#
# Team Questions
# 1. Write a function that will take an integer n as input.
#    The function should generate n random points in the square
#    [-1,1]x[-1,1], and return the proportion of those points
#    that land in the unit circle x^2 + y^2 <= 1.
# 2. Evaluate your function for n = 50,000.
# 3. What is the probability that one of your random points
#    lands in the unit square?
# 4. Use the answers to 2. and 3. to estimate the value of pi.

## Reading data and string manipulation
#
# The notes below cover reading and writing to files,
# and string manipulation
#
# readLines reads in a file, one line at a time, regardless
# of format.
purchase.lines <- readLines("purchases.txt")
purchase.lines[1]
purchase.lines[2]
purchase.lines
#
# strsplit will split strings, returning a list
#
strsplit(purchase.lines,";")
#
# If a vector is more convenient, use unlist
#
dat <- unlist(strsplit(purchase.lines,";"))
dat
dat[2]
#
# dat[2] is a single string. We can convert it to 
# a vector of strings.
#
unlist(strsplit(dat[2],","))
#
# We get a vector of numbers with as.numeric
#
my.vec <- as.numeric(unlist(strsplit(dat[2],",")))
my.vec
sum(my.vec)
#
# Strings can be split on various characters
strsplit("This is a typo"," ")
strsplit("This is a typo","")
#
#
# String manipulations
#
my.string<-"This is a sample string."
#
# We can change cases
#
tolower(my.string)
toupper(my.string)
#
# It's also possible to manipulate strings in various ways.
# The package "stringr" has many handy functions.
# Note: You might have to download stringr before installing.
install.packages("stringr")
library(stringr)
#
str_detect(my.string, "a")  # Determine if "a" is in my.string
str_count(my.string, "a")  # Number of times "a" is in my.string
str_locate(my.string, "a") # Location of first "a"
str_locate_all(my.string, "a") # Location of all "a"s
str_pad(my.string, 30, side="right") # Pad out to 30 chars.
#
# We can replace portions of strings
str_replace(my.string, "s", "XX") # Just the first "s"
str_replace_all(my.string, "s", "XX")
# We can replace multiple characters
# This example replaces all vowels with "XX"
#
str_replace_all(my.string, "[aeiou]", "XX")
#
# We can extract substrings in specified positions
#
str_sub(my.string,7,12)
str_sub(my.string, end=8)
str_sub(my.string, start=10)
str_sub(my.string, -5)  # The last 5 characters
#
# See the documentation for "stringr" for more info.
#
# Writing to files
#
# The function "write" can be used to write text to a file.
#
purchase.lines[6]  # Purchaser's name in all caps
new.line <- str_replace(purchase.lines[6],"SMITH", "Smith")
new.line  # Just to see the new string
write(new.line, "my.output.txt", append=TRUE) # Write to file
