## Team Assignment 5
#
#  Please submit one set of answers per team. All answers can be submitted
#  in an annotated R file.

mse <- function(sm) {
  return(mean(sm$residuals ^ 2))
}

mystandard <- function(sm) {
  return(sm$residuals) / sqrt(mse(sm))
}

mystudent <- function(sm) {
  hats <- hatvalues(sm)
  return((sm$residuals) / (sqrt(mse(sm) * (1 - hats))))
}

myPRESS <- function(sm) {
  hats <- hatvalues(sm)
  return(sum((sm$residuals / (1 - hats)) ^ 2))
}

myRStud <- function(sm) {
  n <- length(sm$residuals)
  hats <- hatvalues(sm)
  s2 <- ((n - sm$rank - 1) * mse(sm) - sm$residuals/(1 - hats)) / (n - sm$rank)
  return(sm$residuals / sqrt(s2 * (1 - hats)))
}
# Question 1: For each part of this problem, you are to create (or find) a
##  data set suitable for simple linear regression (with about 30 observations)
##  that satisfies the given specifications.  If it is not possible, explain
##  why it isn't possible.
##  For this question, in addition to the annotated R file please provide a
##  PDF document that includes plots of your data that show the interesting
##  points.
##
##  (a) The data set has a point that is clearly visible for all four types of
##      residuals discussed -- standardized, studentized, PRESS, R-student.
set.seed(1)
x <- rnorm(30)
x[30] <- 6
y <- 5 * x + 2 + rnorm(30,0,1)
y[30] <- 0
plot(y ~ x)
model <- lm(y ~ x)

plot(mystandard(model))
plot(mystudent(model))
myPRESS(model)
plot(myRStud(model))

##  (b) The data set has a point that stands out when viewing studentized
##      residuals but not when viewing standardized residuals.
set.seed(1)
x <- rnorm(30)
x[30] <- 10
y <- 5 * x + 2 + rnorm(30,0,1)
y[30] <- 10
plot(y ~ x)
model <- lm(y ~ x)

plot(mystandard(model))
plot(mystudent(model))

##  (c) The data set has a point that stands out when viewing PRESS
##      residuals but not when viewing standardized residuals.
set.seed(1)
x <- rnorm(30)
x[30] <- 20
y <- 5 * x + 2 + rnorm(30,0,1)
y[30] <- 0
plot(y ~ x)
model <- lm(y ~ x)

plot(mystandard(model))
myPRESS(model)

##  (d) The data set has a point that stands out when viewing R-student
##      residuals but not when viewing standardized residuals.
set.seed(1)
x <- rnorm(30)
x[30] <- 10
y <- 5 * x + 2 + rnorm(30,0,1)
y[30] <- 10
plot(y ~ x)
model <- lm(y ~ x)

plot(mystandard(model))
plot(myRStud(model))


##  (e) The data set has a point that stands out when viewing PRESS
##      residuals but not when viewing studentized residuals.
set.seed(1)
x <- rnorm(30)
x[30] <- 6
y <- 5 * x + 2 + rnorm(30,0,1)
plot(y ~ x)
model <- lm(y ~ x)

plot(mystudent(model))
myPRESS(model)

##  (f) The data set has a point that stands out when viewing R-student
##      residuals but not when viewing PRESS residuals.
set.seed(1)
x <- rnorm(30)
y <- 5 * x + 2 + rnorm(30,0,1)
y[30] <- -10
plot(y ~ x)
model <- lm(y ~ x)

myPRESS(model)
plot(myRStud(model))

# Question 2: For each part of this problem, you are to create (or find) a
##  data set suitable for simple linear regression (with about 30 observations)
##  that satisfies the given specifications on the variance of the residuals
##  For this question, in addition to the annotated R file please provide
##  plots of your residuals that show the required characteristic.  (Put
##  the plots in the PDF used for Question 1.)
##
##  (a) The residuals have constant variance.
set.seed(1)
x <- (1:300)/300
y <- x + rnorm(300,0,1)
model <- lm(y ~ x)
plot(model$residuals ~ x)
##  (b) The residuals have variance proportional to E(y).
set.seed(1)
x <- (1:300)/300
y <- x
y <- y + rnorm(300,0,y)
model <- lm(y ~ x)
plot(model$residuals ~ x)
##  (c) The residuals have variance proportional to E(y)^2.
set.seed(1)
x <- (1:300)/300
y <- x
y <- y + rnorm(300,0,y^2)
model <- lm(y ~ x)
plot(model$residuals ~ x)
##  (d) The residuals have variance proportional to 1/E(y).
set.seed(1)
x <- (1:300)/300
y <- x
y <- y + rnorm(300,0,1/y)
model <- lm(y ~ x)
plot(model$residuals ~ x)
##  (e) The residuals have variance proportional to C-E(y) for some constant C.
set.seed(1)
x <- (1:300)/300
y <- x
y <- y + rnorm(300,0,10-y)
model <- lm(y ~ x)
plot(model$residuals ~ x)
##  (f) The residuals have variance proportional to E(y)(C-E(y)) for some constant C.
set.seed(1)
x <- (1:300)/300
y <- x
y <- y + rnorm(300,0,y*(10-y))
model <- lm(y ~ x)
plot(model$residuals ~ x)
