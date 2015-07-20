# Marcus Rosti
# mer3ef
# 16th July 2015
# Stat 6430

## import // boilerplate
grade.data <- read.csv("samplegrades.csv", header=TRUE)

## 1
#  a

# max value on hw
top_homework <-subset(grade.data,grade.data$Homework..200.0.==max(grade.data$Homework..200.0.))

# bottom 10% 
bottom_20_homework <- subset(grade.data, grade.data$Homework..200.0.<=quantile(grade.data$Homework..200.0.,.1))

# xbar +/-  2 * s / sqrt(n)
top_upp  <- mean(top_homework$Course.Average..100.0.) + 2 * sd(top_homework$Course.Average..100.0.) / sqrt(123)
top_down <- mean(top_homework$Course.Average..100.0.) - 2 * sd(top_homework$Course.Average..100.0.) / sqrt(123)
cat("The top 20: (",top_down,", ",top_upp,") \n")
# The top 20: ( 85.95184 ,  87.97987 ) 

bot_upp  <- mean(bottom_20_homework$Course.Average..100.0.) + 2 * sd(bottom_20_homework$Course.Average..100.0.) / sqrt(57)
bot_down <- mean(bottom_20_homework$Course.Average..100.0.) - 2 * sd(bottom_20_homework$Course.Average..100.0.) / sqrt(57)
cat("The bottom 20: (",bot_down,", ",bot_upp,") \n")
# The bottom 20: ( 58.59651 ,  67.52072 ) 

# b
# subsets
close_studs = subset(grade.data, abs(grade.data$Final.Exam..100.0. - grade.data$Midterm..100.0.) <= 2)
far_studs   = subset(grade.data, abs(grade.data$Final.Exam..100.0. - grade.data$Midterm..100.0.) >= 10)

cs_upp  <- mean(close_studs$Course.Average..100.0.) + 2 * sd(close_studs$Course.Average..100.0.) / sqrt(91)
cs_down <- mean(close_studs$Course.Average..100.0.) - 2 * sd(close_studs$Course.Average..100.0.) / sqrt(91)
cat("The close 20: (",cs_down,", ",cs_upp,") \n")
# The close 20: ( 79.17462 ,  83.79727 ) 

fs_upp  <- mean(far_studs$Course.Average..100.0.) + 2 * sd(far_studs$Course.Average..100.0.) / sqrt(222)
fs_down <- mean(far_studs$Course.Average..100.0.) - 2 * sd(far_studs$Course.Average..100.0.) / sqrt(222)
cat("The far 20: (",fs_down,", ",fs_upp,") \n")
# The far 20: ( 77.16021 ,  80.23843 )

# c
letter_grades = function(data) {
  # helper function to assign grades
  data["Letter.Grade"] <- ""
  for(i in 1:nrow(data)) {
    if (data$Course.Average..100.0.[i]>=90) {
      data$Letter.Grade[i] <- "A"
    } else if (data$Course.Average..100.0.[i]>=80) {
      data$Letter.Grade[i] <- "B"
    } else if (data$Course.Average..100.0.[i]>=70) {
      data$Letter.Grade[i] <- "C"
    } else if (data$Course.Average..100.0.[i]>=60) {
      data$Letter.Grade[i] <- "D"
    } else {
      data$Letter.Grade[i] <- "F"
    }
  }
  return (data)
}

updated_grades <- grade.data
updated_grades$Course.Average..100.0. <- updated_grades$Course.Average..100.0. * .95 + 5

updated_grades <- letter_grades(updated_grades)
grade.data     <- letter_grades(grade.data)

sum(updated_grades$Letter.Grade!=grade.data$Letter.Grade)
# 57 grade changes

## 2 
my.summary = function(data) {
  return (c(min(data),median(data),max(data)))
}

## 3
# for every number either the floor or ceiling will be even and that will be the closest even number
# if the number is exactly an odd number, it returns that interger plus 1
my.even = function(val) {
  if(floor(val)  %%2==0) return (floor(val))
  if(ceiling(val)%%2==0) return (ceiling(val))
                         return (floor(val)+1)
}
#my.even(-1)
my.even(2.8)
my.even(1.3)
my.even(-5.2)

## 4

my.hist1 = function(n) {
  random_values <- runif(n,0,10)
  hist(random_values)
  return (NULL)
}
my.hist1(10)

## 5

my.hist2 = function(m,n) {
  random_values = {}
  for(i in 1:m)
    random_values[i] <- max(runif(n,0,10))
  print(length(random_values))
  hist(random_values)
  return (NULL)
}
my.hist2(10,10)
## 6

mode_check = function(vect) {
  #checks to see if mode applies
  #if there are any duplicates, return true
  return (sum(duplicated(vect))!=0)
}

Mode <- function(x) {
  # http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
  ux <- unique(x)
  return (ux[which.max(tabulate(match(x, ux)))])
}

mean_check = function(vect) {
  # checks to see if mean applies
  # if there are any points above or below, return false
  # else the property holds
  mean_vect = mean(vect)
  sd_vect = sd(vect)
  if(sum(vect>=mean_vect+3*sd_vect))  return (FALSE)
  if(sum(vect<=mean_vect-3*sd_vect))  return (FALSE)
  return (TRUE)
}

my.centers = function (vect) {
  median_vect = median(vect)
  mean_vect = NULL
  mode_vect = NULL
  
  if(mean_check(vect)) mean_vect = mean(vect)
  if(is.null(mean_vect)) cat("The mean is NULL\n")
  
  if(mode_check(vect)) mode_vect = Mode(vect)
  if(is.null(mode_vect)) cat("The mode is NULL\n")
  
  return(c(median_vect,mean_vect,mode_vect))
}

my.centers(c(round(runif(10000,0,1)))) # (1,0,0,1,1,0,1,0...)
# lots of ones and a ten
my.centers(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10)) 
my.centers(c(runif(10000,0,1)))        # (.01231,.023,.92234,.21,.543...)  
my.centers(c(rnorm(10000,0,1)))
