# Marcus Rosti
# mer3ef
# 13 July 2015
# Stat 6430

# Set working directory
setwd("~/MSDS/Summer/Stat/Homework")

# Import grade data from samplegrades.csv
grade.data <- read.csv("samplegrades.csv", header=TRUE)
summary(grade.data) # Just looking at the summary 

## a
# count the number of students who failed the class
length(grade.data$Student.ID[grade.data$Course.Average..100.0.<60])
# 24

## b
sum(grade.data$Final.Exam..100.0.==0)
# 5

## c
final_mean <- mean(grade.data$Final.Exam..100.0.)
quiz_mean  <- mean(grade.data$Quiz..45.0.)/45*100
mid_mean   <- mean(grade.data$Midterm..100.0.)
max_mean   <- max(final_mean,quiz_mean,mid_mean)
if (final_mean==max_mean) {
  print("The final was highest")
} else if (quiz_mean==max_mean) {
  print("The quizes were highest")
} else {
  print("The midterm was highest")
}
# Quizes had the highest

## d
sum(grade.data$Midterm..100.0.>=80)
#232
sum(grade.data$Quiz..45.0.<=70*45/70)
#566
sum(grade.data$Quiz..45.0.<=(70*45/100) & grade.data$Midterm..100.0.>=80)
#8
 