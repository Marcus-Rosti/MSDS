# Marcus Rosti
# mer3ef
# 13 July 2015
# Stat 6430

# Set working directory
setwd("~/MSDS/Summer/Stat/Homework")

# Import grade data from samplegrades.csv
grade.data <- read.csv("samplegrades.csv", header=TRUE)
summary(grade.data) # Just looking at the summary 

### 1 ###
## a
# count the number of students who failed the class
length(grade.data$Student.ID[grade.data$Course.Average..100.0.<60])/5.66
# ~4.24

## b
sum(grade.data$Final.Exam..100.0.==0)
# 5

## c
final_mean <- mean(grade.data$Final.Exam..100.0.)
quiz_mean  <- mean(grade.data$Quiz..45.0.)/45*100
mid_mean   <- mean(grade.data$Midterm..100.0.)
max_mean   <- max(final_mean,quiz_mean,mid_mean)

# Some logic to print out the right answer
if (final_mean==max_mean) {
    print("The final was highest")
} else if (quiz_mean==max_mean) {
    print("The quizes were highest")
} else {
    print("The midterm was highest")
}
# Quizes had the highest

## d
# The number of people that had trouble on quizes but got at least a B on the midterm
sum(grade.data$Midterm..100.0.>=80)
#232
sum(grade.data$Quiz..45.0.<=70*45/70)
#566
sum(grade.data$Quiz..45.0.<=(70*45/100) & grade.data$Midterm..100.0.>=80)
# 8 people

## e
# The number of people who got a lower course grade than their final exam
sum(grade.data$Course.Average..100.0. < grade.data$Final.Exam..100.0.) / 5.66
# ~8.12

## f
# Make subsets of the data
top_20 <- subset(grade.data, grade.data$Course.Average..100.0>quantile(grade.data$Course.Average..100.0.,.8))
bottom_20 <- subset(grade.data, grade.data$Course.Average..100.0.< quantile(grade.data$Course.Average..100.0.,.2))

# for the top 20
sum(top_20$Course.Average..100.0. < top_20$Final.Exam..100.0.) / 1.13
# 9.73

# for the bottome 20
sum(bottom_20$Course.Average..100.0. < bottom_20$Final.Exam..100.0.) / 1.13
# 13.2

# g
# #of students with quiz grades between 70 and 80
sum(grade.data$Quiz..45.0.>70*45/100 & grade.data$Quiz..45.0.<80*45/100)

# #of students with homework grades between 90  and 95
sum(grade.data$Homework..200.0./2>90 & grade.data$Homework..200.0./2<95)

# #in both groups
both_groups <- sum( (grade.data$Quiz..45.0.>70*45/100 & grade.data$Quiz..45.0.<80*45/100) | (grade.data$Homework..200.0./2>90 & grade.data$Homework..200.0./2<95) )
print("# of students with a C on the quiz a low A on the homework: ", both_groups)
#111

### 2 ###
print("Updating course letter grade...")
grade.data["Letter.Grade"]<-""

# Iterate over each row and assign a grade to each student
for(i in 1:nrow(grade.data)) {
  if (grade.data$Course.Average..100.0.[i]>=90) {
    grade.data$Letter.Grade[i] <- "A" 
  } else if (grade.data$Course.Average..100.0.[i]>=80) {
    grade.data$Letter.Grade[i] <- "B" 
  } else if (grade.data$Course.Average..100.0.[i]>=70) { 
    grade.data$Letter.Grade[i] <- "C"
  } else if (grade.data$Course.Average..100.0.[i]>=60) { 
    grade.data$Letter.Grade[i] <- "D"
  } else { 
    grade.data$Letter.Grade[i] <- "F"  
  }
}

table(grade.data$Letter.Grade)
print("Finished updating course letter grade")
### 3 ###
print("Writing grades to file")
write.csv(grade.data,file = "mygradefile.csv")
print("Homework 1 complete")
