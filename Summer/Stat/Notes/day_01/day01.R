# STAT 6430
# Day 1 -- R
# 
# Today we start with a short tutorial on R that is also in R.
#

# Basic Operations (# = comment)
#

3+5
4^3
c(2,3,4,5)    # C stands for "combine" or "concatenate." 
c(2,3,4,5) + c(1,4,2,3)
c(2,3,4)*2

c(2,3,4)^3
c(3,4,5) + 2
c(2,3,4,5) + c(5,10)
c(2,3,4,5,6) + c(1,2)
c(1,2,5)^c(2,2,3)
c(1,2,5)^c(2,3)

#
# Sequences
#

1:10
3:15
12:2
seq(1,11,by=2)
seq(4,30, by=3)
rep(7,20)

#
# Variables
#

x <- 7   # You can also use "x = 7" if you prefer
y <- -2
z <- c(x,8)   # z is a vector
z

#
# Changing x now does not change z
#

x <- 5
z

#
# Extracting values from vectors
#

v <- 10:20
v
v[2]
v[4:7]
v[-5]
v[9:7]
v[c(2,6,8)]
v[(2,6,8)]

(1:11) %% 2
1:11 %% 2
(1:11) %% 2 == 1  # True when element odd
v[(1:11) %% 2 == 1]   # Extracts every other entry of v
v[v %% 2 == 1]   # Extracts the odd entries of v
(1:11)[v %% 2 == 1]  # Positions of odd entries of v

# Matrices and Arrays
#
m <- matrix(1:12, nrow=3, ncol=4)  # Defines a 3-by-4 matrix
m
m <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
m

m[2,3]   # Entry in row 2, column 3
m[1,]   # Row 1
m[,3]   # Column 3
m[1:2,3:4]   # Entries in rows 1 or 2 AND cols 3 or 4
m[,2:4]   # Columns 2-4
m[7]  # Gives 7th entry when counting by columns 
# Matrices are actually stored as vectors in R

a <- array(1:24, dim=c(2,4,3))   # Define a 2-by-4-by-3 array
a
a[1,2,3]
a[1,2,]
a[,,3]

#
# Lists
#
# A list can contain different data types, such as strings and numbers.
# The components can be named, so that items can be referenced by either
# name or location

my.list <- list(school="UVA", program="MSDS", year=2015)
my.list
my.list$program
my.list[1]
my.list[[3]]

#
# Data Frames
#
# A data frame is a special type of list made up of vectors all of the
# same length.

courses <- c("STAT6021", "CS5012", "DS6001", "STAT5170","MATH5100")
daysofweek <- c("MWF", "MW", "TuTh", "MWF", "MWF")
starttime <- c(1000, 1400, 1530, 1200, 1300)
endtime <- c(1050, 1515, 1645, 1250, 1350)

classes <- data.frame(courses,daysofweek,starttime,endtime)
classes
classes$starttime
classes$endtime[3]
classes$starttime > 1000
classes$courses[classes$starttime > 1200]   # No morning classes!
classes$courses[classes$starttime >= 1300 & classes$daysofweek=="TuTh"]

# Introduction to vectorization
#
# Below is an example of the typical inefficient code that I like to write.
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
slow.geom.sum(3,10)
slow.geom.sum(.5,20)

#
# Quick introduction to If/Then statements
#

small.number <- function(x) {
  if (x <= 100) {
    return("small")
  } 
}
small.number(45.8)
small.number(108)

small.or.big.number <- function(x) {
  if (x <= 100) {
    return("small")
  } else {
    return("big")
  }
}
small.or.big.number(45.8)
small.or.big.number(108)

#
# Read in a text file "day01-1.txt"
# This file contains over 100,000 measurements
# (in seconds) for the times required to complete
# a purchase at a certain fast food chain.  Each 
# record (line) includes the transaction time, the
# store number, and the day of the week.
#

d1 <- read.table("fastfood.txt",sep=",",header=T)
typeof(d1) # A data frame is a list in R
names(d1)  # Column names for data

d1$secs[1:20]
length(d1$dayofweek)
max(d1$storenum)

#
# Exercises
#
# Each team should have answers for all 13 questions.  
#
# 1. Find the minimum time recorded for a transaction.
#
min(d1$secs)
30
# 2. How many transactions took the maximum recorded time?
#
length(d1$secs[d1$secs==max(d1$secs)])
23
# 3. How many transactions took place at store number 500?
#
length(d1$storenum[d1$storenum==500])
114
# 4. How many transactions took place on Tuesday?
#
length(d1$dayofweek[d1$dayofweek=="Tues"])
20194
# 5. How many transactions required less than 3 minutes?
#
length(d1$secs[d1$secs<180])
56337
# 6. How many transactions were recorded at store 711 on Friday?
#
sum(d1$storenum==117 & d1$dayofweek=="Fri")
length(d1$storenum[d1$storenum==117 & d1$dayofweek=="Fri"])
22
# 7. How many transactions took more than 4 minutes at store
#    117 on Thursday?
#
length(d1$secs[d1$storenum==117 & d1$dayofweek=="Thur" & d1$secs>240])
7
# 8. Which day of the week had the most transactions?
#
summary(d1$dayofweek)
"Tuesday"
# 9. What was the maximum number of transactions at a single 
#    store?  
# 
table(d1$storenum)
max(table(d1$storenum))
147
# 10. How many stores had the minimum number of recorded 
#     transactions?
#
min(table(d1$storenum))

# ??? d1$storenum[table(d1$storenum). == (min(table(d1$storenum)))]
1
# 11. What transaction time is the 50th percentile?
#
summary(d1$secs)
quantile(d1$secs,.5)
164
# 12. What percentile is a transaction of 8 minutes?
#
ecdf(d1$secs)(480)
###
above <- length(d1$secs[d1$secs >=480])
quant <- (1-above/length(d1$secs))*100
quant
~92
# 13. Select a random sample of 20 transaction times, and
#     compute the median of your sample.
#
median(sample_n(d1,20)$secs)
