

#*****************************************************************************
#
#					Data Wrangling - Train Accidents
#
#*****************************************************************************

setwd("~/MSDS/Fall/DS_6001/inclass2")

#************************************
# Reading in data
#************************************

# Read all the data into one data frame
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

data_files <- list.files("data")
datasets = rep(data.frame(),14)
datasets<-sapply(sapply(data_files,function(x) paste("data/",x,sep="")), read.csv)
all_sets <- Reduce(function(x,y) rbind(x,y),datasets[2:14],datasets[1])

data_2001 <- read.csv("RailAccidents01.txt") 
# Your combination should be an inner join
# or an intersection of column names (variables)
# Set working directory

# Hints: use functions like sapply() and rbind()


# For the work in the class we will use 2001 data


# Set working directory


# Read in 2001 data



# To get a summary of all of the variables
summary(data_2001[,c("ACCDMG","TRNSPD","TONS","TOTKLD","TOTINJ")])


# How many accident reports and variables?


##################################################
#
#	Data Cleaning
#
##################################################

#*************************************************
#		Duplicates
#*************************************************

# Are there other duplicates?
sum(duplicated(data_2001$INCDTNO))


# Remove duplicates
data_2001[!duplicated(data_2001$INCDTNO),]

nodup_data_2001 <- data_2001[!duplicated(data_2001[,c("YEAR","DAY","MONTH","TIMEHR","TIMEMIN")]),]
#*************************************************
#		Exteme Point
#*************************************************



# Look at the most costly accident in the data set

boxplot(nodup_data_2001$ACCDMG)

# Check out the narratives for this extreme accident

nodup_data_2001[which.max(nodup_data_2001$ACCDMG),c("ACCDMG","YEAR","MONTH","DAY","NARR1")]
# it's the 9/11 attack

# what should we do?
no911<-nodup_data_2001[-which.max(nodup_data_2001$ACCDMG),]


#********************************************
# Missing data
#********************************************

# Are we missing values for any variables?
# yes!

# How many?

nafind <- function(x) { sum(is.na(x))}

nacount<-apply(no911,2,"nafind")

sum(apply(no911,2,"nafind")>0)

# Do we need all the variables?

# defintely not

# Remove unnecessary variables, then get a summary
# Keep TYPEQ, we'll use it. 

varWna <- which(nacount>0)

varWna<-varWna[-which(colnames(no911)[varWna] == "TYPEQ")]

clean_2001 <- no911[,-varWna]

#*************************************************
#
#   Reformating variables
#
#*************************************************


# Type of accident
# put in the values from the variable definitions

summary(clean_2001$TYPE)
#clean_2001< factor(clean_2001$TYPE,labels = c("Derailment","HeadsOn","Rearmed","Side","Raking","BrokenTrain","Hwy-Rail",
#                                              "GradeX","Obstruction","Explosive","Fire","Other","SeeNarrative"))
clean_2001$TYPE <- factor(clean_2001$TYPE,labels = c("Derailment","HeadsOn","Rearmed","Side","Raking","BrokenTrain","Hwy-Rail",
                                              "Obstruction","Explosive","Fire","Other","SeeNarrative"))
summary(clean_2001$TYPE)
# Type of Train
# put in the values from the definitions

clean_2001$TYPEQ <- factor(clean_2001$TYPEQ,labels = c("Freight","Passenger","Commuter","Work","Single","CutofCars","Yard","Light","Maint"))
summary(clean_2001$TYPEQ)

# Clean up the values



# New labels



# Don't do it, but think about
# how you can replace the missing values



# Create a new variable called Cause
# that uses the first character in the string labels for cause.

clean_2001$Cause <- rep(NA,nrow(clean_2001))

clean_2001$Cause[which(substr(clean_2001$CAUSE,1,1)=="M")] <-"M"
clean_2001$Cause[which(substr(clean_2001$CAUSE,1,1)=="T")] <-"T"
clean_2001$Cause[which(substr(clean_2001$CAUSE,1,1)=="S")] <-"S"
clean_2001$Cause[which(substr(clean_2001$CAUSE,1,1)=="H")] <-"H"
clean_2001$Cause[which(substr(clean_2001$CAUSE,1,1)=="E")] <-"E"

clean_2001$Cause <- factor(clean_2001$Cause)

summary(clean_2001$Cause)

#*************************************************
#
#   Reformating narratives
#
#*************************************************

# Look at the narratives
# Combine them into one vector
# where each entry is the complete narrative
# for that accident

clean_2001[1,c("NARR1","NARR2","NARR3","NARR4")]

clean_2001$NARR <- rep(NA,nrow(clean_2001))










