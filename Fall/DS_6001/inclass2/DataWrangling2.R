

# Read the accident data from files in a directory
# The default is csv


#******************************
# Replace blank entries with NA
#******************************

#************************************
#
# Data Cleaning Function
#
#************************************

# Get the clean data set
# that removes duplicates
# removes the accident associated with the 9/11/01
# attack
# Removes rows with NA's but keeps TYPEQ
# Cleans TYPE, TYPEQ, and CAUSE
# Takes a data frame as input

setwd("~/MSDS/Fall/DS_6001/inclass2")

#************************************
# Reading in data
#************************************


# Read all the data into one data frame
trim <- function(x)
  gsub("^\\s+|\\s+$", "", x)


# I put all my files into a sub directory so I hd to parse them correctly
data_files <- list.files("data")
datasets = rep("",14)
datasets <- sapply(data_files,function(x)
  paste("data/",x,sep = ""))
all_trains <- sapply(datasets, function(x)
  read.csv(x))
other_trains <- all_trains[-1]


all_sets <-
  Reduce(function(x,y)
    merge(x ,y ,all = TRUE), other_trains, all_trains[[1]])


# To get a summary of all of the variables
summary(all_sets[,c("ACCDMG","TRNSPD","TONS","TOTKLD","TOTINJ")])


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
sum(duplicated(all_sets$INCDTNO))

# Remove duplicates
all_sets[!duplicated(all_sets$INCDTNO),]

nodup_all_sets <-
  all_sets[!duplicated(all_sets[,c("YEAR","DAY","MONTH","TIMEHR","TIMEMIN")]),]
#*************************************************
#		Exteme Point
#*************************************************



# Look at the most costly accident in the data set

boxplot(nodup_all_sets$ACCDMG)

# Check out the narratives for this extreme accident

nodup_all_sets[which.max(nodup_all_sets$ACCDMG),c("ACCDMG","YEAR","MONTH","DAY","NARR1")]
# it's the 9/11 attack

# what should we do?
no911_all_sets <- nodup_all_sets[-which.max(nodup_all_sets$ACCDMG),]

#********************************************
# Missing data
#********************************************

# Are we missing values for any variables?
# yes!

# How many?


nafind <- function(x) {
  sum(is.na(x))
}

nacount_all_sets <- apply(no911_all_sets,2,"nafind")

sum(apply(no911_all_sets,2,"nafind") > 0)

# Do we need all the variables?

# defintely not

# Remove unnecessary variables, then get a summary
# Keep TYPEQ, we'll use it.

varWna_all_sets <- which(nacount_all_sets > 0)

varWna_all_sets <-
  varWna_all_sets[-which(colnames(no911_all_sets)[varWna_all_sets] == "TYPEQ")]

clean_all_sets <- no911_all_sets[,-varWna_all_sets]

#*************************************************
#
#   Reformating variables
#
#*************************************************


# Type of accident
# put in the values from the variable definitions

summary(clean_all_sets$TYPE)
#clean_2001< factor(clean_2001$TYPE,labels = c("Derailment","HeadsOn","Rearmed","Side","Raking","BrokenTrain","Hwy-Rail",
#                                              "GradeX","Obstruction","Explosive","Fire","Other","SeeNarrative"))
clean_all_sets$TYPE <-
  factor(
    clean_all_sets$TYPE,labels = c(
      "Derailment","HeadsOn","Rearmed","Side","Raking","BrokenTrain","Hwy-Rail",
      "GradeX","Obstruction","Explosive","Fire","Other","SeeNarrative"
    )
  )
summary(clean_all_sets$TYPE)
# Type of Train
# put in the values from the definitions

clean_all_sets$TYPEQ <-
  factor(
    clean_all_sets$TYPEQ,levels = list(
      c("1"),c("2"),c("3"),c("4"),c("5"),c("6"),c("7"),c("8"),c("9"),c("","A","B","C","D","E")
    ),labels = c(
      "Freight","Passenger","Commuter","Work","Single","CutofCars","Yard","Light","Maint","Other"
    )
  )
summary(clean_all_sets$TYPEQ)

# Clean up the values



# New labels



# Don't do it, but think about
# how you can replace the missing values



# Create a new variable called Cause
# that uses the first character in the string labels for cause.

clean_all_sets$Cause <- rep(NA,nrow(clean_all_sets))

clean_all_sets$Cause[which(substr(clean_all_sets$CAUSE,1,1) == "M")] <- "M"
clean_all_sets$Cause[which(substr(clean_all_sets$CAUSE,1,1) == "T")] <- "T"
clean_all_sets$Cause[which(substr(clean_all_sets$CAUSE,1,1) == "S")] <- "S"
clean_all_sets$Cause[which(substr(clean_all_sets$CAUSE,1,1) == "H")] <- "H"
clean_all_sets$Cause[which(substr(clean_all_sets$CAUSE,1,1) == "E")] <- "E"

clean_all_sets$Cause <- factor(clean_all_sets$Cause)

summary(clean_all_sets$Cause)

#*************************************************
#
#   Reformating narratives
#
#*************************************************

# Look at the narratives
# Combine them into one vector
# where each entry is the complete narrative
# for that accident

clean_all_sets$NARR <- rep(NA,nrow(clean_all_sets))

clean_all_sets$NARR <- apply(clean_all_sets[,c(
  "NARR1","NARR2","NARR3","NARR4","NARR5","NARR6","NARR7","NARR8","NARR9","NARR10","NARR11","NARR12","NARR13","NARR14","NARR15"
)],1,paste,collapse = "")



totacts <- clean_all_sets
#********************************************
#
#   Step-by-step approach to cleaning
#
#********************************************
# Removing duplicates
# Don't use AMPM because sometimes they left it blank


totactsClean <-
  totacts[!duplicated(totacts[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN", "ACCDMG")]),]

# Removing 9/11

nine11 <-
  which(totacts$MONTH == 9 &
          totacts$DAY == 11 & totacts$YEAR == 1 & totacts$ACCDMG > 1.5e7)

totactsClean <- totactsClean[-nine11,]

#********************************************
# Removing variables with missing data
#********************************************

# function to find & remove variables with missing
# data but keep ones in list of names

nafind <- function(x) {
  sum(is.na(x))
}

removeNAvar <- function(DF, Names = NULL)
{
  nacount <- apply(DF,2, "nafind")
  varWna <- which(nacount > 0)
  keepidx <- sapply(Names, function(i) which(colnames(DF)[varWna] == i))
  varWna <- varWna[-keepidx]
  DF[,-varWna]
}

# Keeping variables

totactsClean <-
  removeNAvar(
    totacts, list(
      "TYPEQ", "ACCTRKCL", "AMPM", "NARR1", "NARR2", "NARR3", "NARR4", "NARR5", "NARR6", "NARR7", "NARR8", "NARR9", "NARR10", "NARR11", "NARR12", "NARR13", "NARR14", "NARR15"
    )
  )

#*************************************************
#
#   Reformating variables
#
#*************************************************

# Type of accident

totactsClean$TYPE <-
  factor(
    totactsClean$TYPE, labels = c(
      "Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"
    )
  )


# Type of Train
#

# convert missing values to NA

totactsClean$TYPEQ <-
  factor(
    totactsClean$TYPEQ, labels = c(
      "Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"
    )
  )




# Cause
# Create a new variable called Cause
# that uses the first character in the string labels for cause.

totactsClean$CAUSE <-
  factor(sapply(totactsClean$CAUSE, function(i)
    substr(i, 1, 1)))




#**************************************************
#
#   Time series of Accidents
#
#**************************************************

library(ggplot2)

# Putting total and maximum together using symbols
agg <- aggregate(totacts$ACCDMG,by = list(totacts$YEAR4),FUN = sum)
agg2 <- aggregate(totacts$ACCDMG,by = list(totacts$YEAR4),FUN = max)
agg$MAX <- agg2$x


ggplot(data = agg, aes(x = Group.1, y = x, group = 1)) +
  geom_line() +
  geom_point() +
  geom_point(aes(size = MAX,colour = "tomatoRed")) +
  scale_size_continuous(range = c(2, 30)) +
  xlab("Year") + ylab("Total Cost") +
  ggtitle("Total cost by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")

# Repeat this for total killed and total injured and the sum of them.

agg_causalties <- aggregate(totacts$TOTINJ+totacts$TOTKLD,by = list(totacts$YEAR4),FUN = sum)
agg_causalties_max <- aggregate(totacts$TOTINJ+totacts$TOTKLD,by = list(totacts$YEAR4),FUN = max)
agg_causalties$MAX <- agg_causalties_max$x

ggplot(data = agg_causalties, aes(x = Group.1, y = x, group = 1)) +
  geom_line() +
  geom_point() +
  geom_point(aes(size = MAX,colour = "tomatoRed")) +
  scale_size_continuous(range = c(2, 30)) +
  xlab("Year") + ylab("Total casualties") +
  ggtitle("Total casualties by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")


#***********************************
#
# 	Box plots of ACCDMG and TOTKLD
#
#***********************************


# Box plot of ACCDMG and logs

ggplot(data=totacts, aes(x=factor(YEAR4), y=ACCDMG)) +
  geom_boxplot() +
  xlab("Year") + ylab("Total Cost") +
  ggtitle("Total cost by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")

# Log

ggplot(data=totacts, aes(x=factor(YEAR4), y=log(ACCDMG))) +
  geom_boxplot() +
  xlab("Year") + ylab("Log Cost") +
  ggtitle("Total log cost by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none") +
  geom_abline(intercept=log(4*IQR(totacts$ACCDMG)),slope=0,color="red")

# Casualties
# Box plot of Casualties and logs
ggplot(data=totacts, aes(x=factor(YEAR4), y=TOTKLD+TOTINJ)) +
  geom_boxplot() +
  xlab("Year") + ylab("Casualties") +
  ggtitle("Total casualties by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none") +
  geom_abline(intercept=100,slope=0,color="red")

# Extreme Points
big_casualties<-subset(totacts, TOTKLD + TOTINJ > 4* IQR(TOTKLD + TOTINJ))

# ACCDMG
big_damage <- subset(totacts, ACCDMG >4*IQR(ACCDMG))

#******************************************
#
#   Extreme Accidents
#
#******************************************


# Plot only the extreme points
# (extreme defined by the box plot rule)
ggplot(data=big_damage, aes(x=factor(YEAR4), y=log(ACCDMG))) +
  geom_boxplot() +
  xlab("Year") + ylab("Log Cost") +
  ggtitle("Total big log cost by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")

# Casualties
# Box plot of Casualties and logs
ggplot(data=big_casualties, aes(x=factor(YEAR4), y=TOTKLD+TOTINJ)) +
  geom_boxplot() +
  xlab("Year") + ylab("Casualties") +
  ggtitle("Total big casualties by year") +
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")

# Get the values in the box plot


#**************************************************
#
#      Scatter Plot Matrices
#
#**************************************************


# Basic SPM

pairs( ~ TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = totacts)

# Use panel functions for improving this.


#**************************************************
#
#     Beginning Imputation
#
#**************************************************

# How do you impute the values for AMPM?


# Other imputation
# knnImputation in DMwR

