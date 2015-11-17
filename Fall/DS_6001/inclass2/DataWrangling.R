# Marcus Rosti
# mer3ef
# DS 6001

#*****************************************************************************
#
#					Data Wrangling - Train Accidents
#
#*****************************************************************************

setwd("~/MSDS/Fall/DS_6001/inclass2")

#************************************
# Reading in data
#************************************
l

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

# GG
