# Marcus Rosti
# mer3ef
# Stat 6430
setwd("~/MSDS/Summer/Stat/Homework/hw03")
library(stringr)
############################################################################################################
# read in the file
lines         <- readLines("transactions.txt")
# only take the lines where Library is mentioned
clean_lines   <- lines[str_detect(lines, "Library")]
# split the strings on tabs
line_list     <- strsplit(clean_lines,"\t")
# turn it into a dataframe
df_list       <- data.frame(matrix(unlist(line_list), nrow=126, byrow=T))
# only take the first and fourth column
final_list    <- df_list[c(1,4)]
# substring the first column
final_list$X1 <- substr(final_list$X1,0,26)
# write the column to a file
write.table(final_list, file = "output.txt",sep = " ", row.names=FALSE, col.names=FALSE,quote=FALSE)

############################################################################################################

rm(list=ls(all=TRUE))

theurl <- "supremecourtclerks.html"

get_seat <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  cleaner <- str_extract(the_line,"CJ")
  if (!is.na(cleaner)) {
    return ("CJ")
  }
  else {
    cleaner_other <- str_extract(the_line,">[0-9]{1,2}<")
    ret <- str_extract(cleaner_other,"[0-9]{1,2}")
    if(is.na(ret)) return (0)
    return (ret)
  }
}
get_num <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,">[0-9]{1,3}<")
  ret <- str_extract(clean,"[0-9]{1,3}")
  if(is.na(ret)) return (0)
  return (ret)
}
get_justice <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,">['a-zA-Z\\s.,]+<")
  ret <- str_extract(clean,"['a-zA-Z\\s.,]+")
  if(is.na(ret)) return ("")
  return (ret)
}
get_clerk <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,">['a-zA-Z\\s.,]+<")
  ret <- str_extract(clean,"['a-zA-Z\\s.,]+")
  if(is.na(ret)) return ("")
  return (ret)
}
get_started <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,"[0-9]{4}")
  ret <- str_extract(clean,"[0-9]{4}")
  if(is.na(ret)) return (0)
  return (ret)
}
get_finished <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,"[0-9]{4}")
  ret <- str_extract(clean,"[0-9]{4}")
  if(is.na(ret)) return (0)
  return (ret)
}
get_school <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,">['a-zA-Z\\s.,\\&]+<")
  ret <- str_extract(clean,"['a-zA-Z\\s.,\\&]+")
  if(is.na(ret)) return ("")
  return (ret)
}
get_previousclerkship <- function(line_number) {
  the_line = NA
  cleaner = NA
  ret = NA
  
  the_line <- lines[line_number]
  clean <- str_extract(the_line,">[0-9a-zA-Z\\s.,]+<")
  ret <- str_extract(clean,"[0-9a-zA-Z\\s.,]+")
  if(is.na(ret)) return ("")
  return (ret)
}

lines <- readLines(theurl)
table_index <- 1
i <- 144
rm(clean_table)
clean_table <- matrix(nrow=1960, ncol=8)
dimnames(clean_table) <- list(c(1:1960),c("Seat","Num","Justice","Clerk","Started","Finished","School","Previous Clerkship"))
while (i < 19744) {
  clean_table[table_index,1] <- get_seat(i+1)
  clean_table[table_index,2] <- get_num(i+2)
  clean_table[table_index,3] <- get_justice(i+3)
  clean_table[table_index,4] <- get_clerk(i+4)
  clean_table[table_index,5] <- get_started(i+5)
  clean_table[table_index,6] <- get_finished(i+6)
  clean_table[table_index,7] <- get_school(i+7)
  clean_table[table_index,8] <- get_previousclerkship(i+8)
  table_index = table_index + 1
  i = i + 10
}

## 2 a
clean_table[str_detect(clean_table[,4],"Kravitz, D")]
# Sandra Day O'Connor
 
## 2 b
length(unique(clean_table[str_detect(clean_table[,7],"Texas"), ][,4]))
# there are thirty unique texas goers

## 2 c
unique(clean_table[str_detect(clean_table[,3],"Black, H"),][,4])
length(unique(clean_table[str_detect(clean_table[,3],"Black, H"),][,4]))
# he had 33 clerks

## 2 d
max(table(as.factor(clean_table[,7])))
# Harvard produced the most with 503 going there

## 2 e
table(clean_table[,3])
# Antonia Scalia has the most with 117

## 2 f
df_table<-data.frame(clean_table)
write.csv(df_table,"clerks.csv",row.names = FALSE)
