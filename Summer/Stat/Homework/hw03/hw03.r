# Marcus Rosti
# mer3ef
# Stat 6430

library(stringr)

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
