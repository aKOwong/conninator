setwd("D:/Dropbox/Connie")
data.df <- read.csv("bw_itga6.csv")

normalization <- function(data.df, week){
  normtable <- data.df
  weeklist <- character()
  for (i in colnames(data.df)){
    if (substring(i,1,2) == "bw"){weeklist <- append(weeklist, i)}
    }
  for (row in seq(1, dim(data.df)[1])){
   for (col in weeklist){normtable[row, col] <- ( data.df[row,col]) - data.df[row,week]}
  }
  return(normtable)
}

cleanup <- function(table, week){
  no_good_rows <- numeric()
  for (row in seq(1, dim(data.df)[1])){
    if(table[row,"Geno"] == "" | is.na(table[row,week])){no_good_rows <- append(no_good_rows, row)}
  }
  return(table[-no_good_rows,])
}








normalized_table <- normalization(data.df, "bw4")
cleaned_table <- cleanup(normalized_table, "bw4")
