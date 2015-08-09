# This script is used to concatenate datasets.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# R-version: 3.2.1
# 27 July 2015

args <- commandArgs(trailingOnly=TRUE)

options(warn=1, stringsAsFactors=FALSE)

if (length(args) < 2) {
   cat("Error: too few arguments. 'savefile file1 file2 ...'\n")
} else {
   cat(" ADDING FILE",args[2],"\n")
   result <- read.csv(args[2], header=TRUE)
   cat(" SIZE:",nrow(result),"\n\n")
   
   if (length(args) > 2) {
      for (i in 3:length(args)) {
         cat(" ADDING FILE",args[i],"\n")
         result <- rbind(result, read.csv(args[i], header=TRUE))
         cat(" SIZE:",nrow(result),"\n\n")
      }
   }
   
   cat(" SAVING DATA TO",args[1],"\n")
   write.csv(result, file=args[1], row.names=FALSE)
   cat("",nrow(result),"rows saved to file.\n")
}
