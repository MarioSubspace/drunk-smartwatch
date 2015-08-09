# This script is used to remove rows with missing values.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# R-version: 3.2.1
# 28 July 2015

args <- commandArgs(trailingOnly=TRUE)

options(warn=1, stringsAsFactors=FALSE)

if (length(args) != 2) {
   cat("Error: invalid number of arguments. 'dataset savefile'\n")
} else {
   result <- read.csv(args[1], header=TRUE)
   cat(" SIZE:",nrow(result),"\n\n")
   for (i in 1:ncol(result)) {
      vt <- as.vector(result[,i])
      ind <- grep(pattern="null", x=vt)
      if (length(ind) != 0) {
         cat(" ROW",i,"\n")
         cat(" REMOVING INDICES:",ind,"\n")
         result <- result[-ind,]
         cat(" SIZE:",nrow(result),"\n\n")
      }
   }
   write.csv(result, file=args[2], row.names=FALSE)
   cat("",nrow(result),"written to",args[2],"\n")
}
