# This script is used to normalize values of specified columns.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# R-version: 3.2.1
# 29 July 2015

args <- commandArgs(trailingOnly=TRUE)

options(warn=1, stringsAsFactors=FALSE)

if (length(args) != 2) {
   cat("Error: invalid number of arguments. 'dataset savefile'\n")
} else {
   result <- read.csv(args[1], header=TRUE)
   
   cat("NORMALIZING...\n")
   for (i in 1:9) result[,i] <- scale(result[,i]) # Accelerometer and gyroscope values.
   result$ms_heart_rate_bpm <- scale(result$ms_heart_rate_bpm)
   result$ms_skin_temperature_celsius <- scale(result$ms_skin_temperature_celsius)
   
   write.csv(result, args[2], row.names=FALSE)
   cat("DONE.\n")
}
