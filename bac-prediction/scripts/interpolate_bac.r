# This script is used to interpolate BAC values.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# R-version: 3.2.1
# 28 July 2015

require(ggplot2)

args <- commandArgs(trailingOnly=TRUE)

options(warn=1, stringsAsFactors=FALSE)

if (length(args) != 2) {
   cat("Error: invalid number of arguments. 'dataset savefile'\n")
} else {
   result <- read.csv(args[1], header=TRUE)

   # Get the unique BAC values.
   unq <- unique(result$bac_observed)
   cat("Unique BAC values:",unq,"\n")

   # Get the indicies of the first of each value encountered.
   ind <- match(unq, result$bac_observed)

   #ind_drnk <- grep(pattern="ACTION_DOWN", result$button_touch_event)
   #ind_drnk <- setdiff(ind_drnk, ind_drnk+1) # Only want first ind of each event.

   ds <- result[ind,]

   plt <- ggplot(data=ds, aes(x=ds$timestamp, y=ds$bac_observed)) +
          ylab("BAC Observed") + xlab("Time") +
          geom_point() +
          theme_bw(base_size=8) +
          stat_smooth(method="loess")
   ggsave("interpolation.pdf")
   #points(x=result$button_touch_timestamp[ind_drnk], y=rep(0.03,length(ind_drnk)), col="blue", pch=16)
   
   # Recalculate BAC value column based on timestamp.
   mdl <- loess(ds$bac_observed ~ ds$timestamp)
   result$bac_observed <- predict(mdl, result$timestamp)

   result <- na.omit(result)
   
   #plt <- ggplot(data=result, aes(x=result$timestamp, y=result$bac_observed)) +
   #       ylab("BAC Interpolated") + xlab("Time") +
   #       geom_line() +
   #       theme_bw(base_size=8)
   #ggsave("interpolation_result.pdf")

   write.csv(result, args[2], row.names=FALSE)
}
