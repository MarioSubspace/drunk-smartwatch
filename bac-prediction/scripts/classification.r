# This script runs a ANN experiment on the data.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# R-version: 3.2.1
# 30 July 2015

args <- commandArgs(trailingOnly=TRUE)

options(warn=1, stringsAsFactors=FALSE)

if (length(args) == 0) {
   cat("Usage: Rscript ann_exp.r <input_data> <output_file>\n")
} else {

# Cross-validation parameters.
k <- 6   # Number of partitions

# ANN training parameters.
topo = c(5, 3)    # The ANN hidden layer topology
errt = 0.00999   # Error threshold for ANN training stopping point

# BAC parameters.
drunk_thresh = 0.06 # 'Drunk' above this.

# Create list of feature names.
perfs <- c("ms_skin_temperature_celsius")

# Create 'formula' from PMC names.
form <- as.formula(paste("bac_observed ~", paste(perfs, collapse="+")))

# Read in samples from file.
ds <- read.csv(args[1], header=TRUE)

ds <- ds[grep("LOCKED", ds$ms_heart_rate_quality), ]

# Change labels to classification labels.
ds$bac_observed <- ds$bac_observed > drunk_thresh

# Shuffle samples.
ds <- ds[sample(1:nrow(ds)), ]

# Create data frame to enter results.
result_header <- c("TP", "FP", "TN", "FN", "PREC", "RECL", "FSCR", "TH")
result <- as.data.frame(matrix(0.0, nrow=k+1, ncol=length(result_header)))
names(result) <- result_header

# Calculate the partition size for cross-validation.
psize <- floor(nrow(ds) / k)
sel <- 1:psize

cat("\nDataset has", nrow(ds), "total samples.\n")
cat("Running ", k, "-fold cross-validation ", sep="")
cat("(",psize, " samples/partition)\n", sep="")

# Run the cross-validation.
for (i in 1:k) {

   # Partition dataset.
   data.h <- ds[sel, ]  # Hold-out partition.
   data.t <- ds[-sel, ] # Use rest for training.
   
   # Do logistic regression training and test.
   regm <- glm(form, family=binomial(link="probit"), data=data.t)
   predictions <- predict(regm, newdata=data.h, type="response")

   fscr <- 0.0
   prec <- 0.0
   recl <- 0.0
   pickr <- 0.0 # Metric used to score thresholds.
   for (j in seq(0.05, 1.0, 0.05)) {
      predictions.c <- predictions > j
      tp.new <- sum( data.h$bac_observed &  predictions.c)
      fn.new <- sum( data.h$bac_observed & !predictions.c)
      fp.new <- sum(!data.h$bac_observed &  predictions.c)
      tn.new <- sum(!data.h$bac_observed & !predictions.c)
      
      #pickr.new <- tp.new / fp.new + 0.0001
      pickr.new <- tp.new / sum(predictions.c)
      if ( !is.na(pickr.new) & (pickr.new >= pickr) ) {
         tp <- tp.new
         fn <- fn.new
         fp <- fp.new
         tn <- tn.new
         prec <- tp.new / sum(predictions.c)
         recl <- tp.new / sum(data.h$bac_observed)
         fscr <- 2 * (prec * recl) / (prec + recl)
         th <- j
      }
   }

   result[i,]$TP <- tp
   result[i,]$FP <- fp
   result[i,]$TN <- tn
   result[i,]$FN <- fn
   result[i,]$PREC <- prec
   result[i,]$RECL <- recl
   result[i,]$FSCR <- fscr
   result[i,]$TH <- th
   
   # Move to next partition.
   sel = sel + psize
}

result[nrow(result), ] <- colMeans(result[1:k, ])

print(result)

write.table(result, args[2], sep="\t", row.names=FALSE)

}
