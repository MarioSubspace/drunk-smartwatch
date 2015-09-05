args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 2) {
  cat("Rscript combine.r #_files output_name")
} else {
  ds <- read.csv("00.csv", header=TRUE)
  ds$subject <- rep(0, nrow(ds))

  cat(nrow(ds), "rows added from 00.csv\n")

  for (i in seq(1, as.numeric(args[1])-1, 1)) {
    tmp <- read.csv(sprintf("%02d.csv", i), header=TRUE)
    tmp$subject <- rep(i, nrow(tmp))
    ds <- rbind(ds, tmp)
    cat(nrow(tmp), "rows added from", sprintf("%02d.csv", i), "\n")
  }

  write.csv(ds, sprintf("%s.csv", args[2]), row.names=FALSE)
}
