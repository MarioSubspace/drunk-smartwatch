# Collection of R functions by Mario A. Gutierrez.
# Designed for project DRUNK-SMARTWATCH.

# Scales data to have range from 0 to 1.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
#	 x - the data to scale.
# Returns: the scaled data object.
rescale_01 <- function(x) {
    minx <- min(x)
    result <- (x - minx) / (max(x) - minx)
    return (result)
}

# Draws a scatterplot with a curve over it, and a line plot in the background.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
#	 data - is the data.frame with the data
#	 bac - the data for the BAC line plot in the back.
#	 front - the data for the scatterplot in the front.
#	 use_linear - fit a line to the 'front' data if TRUE, fits a curve otherwise
# 		(default: TRUE).
# Returns: the resulting ggplot2 object.
fadoodle <- function(data, bac, front, front.label="", curve='l') {
    require('ggplot2')
	
    ifelse(front.label == "", front.label <- toupper(front), front.label <- toupper(front.label))
    ylab <- paste("BAC vs.", front.label)
	
    result <- ggplot(data, aes_string(x = "timestamp", y = bac)) +
        geom_line() +
        geom_point(aes_string(x = "timestamp", y = front), color = "red", size = 1.5) +
        theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
        ylab(ylab) +
        xlab("")
	
    if (!is.null(curve))	{
        if (curve == 's') {
            result <- result + geom_smooth(aes_string(x = "timestamp", y = front))
        } else {
            result <- result + geom_smooth(aes_string(x = "timestamp", y = front), method="lm")
        }
    }
	
    result <- result + facet_wrap( ~ subject, ncol=3)
	
    return(result)
}

# Draws the heart rate data split in two colors by a set BPM. Used for one of the
# figures in the paper. Also draws the BAC line in the background.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
#	 data - is the data.frame with the data
#	 bac - the data for the BAC line plot in the back.
#	 front - the data for the heart rate scatterplot in the front.
#	 split_bpm - the bpm to split the heart rate data at.
# Returns: the resulting ggplot2 object.
splithr <- function(data, bac, front, split_bpm=85, front.label="") {
    require('ggplot2')
	
	ifelse(front.label == "", front.label <- toupper(front), front.label <- toupper(front.label))
	ylab <- paste("BAC vs.", front.label)
	
	colors <- rep("red", nrow(data))
	colors[data$ms_heart_rate_bpm < split_bpm] <- "blue"
	
	result <- ggplot(data, aes_string(x = "timestamp", y = bac)) +
		geom_line() +
		geom_point(aes_string(x = "timestamp", y = front), color = colors, size = 1.5) +
		theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
		ylab(ylab) +
		xlab("")
	
	return(result)
}

# Rescales feature data using rescale_01, per subject (0 to max(subject)).
# Author: Mario A. Gutierrez (mag262@txstate.edu)
#   data - the data frame.
#   feature - the set of features to scale.
# Returns: the resulting scaled features in a new data frame.
rescalePerSubject <- function(data, feature) {
    selected <- data[, c(feature, "subject")]
    for (i in seq(0, max(data$subject, 1))) {
        selected[selected$subject == i, feature] <- rescale_01(selected[, feature][selected$subject == i])
    }
    return(selected[, feature])
}

# Selects a random partition in a repeatable manner for the training set.
# Returns: a boolean vector, TRUE if row is in the partition.
trainPartition <- function(data, percent) {
    cat("Total of", nrow(data), "rows.\n")
    set.seed(1234) # A constant seed, so we get the same results every time.
    result <- rep(FALSE, nrow(data))
    sct <- sample(1:nrow(data), ceiling(percent * nrow(data)))
    result[sct] <- TRUE
    cat("Selected", sum(result), "rows for training set, ", sum(!result), "for test.\n")
    return(result)
}

# Uniformly shuffles the samples in the data set.
# Returns: a numeric vector of the shuffled indices.
shuffleUniform <- function(data) {
    set.seed(1234) # A constant seed, so we get the same results every time.
    return(sample(1:nrow(data), nrow(data)))
}

# Calculate the R-sq value between two vectors.
#   y - the actual values.
#   f - the predicted values.
# Returns: the R-sq value.
rsq <- function(y, f) { return(1 - (sum((y-f)^2)/sum((y-mean(y))^2))) }

resid <- function(data, model) {
    df <- nrow(data) - nrow(summary(model)$coefficients)
    err <- sqrt(sum(residuals(model)^2) / df)
    cat("Residual standard error:", err,"on", df, "degrees of freedom.\n");
    return(err)
}

# Calculate the RMSE between two vectors.
#   y - the actual values.
#   f - the predicted values.
# Returns: the RMSE.
rmse <- function(y, f) { return(sqrt(sum((y - f)^2) / length(y))) }

# Runs the logistic regression experiments.
#   data - the raw data.
#   k - number of folds in cross-validation.
#   threshold - the threshold for labeling DRUNK or SOBER.
#   toFile - file to save output to.
runLogisticRegression <- function(data, k, threshold, toFile=NULL) {
    if (!is.null(toFile)) sink(file=toFile)
    
    require('ggplot2')
    
    cat("k:",k,", bac-threshold:",threshold,"\n\n")
    
    # INITIAL DATA SETUP.
    cat("\nShuffling dataset.\n")
    shuf <- shuffleUniform(data)
    
    cat("Normalizing columns per subject...\n\n")
    data$ms_heart_rate_bpm <- rescalePerSubject(data, "ms_heart_rate_bpm")
    data$ms_skin_temperature_celsius <- rescalePerSubject(data, "ms_skin_temperature_celsius")
    cat("Heart Rate Summary:\n")
    print(summary(data$ms_heart_rate_bpm))
    cat("Skin Temperature Summary:\n")
    print(summary(data$ms_skin_temperature_celsius))
    
    cat("\nApplying BAC threshold...\n")
    data$bac_observed <- ifelse(data$bac_observed > threshold, "DRUNK", "SOBER")
    data$bac_observed <- as.factor(data$bac_observed)
    print(summary(data$bac_observed))
    
    psize <- floor(nrow(data) / k)
    sel <- 1:psize
    
    cat("\nRunning", k, "fold cross-validation.\n")
    cat("Partition size:",psize,"\n\n")
    
    perf <- data.frame(prec=numeric(), recl=numeric(), f1=numeric())
    
    # TRAIN THE MODEL (cross-validation).
    for (i in seq(1, k, 1)) {
        train <- shuf[-sel] # !psize selection of shuffled indices
        test <- shuf[sel] # psize selection of shuffled indices
        
        cat("Fold:",i,"\n")
        dsr <- sum(data[train,]$bac_observed == "DRUNK") / length(train)
        dse <- sum(data[test,]$bac_observed == "DRUNK") / length(test)
        cat("% Drunk:",round(dsr, 3)*100,"(train),",round(dse, 3)*100,"(test).\n")
        
        mdl <- glm(bac_observed ~
            ms_heart_rate_bpm +
            ms_skin_temperature_celsius +
            ms_accelerometer_x +
            ms_accelerometer_y +
            ms_accelerometer_z ,
            data=data[train,],
            family=binomial(link="logit")
        )
        
        data$pred <- predict(mdl, data, type="response")
        
        # 0.32 is was determined from density plot.
        conf <- table(pred=ifelse(data[test,]$pred > 0.32, "SOBER", "DRUNK"), actual=data[test,]$bac_observed)
        prec <- conf[1, 1] / sum(conf[1, ])
        recl <- conf[1, 1] / sum(conf[, 1])
        f1 <- 2 * ((prec * recl) / (prec + recl))
        perf <- rbind(perf, data.frame(prec=prec, recl=recl, f1=f1))
        cat("PR:",prec,"REC:",recl,"F1:",f1,"\n\n")
        
        system(paste("say Fold", i, "calculated."))
        
        sel <- sel + psize
    }
    
    print(perf)
    
    cat("\nPrecision:",mean(perf$prec),"+/-",sd(perf$prec),"\n")
    cat("Recall:",mean(perf$recl),"+/-",sd(perf$recl),"\n")
    cat("F1-score:",mean(perf$f1),"+/-",sd(perf$f1),"\n\n")
    
    # USED TO FIND THRESHOLD.
    #plt <- ggplot(data[test,], aes(x=pred, color=bac_observed, linetype=bac_observed)) +
    #    geom_density() +
    #    theme(legend.title=element_blank()) +
    #    xlab("") +
    #    ylab("")
    
    #ggsave("log_pred_density.png", plt, width=8, height=4)
    
    if (!is.null(toFile)) sink()
}

# Runs the SVM experiments.
#   data - the raw data.
#   reduce - what fraction of the overall data to use.
#   k - number of folds for cross-validation.
#   threshold - the threshold for labelling DRUNK or SOBER.
#   soberWeight - higher values penalize false positives more.
#   toFile - file to save output to.
runSVM <- function(data, reduce, k, threshold, soberWeight=1, toFile=NULL) {
    if (!is.null(toFile)) sink(file=toFile)
    
    require('ggplot2')
    require('kernlab')
    
    cat("k:", k, "threshold:", threshold, "\n")
    cat("reduce:", reduce, "soberWeight:", soberWeight, "\n\n")
    
    # INITIAL DATA SETUP.
    set.seed(1234)
    data <- data[sample(1:nrow(data), floor(nrow(data)*reduce)),]
    cat("Dataset subsampled to",nrow(data),"samples.\n\n")
    
    cat("Shuffling dataset.\n")
    shuf <- shuffleUniform(data)
    
    cat("Normalizing columns per subject...\n\n")
    data$ms_heart_rate_bpm <- rescalePerSubject(data, "ms_heart_rate_bpm")
    data$ms_skin_temperature_celsius <- rescalePerSubject(data, "ms_skin_temperature_celsius")
    cat("Heart Rate Summary:\n")
    print(summary(data$ms_heart_rate_bpm))
    cat("Skin Temperature Summary:\n")
    print(summary(data$ms_skin_temperature_celsius))
    
    cat("\nApplying BAC threshold...\n")
    data$bac_observed <- ifelse(data$bac_observed > threshold, "DRUNK", "SOBER")
    data$bac_observed <- as.factor(data$bac_observed)
    print(summary(data$bac_observed))
    
    psize <- floor(nrow(data) / k)
    sel <- 1:psize
    
    cat("\nRunning", k, "fold cross-validation.\n")
    cat("Partition size:",psize,"\n\n")
    
    perf <- data.frame(prec=numeric(), recl=numeric(), f1=numeric())
    
    # TRAIN THE MODEL (cross-validation).
    for (i in seq(1, k, 1)) {
        train <- shuf[-sel] # !psize selection of shuffled indices
        test <- shuf[sel] # psize selection of shuffled indices
        
        cat("Fold:",i,"\n")
        dsr <- sum(data[train,]$bac_observed == "DRUNK") / length(train)
        dse <- sum(data[test,]$bac_observed == "DRUNK") / length(test)
        cat("% Drunk:",round(dsr, 3)*100,"(train),",round(dse, 3)*100,"(test).\n")
        
        mdl <- ksvm(bac_observed ~
            ms_heart_rate_bpm +
            ms_skin_temperature_celsius +
            ms_accelerometer_x +
            ms_accelerometer_y +
            ms_accelerometer_z ,
            data=data[train,],
            kernel='rbfdot',
            C=75,
            class.weights=c('DRUNK'=1, 'SOBER'=soberWeight)
        )
        # class.weights=1, 5 (avoid false positives)
        # class.weights=1, 1 (equal)
        
        data$pred <- predict(mdl, data, type="response")
        
        # EVALUATE MODEL
        conf <- with(data[test,], table(pred=pred, actual=bac_observed))
        prec <- conf[1, 1] / sum(conf[1, ])
        recl <- conf[1, 1] / sum(conf[, 1])
        f1 <- 2 * ((prec * recl) / (prec + recl))
        perf <- rbind(perf, data.frame(prec=prec, recl=recl, f1=f1))
        cat("PR:",prec,"REC:",recl,"F1:",f1,"\n\n")
        
        system(paste("say Fold", i, "calculated."))
        
        sel <- sel + psize
    }
    
    print(perf)
    
    cat("\nPrecision:",mean(perf$prec),"+/-",sd(perf$prec),"\n")
    cat("Recall:",mean(perf$recl),"+/-",sd(perf$recl),"\n")
    cat("F1-score:",mean(perf$f1),"+/-",sd(perf$f1),"\n\n")
    
    if (!is.null(toFile)) sink()
    
    system("say Calculations are complete.")
}

# Runs experiments with a neural network.
#   data - the raw data to use.
#   reduce - the fraction of data to use overall.
#   k - the number of folds for cross-validation.
#   hidden - a vector representing the number of nodes per hidden layer.
#   toFile - the file to save the output to.
runANN <- function(data, reduce, k, hidden, toFile=NULL) {
    require('ggplot2')
    require('nnet')
    
    if (!is.null(toFile)) sink(file=toFile)
    
    cat("reduce:", reduce,", k:", k,"\n")
    cat("hidden:", hidden, "\n\n")
    
    # INITIAL DATA SETUP.
    if (reduce < 1) {
        set.seed(1234)
        data <- data[sort(sample(1:nrow(data), floor(nrow(data)*reduce))),]
        cat("Dataset subsampled to",nrow(data),"samples.\n")
    }
    
    cat("Shuffling dataset.\n")
    shuf <- shuffleUniform(data)
    
    cat("Normalizing columns per subject...\n\n")
    data$ms_heart_rate_bpm <- rescalePerSubject(data, "ms_heart_rate_bpm")
    data$ms_skin_temperature_celsius <- rescalePerSubject(data, "ms_skin_temperature_celsius")
    cat("Heart Rate Summary:\n")
    print(summary(data$ms_heart_rate_bpm))
    cat("Skin Temperature Summary:\n")
    print(summary(data$ms_skin_temperature_celsius))
    
    psize <- floor(nrow(data) / k)
    sel <- 1:psize
    
    cat("\nRunning", k, "fold cross-validation.\n")
    cat("Partition size:",psize,"\n\n")
    
    perf <- data.frame(vrsq=numeric(), vrmse=numeric())
    
    bestTest.rsq <- 0
    bestTest <- numeric()
    
    # TRAIN THE NEURAL NETWORK.
    for (i in seq(1, k, 1)) {
        train <- shuf[-sel] # !psize selection of shuffled indices
        test <- shuf[sel] # psize selection of shuffled indices
        
        cat("Fold:",i,"\n")
        
        mdl <- nnet(bac_observed ~
            ms_heart_rate_bpm +
            ms_skin_temperature_celsius +
            ms_accelerometer_x +
            ms_accelerometer_y +
            ms_accelerometer_z,
            data=data[train,],
            size=hidden,
            linout=TRUE,
            decay = 5e-4,
            maxit=200
        );
        
        features <- c(
            "ms_heart_rate_bpm",
            "ms_skin_temperature_celsius",
            "ms_accelerometer_x",
            "ms_accelerometer_y",
            "ms_accelerometer_z"
        )
    
        data$pred <- predict(mdl, data[,features])
    
        # EVALUATE PERFORMANCE.
        vvrsq <- rsq(data[train, ]$bac_observed, data[train, ]$pred)
        vvrmse <- rmse(data[train, ]$bac_observed, data[train, ]$pred)
        cat("Train R-SQ:",vvrsq, "RMSE:", vvrmse, "\n")
        
        vrsq <- rsq(data[test, ]$bac_observed, data[test, ]$pred)
        vrmse <- rmse(data[test, ]$bac_observed, data[test, ]$pred)
        perf <- rbind(perf, data.frame(vrsq=vrsq, vrmse=vrmse))
        cat("Test  R-SQ:",vrsq, "RMSE:", vrmse, "\n\n")
        
        if (vrsq > bestTest.rsq) {
            bestTest.rsq <- vrsq
            bestTest <- data[test, ]
            bestTest$x <- test
        }
        
        system(paste("say Fold", i, "calculated."))
        
        sel <- sel + psize
    }
    
    print(perf)
    
    cat("\nR-SQ:", mean(perf$vrsq), "+/-", sd(perf$vrsq), "\n")
    cat("RMSE:", mean(perf$vrmse), "+/-", sd(perf$vrmse), "\n\n")
    
    # PLOT THE PREDICTIONS.
    cat("Plotting best scoring model on test data with R-SQ",bestTest.rsq,"\n")
    plt <- ggplot(data=bestTest) +
        geom_point(aes(x=x, y=pred), color='blue', size=0.9) +
        geom_point(aes(x=x, y=bac_observed), size=0.8) +
        xlab("") +
        ylab("Predicted vs. Actual")
    ggsave("nn_best_test.png", plt, width=8, height=4)
    
    if (!is.null(toFile)) sink()
    
    system("say Calculations are complete.")
}

