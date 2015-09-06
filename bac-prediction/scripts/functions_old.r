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

# Runs linear regression experiments.
#   data - the raw unnormalized data.
#   trainSplit - what fraction to use for training.
runLinearRegression <- function(data, trainSplit, toFile=NULL) {
    require('ggplot2')
    if (!is.null(toFile)) sink(file=toFile)
    
    cat("trainSplit:", trainSplit,"\n\n")
    
    cat("Normalizing columns per subject...\n\n")
    data$ms_heart_rate_bpm <- rescalePerSubject(data, "ms_heart_rate_bpm")
    data$ms_skin_temperature_celsius <- rescalePerSubject(data, "ms_skin_temperature_celsius")
    cat("Heart Rate Summary:\n")
    print(summary(data$ms_heart_rate_bpm))
    cat("Skin Temperature Summary:\n")
    print(summary(data$ms_skin_temperature_celsius))
    
    cat("\nGetting train/test partition vector...\n\n")
    train <- trainPartition(data, trainSplit)
    test <- !train
    
    cat("\n-----------------------------------------------------------\n")
    # Do linear analysis on skin temperature only.
    cat("\nTraining lm() on skin temperature...\n")
    mdl <- lm(bac_observed ~ ms_heart_rate_bpm, data[train,])
    print(summary(mdl))
    
    cat("\nMaking predictions...\n")
    data$pred <- predict(mdl, data)
    cat("k.\n\n")
    
    data$x <- rep(0, nrow(data))
    data[test,]$x <- 1:nrow(data[test,])
    
    cat("Performance on test data:\n")
    rsq.c <- rsq(data$bac_observed[test], data$pred[test])
    rmse.c <- rmse(data$bac_observed[test], data$pred[test])
    cat("R-squared:", rsq.c, "\n")
    cat("RMSE:", rmse.c, "\n")
    
    cat("\n-----------------------------------------------------------\n")
    
    # Do analysis on skin and heart rate.
    cat("\nTraining lm() on skin temperature and heart rate...\n")
    mdl <- lm(bac_observed ~ ms_heart_rate_bpm + ms_skin_temperature_celsius, data[train,])
    print(summary(mdl))
    
    cat("\nMaking predictions...\n")
    data$pred <- predict(mdl, data)
    cat("k.\n\n")
    
    data$x <- rep(0, nrow(data))
    data[test,]$x <- 1:nrow(data[test,])
    
    plt <- ggplot(data=data[test,]) +
    geom_point(aes(x=x, y=bac_observed),
    position=position_jitter(w=0.005, h=0.005), size=0.4) +
    geom_line(aes(x=x, y=pred), color='blue') +
    xlab("") +
    ylab("Predicted vs. Actual (jittered)")
    ggsave("lm_hr_tmp.png", plt, width=8, height=4)
    
    cat("Performance on test data:\n")
    rsq.c <- rsq(data$bac_observed[test], data$pred[test])
    rmse.c <- rmse(data$bac_observed[test], data$pred[test])
    cat("R-squared:", rsq.c, "\n")
    cat("RMSE:", rmse.c, "\n")
    
    cat("\n-----------------------------------------------------------\n")
    
    # Do analysis on all features
    cat("\nTraining lm() on all features...\n")
    mdl <- lm(bac_observed ~
        ms_heart_rate_bpm +
        ms_skin_temperature_celsius +
        ms_accelerometer_x +
        ms_accelerometer_y +
        ms_accelerometer_z , data[train,])
    print(summary(mdl))
    
    cat("\nMaking predictions...\n")
    data$pred <- predict(mdl, data)
    cat("k.\n\n")
    
    data$x <- rep(0, nrow(data))
    data[test,]$x <- 1:nrow(data[test,])
    
    plt <- ggplot(data=data[test,]) +
    geom_point(aes(x=x, y=bac_observed),
    position=position_jitter(w=0.005, h=0.005), size=0.4) +
    geom_line(aes(x=x, y=pred), color='blue') +
    xlab("") +
    ylab("Predicted vs. Actual (jittered)")
    ggsave("lm_all.png", plt, width=8, height=4)
    
    cat("Performance on test data:\n")
    rsq.c <- rsq(data$bac_observed[test], data$pred[test])
    rmse.c <- rmse(data$bac_observed[test], data$pred[test])
    cat("R-squared:", rsq.c, "\n")
    cat("RMSE:", rmse.c, "\n")
    
    if (!is.null(toFile)) sink()
    
    #system("say Calculations complete.")
    system("afplay wtfboom.wav")
}

# Runs experiments with a neural network.
#   data - the raw data to use.
#   reduce - the percentage of data to use overall
#   trainSplit - the amount of the overall data to use for training.
#   toFile - the file to save the output to.
neuralExperiments <- function(data, reduce, trainSplit, hidden, toFile=NULL) {
    require('ggplot2')
    require('neuralnet')
        
    if (!is.null(toFile)) sink(file=toFile)
    
    cat("reduce:", reduce,", trainSplit:", trainSplit,"\n")
    cat("hidden:",hidden,"\n\n")
    
    # INITIAL DATA SETUP.
    cat("Reducing data set (training set as overall).\n")
    data <- data[trainPartition(data, reduce), ]
    
    cat("\nSplitting into training and test partitions.\n")
    train <- trainPartition(data, trainSplit)
    test <- !train
    
    cat("Normalizing columns per subject...\n\n")
    data$ms_heart_rate_bpm <- rescalePerSubject(data, "ms_heart_rate_bpm")
    data$ms_skin_temperature_celsius <- rescalePerSubject(data, "ms_skin_temperature_celsius")
    cat("Heart Rate Summary:\n")
    print(summary(data$ms_heart_rate_bpm))
    cat("Skin Temperature Summary:\n")
    print(summary(data$ms_skin_temperature_celsius))
    
    # TRAIN THE NEURAL NETWORK.
    mdl <- neuralnet(bac_observed ~
        ms_heart_rate_bpm +
        ms_skin_temperature_celsius +
        ms_accelerometer_x +
        ms_accelerometer_y +
        ms_accelerometer_z,
        data=data[train,],
        hidden=hidden
    )
      
    features <- c(
      "ms_heart_rate_bpm",
      "ms_skin_temperature_celsius",
      "ms_accelerometer_x",
      "ms_accelerometer_y",
      "ms_accelerometer_z"
    )
    
    data$pred <- compute(mdl, data[,features])$net.result
    
    # ESTIMATE PERFORMANCE.
    cat("\nPerformance on TRAINING data:\n")
    rs <- rsq(data[train, ]$bac_observed, data[train, ]$pred)
    cat("R-squared:",rs,"\n")
    vrmse <- rmse(data[train, ]$bac_observed, data[train, ]$pred)
    cat("RMSE:", vrmse, "\n\n")
    
    cat("Performance on TEST data:\n")
    rs <- rsq(data[test, ]$bac_observed, data[test, ]$pred)
    cat("R-squared:",rs,"\n")
    vrmse <- rmse(data[test, ]$bac_observed, data[test, ]$pred)
    cat("RMSE:", vrmse, "\n\n")
    
    # PLOT THE PREDICTIONS.
    data$x <- rep(0, nrow(data))
    data[test,]$x <- 1:nrow(data[test,])
    
    plt <- ggplot(data=data[test,]) +
    geom_point(aes(x=x, y=bac_observed),
    position=position_jitter(w=0.005, h=0.005), size=0.4) +
    geom_point(aes(x=x, y=pred), color='blue', size=0.9) +
    xlab("") +
    ylab("Predicted vs. Actual (jittered)")
    ggsave("nn_all.png", plt, width=8, height=4)
    
    if (!is.null(toFile)) sink()
    
    #system("say Calculations complete.")
    system("afplay wtfboom.wav")
}

# Runs the logistic regression experiments.
#   data - the raw data.
#   threshold - the threshold for labelling DRUNK or SOBER.
#   toFile - file to save output to.
runLogisticRegression <- function(data, trainSplit, threshold, toFile=NULL) {
    if (!is.null(toFile)) sink(file=toFile)
    
    require('ggplot2')
    
    cat("trainSplit:",trainSplit,"threshold:",threshold,"\n\n")
    
    # INITIAL DATA SETUP.
    cat("\nSplitting into training and test partitions.\n")
    train <- trainPartition(data, trainSplit)
    test <- !train
    
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
    
    # TRAIN THE MODEL.
    
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
    
    plt <- ggplot(data[test,], aes(x=pred, color=bac_observed, linetype=bac_observed)) +
        geom_density() +
        theme(legend.title=element_blank()) +
        xlab("") +
        ylab("")
    
    ggsave("log_pred_density.png", plt, width=8, height=4)
    
    conf <- table(pred=ifelse(data[test,]$pred > 0.32, "SOBER", "DRUNK"), actual=data[test,]$bac_observed)
    print(conf)
    prec <- conf[1, 1] / sum(conf[1, ])
    cat("\nPrecision:",prec,"\n")
    recl <- conf[1, 1] / sum(conf[, 1])
    cat("Recall:",recl,"\n")
    f1 <- 2 * ((prec * recl) / (prec + recl))
    cat("F1-score:",f1,"\n")
    
    if (!is.null(toFile)) sink()
}

# Runs the SVM experiments.
#   data - the raw data.
#   threshold - the threshold for labelling DRUNK or SOBER.
#   toFile - file to save output to.
runSVM <- function(data, reduce, trainSplit, threshold, toFile=NULL) {
    if (!is.null(toFile)) sink(file=toFile)
    
    require('ggplot2')
    require('kernlab')
    
    cat("trainSplit:",trainSplit,"threshold:",threshold,"\n")
    cat("reduce:", reduce,"\n\n")
    
    # INITIAL DATA SETUP.
    data <- data[trainPartition(data, reduce), ]
    cat("Reduced dataset to", nrow(data), "rows.\n\n")
    
    cat("\nSplitting into training and test partitions.\n")
    train <- trainPartition(data, trainSplit)
    test <- !train
    
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
    
    # TRAIN THE MODEL.
    cat("\nTraining SVM model...\n")
    mdl <- ksvm(bac_observed ~
        ms_heart_rate_bpm +
        ms_skin_temperature_celsius +
        ms_accelerometer_x +
        ms_accelerometer_y +
        ms_accelerometer_z ,
        data=data[train,],
        kernel='rbfdot',
        C=75,
        class.weights=c('DRUNK'=1, 'SOBER'=5)
    )
    # class.weights=1, 5 (avoid false positives)
    # class.weights=1, 1 (equal)
    
    print(mdl)
    
    data$pred <- predict(mdl, data, type="response")
    
    # EVALUATE MODEL
    cat("\n")
    conf <- with(data[train,], table(pred=pred, actual=bac_observed))
    print(conf)
    prec <- conf[1, 1] / sum(conf[1, ])
    cat("\nTrain Precision:",prec,"\n")
    recl <- conf[1, 1] / sum(conf[, 1])
    cat("Train Recall:",recl,"\n")
    f1 <- 2 * ((prec * recl) / (prec + recl))
    cat("Train F1-score:",f1,"\n\n")
    
    conf <- with(data[test,], table(pred=pred, actual=bac_observed))
    print(conf)
    prec <- conf[1, 1] / sum(conf[1, ])
    cat("\nTest Precision:",prec,"\n")
    recl <- conf[1, 1] / sum(conf[, 1])
    cat("Test Recall:",recl,"\n")
    f1 <- 2 * ((prec * recl) / (prec + recl))
    cat("Test F1-score:",f1,"\n")
    
    if (!is.null(toFile)) sink()
    
    #system("say Calculations complete.")
    system("afplay wtfboom.wav")
}

